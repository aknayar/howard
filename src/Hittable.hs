{-# LANGUAGE ExistentialQuantification #-}

module Hittable where

import Vec3
import Ray
import Data.Maybe (fromMaybe)
import Interval
import Utilities
import System.Random

class Material a where
    scatter :: Ray -> HitRecord -> StdGen -> a ->  (Maybe (Ray, Vec3), StdGen)

data Lambertian = Lambertian Vec3
instance Material Lambertian where
    scatter ray record g (Lambertian albedo) = 
        (Just (Ray (p record) scatter_direction, albedo), g1)
            where
                (rand, g1) = randomUnitVector g
                new_scatter = (n record) `addVec3` rand
                scatter_direction = if (nearZero new_scatter) then (n record) else (new_scatter)

data Metal = Metal Vec3 Double
instance Material Metal where
    scatter ray record g (Metal albedo fuzz) = 
        if ((direction scattered) `dot` (n record) > 0) then (Just (scattered, albedo), g1) else (Nothing, g1)
            where
                reflected = reflect (unitVector (direction ray)) (n record)
                (rand, g1) = randomUnitVector g
                scattered = (Ray (p record) (reflected `addVec3` (rand `multiplyVec3` fuzz)))

data Dielectric = Dielectric Double
instance Material Dielectric where
    scatter ray record g (Dielectric ir) =
        (Just (scattered, color), g)
            where
                color = Vec3 1.0 1.0 1.0
                refractionRatio = if front_face record then 1.0 / ir else ir

                unitDirection = unitVector (direction ray)

                cosTheta = min (negateVec3 unitDirection `dot` n record) 1.0
                sinTheta = sqrt (1.0 - cosTheta * cosTheta)

                cannotRefract = refractionRatio * sinTheta > 1.0

                scattered = if cannotRefract then Ray (p record) (reflect unitDirection (n record)) else Ray (p record) (refract unitDirection (n record) refractionRatio)



data HitRecord = forall a. Material a => HitRecord { p :: Vec3, n :: Vec3, mat :: a, t :: Double, front_face :: Bool }

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal r outward_normal (HitRecord pOriginal _ matOriginal tOriginal _) =
  HitRecord pOriginal new_normal matOriginal tOriginal new_front_face
    where
      new_front_face = direction r `dot` outward_normal < 0
      new_normal = if new_front_face then outward_normal else negateVec3 outward_normal

data Hittable = forall a. Material a => Sphere  Vec3 Double a | forall a. Material a => Triangle Vec3 Vec3 Vec3 a

hitSphere r range record (Sphere cent rad mat) =
    let oc = origin r `minusVec3` cent
        a = lengthSquaredVec3 (direction r)
        half_b = oc `dot` (direction r)
        c = lengthSquaredVec3 oc - (rad * rad)

        discriminant = half_b * half_b - a * c

        checkRoot :: Double -> Bool
        checkRoot root = surrounds root range

        updateHitRecord :: Double -> HitRecord -> HitRecord
        updateHitRecord root (HitRecord _ _ mat2 _ f) =
            setFaceNormal r outward_normal (HitRecord hit_point outward_normal mat2 root f)
                where
                    hit_point = at r root
                    outward_normal = (hit_point `minusVec3` cent) `divideVec3` rad

    in if discriminant < 0
        then Nothing
        else
            let sqrtd = sqrt discriminant
                root1 = (-half_b - sqrtd) / a
                root2 = (-half_b + sqrtd) / a

                validRoot1 = checkRoot root1
                validRoot2 = checkRoot root2

            in case (validRoot1, validRoot2) of
                (True, _) -> Just $ updateHitRecord root1 (HitRecord (Vec3 0 0 0) (Vec3 0 0 0) mat 0 True)
                (_, True) -> Just $ updateHitRecord root2 (HitRecord (Vec3 0 0 0) (Vec3 0 0 0) mat 0 True)
                _ -> Nothing
    

hitTriangle r range record (Triangle a@(Vec3 a1 a2 a3) b@(Vec3 b1 b2 b3) c@(Vec3 c1 c2 c3) mat) =
    let n1 = ((b `minusVec3` a) `cross` (c `minusVec3` a))
        q = ((origin r `minusVec3` a) `cross` (direction r))
        denom = direction r `dot` n1
        
        
        updateHitRecord :: Double -> Vec3 -> HitRecord -> HitRecord
        updateHitRecord t p1 (HitRecord _ _ mat2 _ f) =
            setFaceNormal r n1 (HitRecord p1 n1 mat2 t f)

    in if denom == 0
        then Nothing
        else
            let d = 1.0 / denom
                u = d * ((negateVec3 q) `dot` (c `minusVec3` a))
                v = d * (q `dot` (b `minusVec3` a))
                t = d * ((negateVec3 n1) `dot` (origin r `minusVec3` a))
            in if t < 0 || u < 0 || v < 0 || u + v > 1 then
                Nothing
            else Just $ updateHitRecord t (at r t) (HitRecord (Vec3 0 0 0) (Vec3 0 0 0) mat 0 True)

hit :: Ray -> Interval -> Maybe HitRecord -> [Hittable] -> Maybe HitRecord
hit ray range record items = hitHelper ray range record items
    where
        hitHelper _ _ record [] = record
        hitHelper ray range record (x:xs) =
            case result of
                        Nothing -> hitHelper ray range record xs
                        Just valid -> hitHelper ray (Interval (t_min range) (t valid)) (Just valid) xs
                where
                    result = case x of
                        Sphere cent rad mat -> hitSphere ray range record (Sphere cent rad mat)
                        Triangle a b c mat -> hitTriangle ray range record (Triangle a b c mat)