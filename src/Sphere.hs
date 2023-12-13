{-# LANGUAGE ExistentialQuantification #-}

module Sphere where

import Vec3
import Ray
import Hittable
import Interval
data Sphere = forall a. Material a => Sphere { center :: Vec3, radius :: Double, material :: a}

instance Hittable Sphere where
    hit r range record (Sphere cent rad mat) =
        let oc = origin r `minusVec3` cent
            a = direction r `dot` direction r
            half_b = dot oc (direction r)
            c = (oc `dot` oc) - (rad * rad)
            discriminant = half_b * half_b - a * c

            checkRoot :: Double -> Bool
            checkRoot root = surrounds root range

            updateHitRecord :: Double -> HitRecord -> HitRecord
            updateHitRecord root (HitRecord p n mat t f) =
                let hit_point = origin r `addVec3` (direction  r`multiplyVec3` root)
                    hit_normal = (hit_point `minusVec3` cent) `divideVec3` rad
                    outward_normal = (p `minusVec3` cent) `divideVec3` rad
                    new_front_face = maybe False front_face record
                in setFaceNormal r outward_normal (HitRecord hit_point hit_normal mat root new_front_face)

        in if discriminant < 0
            then Nothing
            else
                let sqrtd = sqrt discriminant
                    root1 = (-half_b - sqrtd) / a
                    root2 = (-half_b + sqrtd) / a

                    validRoot1 = checkRoot root1
                    validRoot2 = checkRoot root2
                    new_front_face = maybe False front_face record

                    new_record = case record of
                        Nothing -> HitRecord (Vec3 0 0 0) (Vec3 0 0 0) mat 0 new_front_face
                        Just real_record -> real_record


                in case (validRoot1, validRoot2) of
                    (True, _) -> Just $ updateHitRecord root1 (HitRecord (p new_record) (n new_record) mat root1 (front_face new_record))
                    (_, True) -> Just $ updateHitRecord root2 (HitRecord (p new_record) (n new_record) mat root2 (front_face new_record))
                    _ -> Nothing
