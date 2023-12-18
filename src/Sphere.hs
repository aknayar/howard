{-# LANGUAGE ExistentialQuantification #-}

module Sphere(
    Sphere (Sphere)
) where

import Vec3
import Ray
import Hittable
import Interval
data Sphere = forall a. Material a => Sphere Vec3 Double a

instance Hittable Sphere where
    hit r range _ (Sphere cent rad mat) =
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
