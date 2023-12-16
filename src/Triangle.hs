{-# LANGUAGE ExistentialQuantification #-}

module Triangle where

import Vec3
import Ray
import Hittable
import Interval

data Triangle = forall a. Material a => Triangle { a :: Vec3, b :: Vec3, c :: Vec3, material :: a}

instance Hittable Triangle where
    hit r range record (Triangle a@(Vec3 a1 a2 a3) b@(Vec3 b1 b2 b3) c@(Vec3 c1 c2 c3) mat) =
        let n1 = unitVector ((b `minusVec3` a) `cross` (c `minusVec3` a))
            denom = n1 `dot` (direction r)
            d = (-(a `dot` n1))
            
            updateHitRecord :: Double -> Vec3 -> HitRecord -> HitRecord
            updateHitRecord t p1 (HitRecord _ _ mat2 _ f) =
                setFaceNormal r n1 (HitRecord p1 n1 mat2 t f)

        in if denom == 0
            then Nothing
            else
                let t = (-((n1 `dot` origin r) + d) / denom)
                in if t < 0 then
                    Nothing
                else
                    let p1 = at r t
                        pa = (cross (b `minusVec3` a) (p1 `minusVec3` a)) `dot` n1
                        pb = (cross (c `minusVec3` b) (p1 `minusVec3` b)) `dot` n1
                        pc = (cross (a `minusVec3` c) (p1 `minusVec3` c)) `dot` n1

                    in if (pa < 0 && pb < 0 && pc < 0)
                        then Nothing
                        else Just $ updateHitRecord t p1 (HitRecord (Vec3 0 0 0) (Vec3 0 0 0) mat 0 True)
