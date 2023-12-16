{-# LANGUAGE ExistentialQuantification #-}

module Triangle where

import Vec3
import Ray
import Hittable
import Interval

data Triangle = forall a. Material a => Triangle { a :: Vec3, b :: Vec3, c :: Vec3, material :: a}

instance Hittable Triangle where
    hit r range record (Triangle a@(Vec3 a1 a2 a3) b@(Vec3 b1 b2 b3) c@(Vec3 c1 c2 c3) mat) =
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
