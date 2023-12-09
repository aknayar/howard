module Vec3 where

data Vec3 = Vec3
    {
        x :: Double,
        y :: Double,
        z :: Double
    } deriving (Show)

negateVec3 :: Vec3 -> Vec3
negateVec3 (Vec3 x y z) = Vec3 (-x) (-y) (-z)

addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) = Vec3 (u1 + v1) (u2 + v2) (u3 + v3)

multiplyVec3 :: Vec3 -> Double -> Vec3
multiplyVec3 (Vec3 x y z) t = Vec3 (x * t) (y * t) (z * t)

minusVec3 :: Vec3 -> Vec3 -> Vec3
minusVec3 (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) = Vec3 (u1 - v1) (u2 - v2) (u3 - v3)

divideVec3 :: Vec3 -> Double -> Vec3
divideVec3 v t = multiplyVec3 v (1 / t)

lengthSquaredVec3 :: Vec3 -> Double
lengthSquaredVec3 (Vec3 x y z) = x * x + y * y + z * z

lengthVec3 :: Vec3 -> Double
lengthVec3 v = sqrt (lengthSquaredVec3 v)

unitVector :: Vec3 -> Vec3
unitVector v = v `divideVec3` lengthVec3 v

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) = (u1 * v1) + (u2 * v2) + (u3 * v3)