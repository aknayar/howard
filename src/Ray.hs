module Ray(
    Ray (Ray),
    origin,
    direction,
    at
) where

import Vec3

data Ray = Ray
    {
        origin :: Vec3,
        direction :: Vec3
    } deriving (Show)

at :: Ray -> Double -> Vec3
at (Ray org dir) t = org `addVec3` (dir `multiplyVec3` t)