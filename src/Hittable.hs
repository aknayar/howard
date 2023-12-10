module Hittable where

import Vec3
import Ray
data HitRecord = HitRecord { p :: Vec3, n :: Vec3, t :: Double }

class Hittable a where
  hit :: Ray -> Double -> Double -> HitRecord -> a -> Maybe HitRecord
  