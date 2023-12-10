module Hittable where

import Vec3
import Ray
data HitRecord = HitRecord { p :: Vec3, n :: Vec3, t :: Double, front_face :: Bool }

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal r outward_normal original =
  HitRecord (p original) new_normal (t original) new_front_face
    where
      new_front_face = (((direction r) `dot` outward_normal) < 0)
      new_normal = case new_front_face of
        True -> outward_normal
        False -> negateVec3 outward_normal

class Hittable a where
  hit :: Ray -> Double -> Double -> HitRecord -> a -> Maybe HitRecord
  