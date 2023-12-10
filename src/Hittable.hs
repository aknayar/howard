module Hittable where

import Vec3
import Ray
import Data.Maybe (fromMaybe)
import Interval

data HitRecord = HitRecord { p :: Vec3, n :: Vec3, t :: Double, front_face :: Bool }

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal r outward_normal original =
  HitRecord (p original) new_normal (t original) new_front_face
    where
      new_front_face = direction r `dot` outward_normal < 0
      new_normal = if new_front_face then outward_normal else negateVec3 outward_normal

class Hittable a where
  hit :: Ray -> Interval -> Maybe HitRecord -> a -> Maybe HitRecord

newtype HittableList a = HittableList [a]

-- clearHittableList :: HittableList
-- clearHittableList = HittableList []

-- addHittableList :: HittableList -> a -> HittableList
-- addHittableList (HittableList l) h = HittableList (l : h)


instance Hittable a => Hittable (HittableList a) where
    hit ray range record (HittableList items) = record
        where
            reduce i (r, current_max) = fromMaybe (r, current_max) m
                where
                    m = do
                        record <- hit ray range record i
                        return (Just record, t record)
            record = fst $ foldr reduce (Nothing, t_max range) items

-- instance Hittable a => Hittable (HittableList a) where
--     hit r ray_tmin ray_tmax (HittableList objects) = record
--         where
--             reduce i (r', current_max) = fromMaybe (r, current_max) m
--                 where
--                     m = do
--                         record <- hit r ray_tmin current_max i
--                         return (Just record, t record)
--             record = fst $ foldr reduce (Nothing, ray_tmax) objects
