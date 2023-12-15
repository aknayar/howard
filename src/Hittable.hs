{-# LANGUAGE ExistentialQuantification #-}

module Hittable where

import Vec3
import Ray
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Data.List (sortBy, reverse)
import Data.Function (on)
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
                scatter_direction = if (nearZero rand) then (n record) else ((n record) `addVec3` rand)

data Metal = Metal Vec3 Double
instance Material Metal where
    scatter ray record g (Metal albedo fuzz) = 
        if ((direction scattered) `dot` (n record) > 0) then (Just (scattered, albedo), g1) else (Nothing, g1)
            where
                reflected = reflect (unitVector (direction ray)) (n record)
                (rand, g1) = randomUnitVector g
                scattered = (Ray (p record) (reflected `addVec3` (rand `multiplyVec3` fuzz)))

data HitRecord = forall a. Material a => HitRecord { p :: Vec3, n :: Vec3, mat :: a, t :: Double, front_face :: Bool }

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal r outward_normal (HitRecord pOriginal _ matOriginal tOriginal _) =
  HitRecord pOriginal new_normal matOriginal tOriginal new_front_face
    where
      new_front_face = direction r `dot` outward_normal < 0
      new_normal = if new_front_face then outward_normal else negateVec3 outward_normal

class Hittable a where
  hit :: Ray -> Interval -> Maybe HitRecord -> a -> Maybe HitRecord

newtype HittableList a = HittableList [a]

instance Hittable a => Hittable (HittableList a) where
    hit ray range record (HittableList items) = new_record
        where
            hits =  (catMaybes (map (hit ray range record) items))
            res = (t_max range, Nothing :: (Maybe HitRecord)) : (map (\a -> (t a, Just a) :: (Double, Maybe HitRecord)) hits)
            sort_res = sortBy (flip compare `on` fst) res
            new_record = (snd (head (reverse sort_res)))