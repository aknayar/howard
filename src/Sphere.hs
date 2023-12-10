module Sphere where

import Vec3
import Ray
import Hittable

data Sphere = Sphere { center :: Vec3, radius :: Double}
instance Hittable Sphere where
    hit r ray_tmin ray_tmax record s = res
        let
            oc = origin r `minusVec3` (center s)
            a = direction r `dot` direction r
            half_b = oc `dot` direction r
            c = (oc `dot` oc) - (radius s * radius s)
            discriminant = half_b*half_b - a*c

        if (discriminant < 0.0)
            then do return False
            else return ()
        let
            sqrtd = sqrt discriminant
            root = (-half_b - sqrtd) / a
        
        if (root <= ray_tmin || ray_tmax <= root)
            then do
                let root = (-half_b + sqrtd) / a
                if (root <= ray_tmin || ray_tmax <= root)
                    then do return False
                    else return ()
            else return ()
        -- (t record) <- root
        -- (p record) <- at r (t record)
        -- (n record) <- (p record - center) / (radius s) 

        return True

hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center radius ray =
                            let
                                oc = origin ray `minusVec3` center
                                a = direction ray `dot` direction ray
                                b = 2.0 * (oc `dot` direction ray)
                                c = (oc `dot` oc) - (radius * radius)
                                discriminant = b*b - 4*a*c
                            in
                                if discriminant < 0
                                    then -1.0
                                    else (-b - sqrt discriminant) / (2.0 * a)


