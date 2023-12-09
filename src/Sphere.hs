module Sphere where

import Vec3
import Ray

hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center radius ray = discriminant
                                    where
                                        oc = origin ray `minusVec3` center
                                        a = direction ray `dot` direction ray
                                        b = 2.0 * (oc `dot` direction ray)
                                        c = (oc `dot` oc) - (radius * radius)
                                        discriminant = b*b - 4*a*c

