module Interval(
    Interval (Interval),
    contains,
    surrounds,
    clamp,
    t_min,
    t_max,
) where

data Interval = Interval 
    {
        t_min :: Double,
        t_max :: Double
    }

contains :: Double -> Interval -> Bool
contains x (Interval t_min1 t_max1) = x >= t_min1 && x <= t_max1

surrounds :: Double -> Interval -> Bool
surrounds x (Interval t_min1 t_max1) = t_min1 < x && x < t_max1

clamp :: Interval -> Double -> Double
clamp (Interval rMin rMax) val
                | val < rMin = rMin
                | val > rMax = rMax
                | otherwise = val
