module Interval where

data Interval = Interval 
    {
        t_min :: Double,
        t_max :: Double
    }

contains :: Double -> Interval -> Bool
contains x (Interval t_min t_max) = x >= t_min && x <= t_max

surrounds :: Double -> Interval -> Bool
surrounds x (Interval t_min t_max) = t_min < x && x < t_max

clamp :: Interval -> Double -> Double
clamp (Interval rMin rMax) val
                | val < rMin = rMin
                | val > rMax = rMax
                | otherwise = val
