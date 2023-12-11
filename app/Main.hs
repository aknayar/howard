module Main (main) where

import Lib
import Vec3
import Color
import Ray
import Sphere
import Hittable
import Interval
import Camera


main :: IO ()
main = do
    let
        world = HittableList [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5) (-1)) 100]

        cam = initialize (16.0/9.0) 400 100

    render cam world

