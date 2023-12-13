module Main (main) where

import Vec3
import Sphere
import Hittable
import Camera


main :: IO ()
main = do
    let
        material_ground = Lambertian (Vec3 0.8 0.8 0)
        material_center = Lambertian (Vec3 0.1 0.2 0.5)
        material_left = Dielectric 1.5
        material_right = Metal (Vec3 0.8 0.6 0.2) 0
        world = HittableList [Sphere (Vec3 0 0 (-1)) 0.5 material_center, Sphere (Vec3 (-1) 0 (-1)) 0.5 material_left, Sphere (Vec3 1 0 (-1)) 0.5 material_right, Sphere (Vec3 0 (-100.5) (-1)) 100 material_ground]

        cam = initialize (16.0/9.0) 400 100

    render cam world

