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

        groundSphere = Sphere (Vec3 0 (-100.5) (-1.0)) 100.0 material_ground
        centerSphere = Sphere (Vec3 0 0 (-1.0)) 0.5 material_center
        leftSphere = Sphere (Vec3 (-1) 0 (-1.0)) 0.5 material_left
        rightSphere = Sphere (Vec3 1 0 (-1.0)) 0.5 material_right

        world = HittableList [leftSphere, centerSphere, rightSphere, groundSphere]

        vFov = 20
        lookFrom = Vec3 (-2) 2 1
        lookAt = Vec3 0 0 (-1)
        vUp = Vec3 0 1 0
        cam = initialize (16.0/9.0) 400 100 vFov lookFrom lookAt vUp

    render cam world

