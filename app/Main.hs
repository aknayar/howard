module Main (main) where

import Vec3
import Sphere
import Hittable
import Camera
import Utilities
import System.Random
import Data.Maybe (catMaybes)

randomLambert :: Vec3 -> StdGen -> (Sphere, StdGen)
randomLambert center g = (Sphere center 0.2 mat, g1)
    where
        (v, g1) = randomVec3 g
        mat = Lambertian v

randomMetal :: Vec3 -> StdGen -> (Sphere, StdGen)
randomMetal center g = (Sphere center 0.2 mat, g2)
    where
        (v, g1) = randomVec3 g
        (f, g2) = randomDouble g1
        mat = Metal v f

dielectric :: Vec3 -> Sphere
dielectric center = Sphere center 0.2 (Dielectric 1.5)

randomBall :: Int -> Int -> StdGen -> Maybe Sphere
randomBall a b g =
    if lengthVec3 (center `minusVec3` (Vec3 4 0.2 0)) < 0.9
            then Nothing
            else Just sphere
                where
                    (chooseMat, g1) = randomDouble g
                    (x', g2) = randomDouble g1
                    (y', g3) = randomDouble g2
                    center = Vec3 (fromIntegral a + 0.9 * x') 0.2 (fromIntegral b + 0.9 * y')
                    (l, g4) = randomLambert center g3
                    (m, _) = randomMetal center g4
                    sphere
                      | chooseMat < 0.8 = l
                      | chooseMat < 0.95 = m
                      | otherwise = dielectric center

main :: IO ()
main = do
    args <- getArgs
    let
        scene = if not (null args) then head args else ""
        material_ground = Lambertian (Vec3 0.5 0.5 0.5)
        objects = (catMaybes [randomBall a b (mkStdGen (21 * a + b)) | a <- [-11,-10..11], b <- [-11,-10..11]])

        material1 = Dielectric 1.5
        material2 = Lambertian (Vec3 0.4 0.2 0.1)
        material3 = Metal (Vec3 0.7 0.6 0.5) 0.0

        width = 720
        samples = 100

    case scene of
        "final" -> do
            let
                groundSphere = Sphere (Vec3 0 (-1000) 0) 1000 material_ground
                sphere3 = Sphere (Vec3 0 1 0) 1.0 material1
                sphere2 = Sphere (Vec3 (-4) 1 0) 1.0 material2
                sphere1 = Sphere (Vec3 4 1 0) 1.0 material3

                world = HittableList ([sphere1, sphere2, sphere3] ++ objects ++ [groundSphere])

                vFov = 20
                lookFrom = Vec3 13 2 3
                lookAt = Vec3 0 0 0
                vUp = Vec3 0 1 0
                cam = initialize (16.0/9.0) width samples vFov lookFrom lookAt vUp

            renderParallel cam world        
        "lambertian" -> do
            let

                groundSphere = Sphere (Vec3 0 (-100.5) 0) 100 material_ground
                sphere1 = Sphere (Vec3 0 0 (-1)) 0.5 material2

                world = HittableList [sphere1, groundSphere]

                vFov = 90
                lookFrom = Vec3 0 1 0
                lookAt = Vec3 0 0 (-1)
                vUp = Vec3 0 1 0
                cam = initialize (16.0/9.0) width samples vFov lookFrom lookAt vUp
            renderParallel cam world
        "metal" -> do
            let
                groundSphere = Sphere (Vec3 0 (-100.5) 0) 100 material_ground
                sphere1 = Sphere (Vec3 0 0 (-1)) 0.5 material3
                sphere2 =  Sphere (Vec3 1 0 (-1)) 0.5 material2
                world = HittableList [sphere1, sphere2, groundSphere]

                vFov = 90
                lookFrom = Vec3 0 1 0
                lookAt = Vec3 0 0 (-1)
                vUp = Vec3 0 1 0
                cam = initialize (16.0/9.0) width samples vFov lookFrom lookAt vUp
            renderParallel cam world
        "dielectric" -> do
            let
                groundSphere = Sphere (Vec3 0 (-100.5) 0) 100 material_ground
                sphere1 = Sphere (Vec3 0 0 (-1)) 0.5 material1
                sphere2 =  Sphere (Vec3 1 0 (-1)) 0.5 material2
                world = HittableList [sphere1, sphere2, groundSphere]

                vFov = 90
                lookFrom = Vec3 0 0 0
                lookAt = Vec3 0 0 (-1)
                vUp = Vec3 0 1 0
                cam = initialize (16.0/9.0) width samples vFov lookFrom lookAt vUp
            renderParallel cam world
        "hollow-sphere" -> do
            let
                groundSphere = Sphere (Vec3 0 (-100.5) 0) 100 material_ground
                sphere1 = Sphere (Vec3 0 0 (-1)) 0.5 material1
                sphere1Inner = Sphere (Vec3 0 0 (-1)) (-0.4) material1
                sphere2 =  Sphere (Vec3 1 0 (-1)) 0.5 material2
                world = HittableList [sphere1, sphere1Inner, sphere2, groundSphere]

                vFov = 90
                lookFrom = Vec3 0 0 0
                lookAt = Vec3 0 0 (-1)
                vUp = Vec3 0 1 0
                cam = initialize (16.0/9.0) width samples vFov lookFrom lookAt vUp
            renderParallel cam world
        _ -> do
            let
                groundSphere = Sphere (Vec3 0 (-1000) 0) 1000 material_ground
                sphere3 = Sphere (Vec3 0 1 0) 1.0 material1
                sphere2 = Sphere (Vec3 (-4) 1 0) 1.0 material2
                sphere1 = Sphere (Vec3 4 1 0) 1.0 material3

                world = HittableList [sphere1, sphere2, sphere3, groundSphere]

                vFov = 20
                lookFrom = Vec3 13 2 3
                lookAt = Vec3 0 0 0
                vUp = Vec3 0 1 0

                cam = initialize (16.0/9.0) width samples vFov lookFrom lookAt vUp

            renderParallel cam world
