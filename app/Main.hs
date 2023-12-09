module Main (main) where

import Lib
import Vec3
import Color
import Ray
import Sphere
import Vec3 (minusVec3)


main :: IO ()
main = do

    let aspect_ratio = 16.0 / 9.0
        image_width = 400

        image_height = max 1 (floor $ fromIntegral image_width / aspect_ratio)
        
        focal_length = 1.0
        view_port_height = 2.0
        view_port_width = view_port_height * (fromIntegral image_width / fromIntegral image_height)
        camera_center = Vec3 0 0 0

    let view_port_u = Vec3 view_port_width 0 0
        view_port_v = Vec3 0 (-view_port_height) 0

        pixel_delta_u = view_port_u `divideVec3`  fromIntegral image_width
        pixel_delta_v = view_port_v `divideVec3`  fromIntegral image_height
    
    let view_port_upper_left = camera_center `minusVec3` Vec3 0 0 focal_length `minusVec3` (view_port_u `divideVec3` 2) `minusVec3` (view_port_v `divideVec3` 2)
        pixel100_loc = view_port_upper_left `addVec3` ((pixel_delta_u `addVec3` pixel_delta_v) `multiplyVec3` 0.5)

    putStrLn $ "P3\n" ++ show image_width ++ " " ++ show image_height ++ "\n255"

    mapM_ (\j -> mapM_ (\i -> do
            let pixel_center = pixel100_loc `addVec3` (pixel_delta_u `multiplyVec3` fromIntegral i) `addVec3` (pixel_delta_v `multiplyVec3` fromIntegral j)
                ray_direction = pixel_center `minusVec3` camera_center

                r = Ray camera_center ray_direction
                pixel_color = rayColor r

            writeColor pixel_color
        ) [0..image_width-1]) [0..image_height-1]

rayColor :: Ray -> Vec3
rayColor (Ray org dir)
                    | t > 0.0 = hit
                    | otherwise = ret
                                where
                                    t = hitSphere (Vec3 0 0 (-1)) 0.5 (Ray org dir)
                                    n = unitVector (at (Ray org dir) t `minusVec3` Vec3 0 0 (-1))
                                    hit = Vec3 (x n + 1) (y n + 1) (z n + 1) `multiplyVec3` 0.5
                                    unit_direction = unitVector dir
                                    a = (y unit_direction + 1.0) * 0.5
                                    ret = (Vec3 1.0 1.0 1.0 `multiplyVec3` (1.0 - a)) `addVec3` (Vec3 0.5 0.7 1.0 `multiplyVec3` a)
