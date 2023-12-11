module Camera where

import Vec3
import Ray
import Hittable
import Interval
import Color
data Camera = Camera 
    {
        aspectRatio :: Double,
        imageWidth :: Int,
        imageHeight:: Int,
        center :: Vec3,
        pixel100Loc :: Vec3,
        pixelDeltaU :: Vec3,
        pixelDeltaV :: Vec3
    } deriving Show

initialize :: Double -> Int -> Camera
initialize aspect width = Camera aspect width height cent pixel100 deltaU deltaV
                            where
                                height = max 1 (floor $ fromIntegral width / aspect)
                                cent = Vec3 0 0 0
                                focalLength = 1.0
                                viewPortHeight = 2.0
                                viewPortWidth = viewPortHeight * (fromIntegral width / fromIntegral height)

                                viewPortU = Vec3 viewPortWidth 0 0
                                viewPortV = Vec3 0 (-viewPortHeight) 0

                                deltaU = viewPortU `divideVec3` fromIntegral width
                                deltaV = viewPortV `divideVec3` fromIntegral height

                                viewPortUpperLeft = cent `minusVec3` Vec3 0 0 focalLength `minusVec3` (viewPortU `divideVec3` 2) `minusVec3` (viewPortV `divideVec3` 2)
                                pixel100 = viewPortUpperLeft `addVec3` ((deltaU `addVec3` deltaV) `multiplyVec3` 0.5)
                                
rayColor :: Hittable a => Ray -> a -> Vec3
rayColor (Ray org dir) world = ret
                where
                    tempRecord = HitRecord (Vec3 0 0 0 ) (Vec3 0 0 0 ) 0 False
                    isHit = hit (Ray org dir) (Interval 0 9999999) (Just tempRecord) world
                    unit_direction = unitVector dir
                    a = (y unit_direction + 1.0) * 0.5
                    ret = case isHit of
                        Nothing -> (Vec3 1.0 1.0 1.0 `multiplyVec3` (1.0 - a)) `addVec3` (Vec3 0.5 0.7 1.0 `multiplyVec3` a)
                        Just yesHit -> (n yesHit `addVec3` Vec3 1 1 1) `multiplyVec3` 0.5

render :: Hittable a => Camera -> a -> IO()
render cam world = do
                putStrLn $ "P3\n" ++ show (imageWidth cam) ++ " " ++ show (imageHeight cam) ++ "\n255"

                mapM_ (\j -> mapM_ (\i -> do
                        let pixel_center = pixel100Loc cam `addVec3` (pixelDeltaU cam`multiplyVec3` fromIntegral i) `addVec3` (pixelDeltaV cam `multiplyVec3` fromIntegral j)
                            ray_direction = pixel_center `minusVec3` center cam

                            r = Ray (center cam) ray_direction
                            pixel_color = rayColor r world

                        writeColor pixel_color 
                    ) [0..imageWidth cam -1]) [0..imageHeight cam-1]
