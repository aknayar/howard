module Camera where

import Vec3
import Ray
import Hittable
import Interval
import Color
import Utilities
data Camera = Camera 
    {
        aspectRatio :: Double,
        imageWidth :: Int,
        imageHeight:: Int,
        samplesPerPixel :: Int,
        center :: Vec3,
        pixel100Loc :: Vec3,
        pixelDeltaU :: Vec3,
        pixelDeltaV :: Vec3
    } deriving Show

initialize :: Double -> Int -> Int -> Camera
initialize aspect width samples = Camera aspect width height samples cent pixel100 deltaU deltaV
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
                        let r = getRay cam i j
                            pixelColor = updateColor (samplesPerPixel cam) (Vec3 0 0 0) r cam world
                        writeColor pixelColor (samplesPerPixel cam)
                    ) [0..imageWidth cam -1]) [0..imageHeight cam-1]

updateColor :: Hittable a => Int -> Vec3 -> Ray -> Camera -> a -> Vec3
updateColor 0 x _ _ _  = x
updateColor samples cur r cam world = updateColor (samples - 1) next r cam world
                                            where
                                                next = cur `addVec3` rayColor r world
    
getRay :: Camera -> Int -> Int -> Ray
getRay cam i j = Ray org dir
                where
                    pixelCenter = pixel100Loc cam + (pixelDeltaU cam `multiplyVec3` fromIntegral i) + (pixelDeltaV cam `multiplyVec3` fromIntegral j)
                    pixelSample = pixelCenter + pixelSampleSquare cam

                    org = center cam
                    dir = pixelSample `minusVec3` org

pixelSampleSquare :: Camera -> Vec3
pixelSampleSquare cam = res
                    where
                        px = -0.5 + randomDouble
                        py = -0.5 + randomDouble
                        res = (pixelDeltaU cam `multiplyVec3` fst px) + (pixelDeltaV cam `multiplyVec3` fst py)
