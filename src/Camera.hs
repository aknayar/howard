module Camera where

import Vec3
import Ray
import Hittable
import Interval
import Color
import Utilities
import System.Random (mkStdGen, StdGen)
data Camera = Camera 
    {
        aspectRatio :: Double,
        imageWidth :: Int,
        imageHeight:: Int,
        samplesPerPixel :: Int,
        center :: Vec3,
        pixel100Loc :: Vec3,
        pixelDeltaU :: Vec3,
        pixelDeltaV :: Vec3,
        maxDepth :: Int
    } deriving Show


initialize :: Double -> Int -> Int -> Camera
initialize aspect width samples = Camera aspect width height samples cent pixel100 deltaU deltaV 50
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
                                
rayColor :: Hittable a => Ray -> a -> Int -> StdGen -> (Vec3, StdGen)
rayColor _ _ 0 g = (Vec3 0 0 0, g)
rayColor (Ray org dir) world i g = ret
                where
                    tempRecord = HitRecord (Vec3 0 0 0 ) (Vec3 0 0 0 ) (Material a) 0 False
                    isHit = hit (Ray org dir) (Interval 0.001 9999999999999) (Just tempRecord) world
                    unit_direction = unitVector dir
                    a = (y unit_direction + 1.0) * 0.5
                    ret = case isHit of
                        Nothing -> ((Vec3 1.0 1.0 1.0 `multiplyVec3` (1.0 - a)) `addVec3` (Vec3 0.5 0.7 1.0 `multiplyVec3` a), g)
                        Just yesHit -> (v `multiplyVec3` 0.5, g2)
                            where
                                (direction, g1) = randomOnHemisphere g (n yesHit)
                                (v, g2) = (rayColor (Ray (p yesHit) (direction `addVec3` n yesHit)) world (i - 1) g1)

render :: Hittable a => Camera -> a -> IO()
render cam world = do
                putStrLn $ "P3\n" ++ show (imageWidth cam) ++ " " ++ show (imageHeight cam) ++ "\n255"
                mapM_ (\j -> mapM_ (\i -> do
                        let (pixelColor, _) = updateColor (samplesPerPixel cam) (Vec3 0 0 0) i j cam world (mkStdGen (i * (imageHeight cam - 1) + j))
                        writeColor pixelColor (samplesPerPixel cam)
                    ) [0..imageWidth cam -1]) [0..imageHeight cam-1]

updateColor :: Hittable a => Int -> Vec3 -> Int -> Int -> Camera -> a -> StdGen -> (Vec3, StdGen)
updateColor 0 x _ _ _ _ g = (x, g)
updateColor samples cur i j cam world g = updateColor (samples - 1) next i j cam world g1
                                            where
                                                r = getRay cam i j (mkStdGen (i * samples * (imageHeight cam-1) + j))
                                                (rc, g1) = rayColor r world (maxDepth cam) g
                                                next = cur `addVec3` rc
    
getRay :: Camera -> Int -> Int -> StdGen -> Ray
getRay cam i j g = Ray org dir
                where
                    pixelCenter = pixel100Loc cam `addVec3` (pixelDeltaU cam `multiplyVec3` fromIntegral i) `addVec3` (pixelDeltaV cam `multiplyVec3` fromIntegral j)
                    (pss, g2) = pixelSampleSquare cam g
                    pixelSample = pixelCenter `addVec3` pss

                    org = center cam
                    dir = pixelSample `minusVec3` org

pixelSampleSquare :: Camera -> StdGen -> (Vec3, StdGen)
pixelSampleSquare cam g = res
                    where
                        px = -0.5 + d
                        py = -0.5 + d1
                        (d, g1) = randomDouble g
                        (d1,g2) = randomDouble g1
                        res = ((pixelDeltaU cam `multiplyVec3` px) `addVec3` (pixelDeltaV cam `multiplyVec3` py), g2)
