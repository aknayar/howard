module Camera where

import Vec3
import Ray
import Hittable
import Interval
import Color
import Utilities
import System.Random (mkStdGen, StdGen)
import Control.Parallel.Strategies
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


initialize :: Double -> Int -> Int -> Double -> Vec3 -> Vec3 -> Vec3 -> Camera
initialize aspect width samples vFov lookFrom lookAt vUp = Camera aspect width height samples cent pixel100 deltaU deltaV 50
                            where
                                height = max 1 (floor $ fromIntegral width / aspect)

                                cent = lookFrom
                                focalLength = lengthVec3 (lookFrom `minusVec3` lookAt)

                                w = unitVector (lookFrom `minusVec3` lookAt)
                                u = unitVector (cross vUp w)
                                v = cross w u
                                
                                theta = degreesToRadians vFov
                                h = tan (theta / 2.0)
                                viewPortHeight = 2.0 * h * focalLength
                                viewPortWidth = viewPortHeight * (fromIntegral width / fromIntegral height)

                                viewPortU = u `multiplyVec3` viewPortWidth
                                viewPortV = (negateVec3 v) `multiplyVec3` viewPortHeight

                                deltaU = viewPortU `divideVec3` fromIntegral width
                                deltaV = viewPortV `divideVec3` fromIntegral height

                                viewPortUpperLeft = cent `minusVec3` (w `multiplyVec3` focalLength) `minusVec3` (viewPortU `divideVec3` 2) `minusVec3` (viewPortV `divideVec3` 2)
                                pixel100 = viewPortUpperLeft `addVec3` ((deltaU `addVec3` deltaV) `multiplyVec3` (0.5 :: Double))
                                
rayColor :: Hittable a => Ray -> a -> Int -> StdGen -> (Vec3, StdGen)
rayColor _ _ 0 g = (Vec3 0 0 0, g)
rayColor (Ray org dir) world i g = ret
                where
                    isHit = hit (Ray org dir) (Interval 0.001 9999999999999) Nothing world
                    unit_direction = unitVector dir
                    a = (y unit_direction + 1.0) * 0.5
                    ret = case isHit of
                        Nothing -> ((Vec3 1.0 1.0 1.0 `multiplyVec3` (1.0 - a)) `addVec3` (Vec3 0.5 0.7 1.0 `multiplyVec3` a), g)
                        Just (HitRecord p2 n2 m t2 ff) -> 
                            case scatter (Ray org dir) (HitRecord p2 n2 m t2 ff) g m of
                                (Nothing, g1) -> (Vec3 0 0 0, g1)
                                (Just (scattered, attenuation), g1) -> (attenuation `multiplyVec3` v, g2)
                                    where
                                        (v, g2) = (rayColor scattered world (i - 1) g1)

renderChunk :: Hittable a => Camera -> a -> Int -> Int -> Int -> String -> String
renderChunk cam world i j end cur
        | j == end = cur
        | i < (imageWidth cam - 1) = renderChunk cam world (i + 1) j end (cur ++ res)
        | otherwise = renderChunk cam world 0 (j + 1) end (cur ++ res)
            where
                (pixelColor, _) = updateColor (samplesPerPixel cam) (Vec3 0 0 0) i j cam world (mkStdGen (i * (imageHeight cam - 1) + j))
                res = writeColorStr pixelColor (samplesPerPixel cam)

renderParallel :: Hittable a => Camera -> a -> Int -> Int -> Int -> String -> String
renderParallel cam world start end chunksize cur
        | start == end = cur
        | otherwise = runEval $ do
                    curChunk <- rpar $ renderChunk cam world 0 start (min end (start + chunksize)) ""
                    nextChunk <- rpar $ renderParallel cam world (min (start + chunksize) end) end chunksize ""
                    rseq curChunk
                    rseq nextChunk
                    return (curChunk ++ nextChunk)
                    

render :: Hittable a => Camera -> a -> IO()
render cam world = do
                putStrLn $ "P3\n" ++ show (imageWidth cam) ++ " " ++ show (imageHeight cam) ++ "\n255"
                mapM_ (\j -> mapM_ (\i -> do
                        let (pixelColor, _) = updateColor (samplesPerPixel cam) (Vec3 0 0 0) i j cam world (mkStdGen (i * (imageHeight cam - 1) + j))
                        writeColor pixelColor (samplesPerPixel cam)
                    ) [0..imageWidth cam -1]) [0..imageHeight cam-1]

updateColor :: Hittable a => Int -> Vec3 -> Int -> Int -> Camera -> a -> StdGen -> (Vec3, StdGen)
updateColor 0 x _ _ _ _ g = (x, g)
updateColor samples cur i j cam world g = updateColor (samples - 1) next i j cam world g2
                                            where
                                                (r, g1) = getRay cam i j g
                                                (rc, g2) = rayColor r world (maxDepth cam) g1
                                                next = cur `addVec3` rc
    
getRay :: Camera -> Int -> Int -> StdGen -> (Ray, StdGen)
getRay cam i j g = (Ray org dir, g1)
                where
                    pixelCenter = pixel100Loc cam `addVec3` (pixelDeltaU cam `multiplyVec3` (fromIntegral i :: Double)) `addVec3` (pixelDeltaV cam `multiplyVec3` (fromIntegral j :: Double))
                    (pss, g1) = pixelSampleSquare cam g
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
