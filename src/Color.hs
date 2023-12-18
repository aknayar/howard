module Color where

import Vec3
import Interval

linearToGamma :: Double -> Double
linearToGamma linearCompart = sqrt linearCompart

writeColor :: Vec3 -> Int -> IO()
writeColor (Vec3 r g b) samples = do

                let scale = 1.0 / fromIntegral samples
                    rScaled = r * scale
                    gScaled = g * scale
                    bScaled = b * scale

                    rGamma = linearToGamma rScaled
                    gGamma = linearToGamma gScaled
                    bGamma = linearToGamma bScaled

                    range = Interval 0.000 0.999
                    ir = 256 * clamp range rGamma
                    ig = 256 * clamp range gGamma
                    ib = 256 * clamp range bGamma
                
                putStrLn $ show ir ++ " " ++ show ig ++ " " ++ show ib

writeColorStr :: Vec3 -> Int -> String
writeColorStr (Vec3 r g b) samples = res
                where 
                    scale = 1.0 / fromIntegral samples
                    rScaled = r * scale
                    gScaled = g * scale
                    bScaled = b * scale

                    rGamma = linearToGamma rScaled
                    gGamma = linearToGamma gScaled
                    bGamma = linearToGamma bScaled

                    range = Interval 0.000 0.999
                    ir = 256 * clamp range rGamma
                    ig = 256 * clamp range gGamma
                    ib = 256 * clamp range bGamma
                    res  = show ir ++ " " ++ show ig ++ " " ++ show ib