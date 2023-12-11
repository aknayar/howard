module Color where

import Vec3
import Interval

writeColor :: Vec3 -> Int -> IO()
writeColor (Vec3 r g b) samples = do

                let scale = 1.0 / fromIntegral samples
                    rScaled = r * scale
                    gScaled = g * scale
                    bScaled = b * scale

                    range = Interval 0.000 0.999
                    ir = 256 * clamp range rScaled
                    ig = 256 * clamp range gScaled
                    ib = 256 * clamp range bScaled
                
                putStrLn $ show ir ++ " " ++ show ig ++ " " ++ show ib