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
                    ir = 256 * clamp range r
                    ig = 256 * clamp range g
                    ib = 256 * clamp range b
                
                putStrLn $ show ir ++ " " ++ show ig ++ " " ++ show ib