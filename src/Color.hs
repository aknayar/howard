module Color where

import Vec3

writeColor :: Vec3 -> IO()
writeColor (Vec3 r g b)= do
                let ir = round $ 255.999 * r
                    ig = round $ 255.999 * g
                    ib = round $ 255.999 * b

                putStrLn $ show ir ++ " " ++ show ig ++ " " ++ show ib