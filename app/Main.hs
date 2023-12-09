module Main (main) where

import Lib
import Vec3
import Color

main :: IO ()
main = do
    -- Image
    let image_width = 256
        image_height = 256

    -- Render
    putStrLn $ "P3\n" ++ show image_width ++ " " ++ show image_height ++ "\n255"

    mapM_ (\j -> mapM_ (\i -> do
            let r =  i /  (image_width-1)
                g =  j / (image_height-1)
                b = 0 :: Double

            writeColor (Vec3 r g b)
        ) [0..image_width-1]) [0..image_height-1]






