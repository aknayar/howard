module Main (main) where

import Lib

main :: IO ()
main = do
    -- Image
    let image_width = 256
        image_height = 256

    -- Render
    putStrLn $ "P3\n" ++ show image_width ++ " " ++ show image_height ++ "\n255"

    mapM_ (\j -> mapM_ (\i -> do
            let r = fromIntegral i / fromIntegral (image_width-1)
                g = fromIntegral j / fromIntegral (image_height-1)
                b = 0 :: Double

            let ir = round $ 255.999 * r
                ig = round $ 255.999 * g
                ib = round $ 255.999 * b

            putStrLn $ show ir ++ " " ++ show ig ++ " " ++ show ib
        ) [0..image_width-1]) [0..image_height-1]






