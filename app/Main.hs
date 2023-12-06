module Main (main) where

import Lib

data Vec3 = Vec3 { x :: Double, y :: Double, z :: Double } deriving (Show)

main :: IO ()
main = do
  putStrLn "hello world"



