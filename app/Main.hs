module Main (main) where

import Lib

data Vec2I = Vec2I 
  {
    xI :: Int,
    yI :: Int
  }

data  Vec2D = Vec2D 
  {
    xD :: Double, 
    yD :: Double
  }

data Ray = Ray
  { rayOrigin :: Vec2I  -- Assuming you have a Point3D data type
  , rayDirection :: Vec2D    -- Assuming you have a Vec3 data type
  , rayPosition :: Vec2I
  }

main :: IO ()
main = do
  putStrLn "hello world"



