module Utilities where
import System.Random
import Vec3

degreesToRadians :: Double -> Double
degreesToRadians deg = deg * pi / 180.0

randomDouble :: StdGen -> (Double, StdGen)
randomDouble = randomR (0.0, 1.0)

randomDoubleR :: Double -> Double -> StdGen -> (Double, StdGen)
randomDoubleR rand_min rand_max gen =
    let (randValue, newGen) = randomDouble gen
    in (rand_min + (rand_max - rand_min) * randValue, newGen)

randomVec3 :: StdGen -> (Vec3, StdGen)
randomVec3 = randomVec3R 0.0 1.0

randomVec3R :: Double -> Double -> StdGen -> (Vec3, StdGen)
randomVec3R rand_min rand_max g =
    ((Vec3 x y z), g3)
        where
            (x, g1) = randomDoubleR rand_min rand_max g
            (y, g2) = randomDoubleR rand_min rand_max g2
            (z, g3) = randomDoubleR rand_min rand_max g3

-- randomInUnitSphere :: StdGen Vec3
