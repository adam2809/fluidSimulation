module FluidSquare where

import qualified Data.Array.Repa as R

data FluidSquare = FS {
    size      :: Int,
    timestep  :: Int,
    diffusion :: Int,
    viscosity :: Int,

    density   :: R.Array R.U R.DIM2 Double,

    velocityX :: R.Array R.U R.DIM2 Double,
    velocityY :: R.Array R.U R.DIM2 Double
} deriving Show


make :: Int -> Int -> Int -> Int -> [Double] -> FluidSquare
make s ts diff visc xs = FS {
    size = s,
    timestep = ts,
    diffusion = diff,
    viscosity = visc,

    density = arr,

    velocityX = arr,
    velocityY = arr
} where
    arr = R.fromListUnboxed shape xs
    shape = (R.Z R.:. s R.:. s)


makeZeros :: Int -> Int -> Int -> Int -> FluidSquare
makeZeros s ts diff visc = make s ts diff visc (replicate (s^2) 0)


makeConsecutive :: Int -> Int -> Int -> Int -> FluidSquare
makeConsecutive s ts diff visc = make s ts diff visc ([1 .. ((fromIntegral s)^^2)] :: [Double])

testFs = makeConsecutive 3 1 1 1
