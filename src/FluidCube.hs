module FluidCube where

import qualified Data.Array.Repa as R

data FluidCube = FC {
                      size      :: Int,
                      timestep  :: Int,
                      diffusion :: Int,
                      viscosity :: Int,

                      density   :: R.Array R.U R.DIM3 Double,

                      velocityX :: R.Array R.U R.DIM3 Double,
                      velocityY :: R.Array R.U R.DIM3 Double,
                      velocityZ :: R.Array R.U R.DIM3 Double
                     }
                  deriving Show


make :: Int -> Int -> Int -> Int -> [Double] -> FluidCube
make s ts diff visc arr = FC {
                              size = s,
                              timestep = ts,
                              diffusion = diff,
                              viscosity = visc,

                              density = zeros3D,

                              velocityX = zeros3D,
                              velocityY = zeros3D,
                              velocityZ = zeros3D
                         } where
                              zeros3D = R.fromListUnboxed shape zeros
                              shape = R.Z R.:. s R.:. s R.:. s
                              zeros = arr


makeZeros :: Int -> Int -> Int -> Int -> FluidCube
makeZeros s ts diff visc = make s ts diff visc (replicate (s^3) 0)


makeRand :: Int -> Int -> Int -> Int -> FluidCube
makeRand s ts diff visc = make s ts diff visc ([1 .. ((fromIntegral s)^^3)] :: [Double])

testFc = makeRand 3 1 1 1
