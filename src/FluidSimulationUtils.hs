{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FluidSimulationUtils where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Shape (shapeOfList)
import FluidSquare
import Control.Monad (mapM)


stepDensity :: R.Array R.D R.DIM2 Double -> Int -> Double -> R.Array R.D R.DIM2 Double
stepDensity arr dt diff = diffuse arr dt diff

-- TODO The definition below is a placeholder
setXBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setXBound = setCorners . setBound


setYBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setYBound = setCorners . R.transpose . setBound . R.transpose


setDensBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setDensBound = setCorners . setXBound . setXBound


setBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setBound arr = R.traverse arr id (\src (R.Z R.:. x R.:. y) -> if
    | x == 0 -> negate $ src (R.Z R.:. 1 R.:. y)
    | x == n-1 -> negate $ src (R.Z R.:. (n-2) R.:. y)
    | y == 0 -> src (R.Z R.:. x R.:. 1)
    | y == n-1 -> src (R.Z R.:. x R.:. (n-2))
    | otherwise -> src (R.Z R.:. x R.:. y)) where
        (R.Z R.:. n R.:. _) = R.extent arr

setCorners :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setCorners arr = R.traverse arr id (\src (R.Z R.:. x R.:. y) -> if [x,y] `elem` cornerIndicies then
    let [x',y'] = map (\coord -> if coord == 0 then 1 else n-1) [x,y] in
        ((arr R.! (R.Z R.:. x' R.:. y)) + (arr R.! (R.Z R.:. x R.:. y'))) / 2
else
    arr R.! (R.Z R.:. x R.:. y)) where
        (R.Z R.:. n R.:. _) = R.extent arr
        cornerIndicies = mapM (const [0,n-1]) [1,2]


diffAccuracy = 20

-- TODO
-- The second parameter of the first argument should be r (with constraint that
--  r is the array type) instead of R.D but types don't match up otherwise so
-- leaving this to fix later
diffuse ::  R.Array R.D R.DIM2 Double -> Int -> Double -> R.Array R.D R.DIM2 Double
diffuse arr dt diff = foldr (\_ accum -> setDensBound $ diffApproxArgs accum) arr [1..diffAccuracy]
    where
        diffApproxArgs = diffApprox arr dt diff


diffApprox :: R.Array R.D R.DIM2 Double -> Int -> Double -> R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
diffApprox org dt diff arr = R.traverse arr id (\src i -> let sumNeighs = sum $ map (\j -> arr R.! j) (neighbors (n-1) i) in
    ((org R.! i) + a*sumNeighs)/(1+4*a)) where
    (R.Z R.:. n R.:. _) = R.extent arr
    a = (fromIntegral dt)*diff*(fromIntegral n)^^2

neighbors :: Int -> (R.Z R.:. Int) R.:. Int -> [(R.Z R.:. Int) R.:. Int]
neighbors maxIndex (R.Z R.:. x R.:. y) = map shapeOfList (filter invalid [[x+1,y],[x-1,y],[x,y+1],[x-1,y]]) where
    invalid = \[a,b] -> (a >= 0) && (a <= maxIndex) && (b >= 0) && (b <= maxIndex)

-- Velo test stuff
-- m=(R.computeP $ setXBound $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
-- t=(R.computeP $ setCorners $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
-- y=(R.computeP $ setCorners $ setXBound $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
