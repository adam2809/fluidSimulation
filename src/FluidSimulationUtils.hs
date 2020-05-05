{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module FluidSimulationUtils where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Shape (shapeOfList)
import FluidSquare
import Control.Monad (mapM)

-- Tam jest inaczej niż myślałem
setBoundsX :: (R.Source r Double) => R.Array r R.DIM2 Double -> R.Array R.D R.DIM2 Double
setBoundsX arr = R.traverse arr id (\src (R.Z R.:. x R.:. y) -> if
    | x == 0 -> negate $ src (R.Z R.:. 1 R.:. y)
    | x == n-1 -> negate $ src (R.Z R.:. (n-2) R.:. y)
    | otherwise -> 0) where
        (R.Z R.:. n R.:. _) = R.extent arr

setCorners :: (R.Source r Double) => R.Array r R.DIM2 Double -> R.Array R.D R.DIM2 Double
setCorners arr = R.traverse arr id (\src (R.Z R.:. x R.:. y) -> if [x,y] `elem` cornerIndicies then
    let [x',y'] = map (\coord -> if coord == 0 then 1 else n-1) [x,y] in
        ((arr R.! (R.Z R.:. x' R.:. y)) + (arr R.! (R.Z R.:. x R.:. y'))) / 2
else
    arr R.! (R.Z R.:. x R.:. y)) where
        (R.Z R.:. n R.:. _) = R.extent arr
        cornerIndicies = mapM (const [0,n-1]) [1,2]


diffApprox :: (R.Source r Double) => R.Array r R.DIM2 Double -> Int -> Double -> R.Array r R.DIM2 Double -> R.Array R.D R.DIM2 Double
diffApprox org dt diff arr = R.traverse arr id (\src i -> let sumNeighs = sum $ map (\j -> arr R.! j) (neighbors (n-1) i) in
    ((org R.! i) + a*sumNeighs)/(1+4*a)) where
    (R.Z R.:. n R.:. _) = R.extent arr
    a = (fromIntegral dt)*diff*(fromIntegral n)^^2

neighbors :: Int -> (R.Z R.:. Int) R.:. Int -> [(R.Z R.:. Int) R.:. Int]
neighbors maxIndex (R.Z R.:. x R.:. y) = map shapeOfList (filter invalid [[x+1,y],[x-1,y],[x,y+1],[x-1,y]]) where
    invalid = \[a,b] -> (a >= 0) && (a <= maxIndex) && (b >= 0) && (b <= maxIndex)


m=(R.computeP $ setBoundsX $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
t=(R.computeP $ setCorners $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
y=(R.computeP $ setCorners $ setBoundsX $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
