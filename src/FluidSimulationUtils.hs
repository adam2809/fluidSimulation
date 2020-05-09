{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FluidSimulationUtils where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Shape (shapeOfList,listOfShape)
import FluidSquare
import Control.Monad (mapM)
import Debug.Trace


stepDensity :: R.Array R.D R.DIM2 Double -> Int -> Double -> R.Array R.D R.DIM2 Double
stepDensity arr dt diff = diffuse dt diff arr

-- TODO The definition below is a placeholder
setXVeloBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setXVeloBound = setCorners . setBound True


setYVeloBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setYVeloBound = setCorners . R.transpose . (setBound True) . R.transpose


setDensBound :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setDensBound = setCorners . setBound False


setBound :: Bool -> R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setBound negX arr = R.traverse arr id (\src (R.Z R.:. x R.:. y) -> if
    | [x,y] `elem` cornerIndicies -> src (R.Z R.:. x R.:. y)
    | x == 0 -> negOrNot $ src (R.Z R.:. 1 R.:. y)
    | x == n-1 -> negOrNot $ src (R.Z R.:. (n-2) R.:. y)
    | y == 0 -> src (R.Z R.:. x R.:. 1)
    | y == n-1 -> src (R.Z R.:. x R.:. (n-2))
    | otherwise -> src (R.Z R.:. x R.:. y)) where
        negOrNot = if negX then negate else id
        (R.Z R.:. n R.:. _) = R.extent arr
        cornerIndicies = mapM (const [0,n-1]) [1,2]

setCorners :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
setCorners arr = R.traverse arr id (\src (R.Z R.:. x R.:. y) -> if [x,y] `elem` cornerIndicies then
    let [x',y'] = map (\coord -> if coord == 0 then 1 else n-2) [x,y] in
        ((arr R.! (R.Z R.:. x' R.:. y)) + (arr R.! (R.Z R.:. x R.:. y'))) / 2
else
    arr R.! (R.Z R.:. x R.:. y)) where
        (R.Z R.:. n R.:. _) = R.extent arr
        cornerIndicies = mapM (const [0,n-1]) [1,2]

diffAccuracy = 7

diffuse ::  Int -> Double -> R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
diffuse dt diff arr = (iterate (\x -> setDensBound $ diffApproxArgs x) arr ) !! diffAccuracy
    where
        diffApproxArgs = diffApprox arr dt diff


showAll :: R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
showAll arr = R.traverse arr id (\src i -> trace (show (src i) ++ "\tNNNNNNNNN") $ src i)


diffApprox :: R.Array R.D R.DIM2 Double -> Int -> Double -> R.Array R.D R.DIM2 Double -> R.Array R.D R.DIM2 Double
diffApprox org dt diff arr = R.traverse arr id (\src i -> let sumNeighs = sum $ map (\j -> arr R.! j) (neighbors (n-1) i) in
    ((org R.! i) + a*sumNeighs)/(1+4*a)) where
    (R.Z R.:. n R.:. _) = R.extent arr
    a = (fromIntegral dt)*diff*(fromIntegral n)^^2

neighbors :: Int -> (R.Z R.:. Int) R.:. Int -> [(R.Z R.:. Int) R.:. Int]
neighbors maxIndex (R.Z R.:. x R.:. y) = map shapeOfList (filter invalid [[x+1,y],[x-1,y],[x,y+1],[x-1,y]]) where
    invalid = \[a,b] -> (a >= 0) && (a <= maxIndex) && (b >= 0) && (b <= maxIndex)

-- Velo test stuff
xBoundSetting = (R.computeP $ setXVeloBound $ R.delay $ velocityX testFs) :: IO(R.Array R.U R.DIM2 Double)
yBoundSetting = (R.computeP $ setYVeloBound $ R.delay $ velocityY testFs) :: IO(R.Array R.U R.DIM2 Double)


-- Dens test stuff
densBoundSetting = (R.computeP $ setDensBound $ R.delay $ density testFs) :: IO(R.Array R.U R.DIM2 Double)
densDiff = (R.computeP $ diffuse (timestep testDiff) (diffusion testDiff) (R.delay $ density testDiff)) :: IO(R.Array R.U R.DIM2 Double)
densDiffS = (R.computeS $ diffuse (timestep testDiff) (diffusion testDiff) (R.delay $ density testDiff)) :: R.Array R.U R.DIM2 Double

dArgs = diffApprox (R.delay $ density testDiff) (timestep testDiff) (diffusion testDiff)
