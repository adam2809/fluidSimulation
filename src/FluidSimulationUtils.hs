{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module FluidSimulationUtils where

import qualified Data.Array.Repa as R
import FluidSquare
import Control.Monad (mapM)

-- Tam jest inaczej niż myślałem
setBoundsX :: (R.Source r Double) => R.Array r R.DIM3 Double -> R.Array R.D R.DIM3 Double
setBoundsX arr = R.traverse arr id (\src (R.Z R.:. x R.:. y R.:. z) -> let
                                        (R.Z R.:. n R.:. _ R.:. _) = R.extent arr in
                                   if
                                        | x == 0 -> negate $ src (R.Z R.:. 1 R.:. y R.:. z)
                                        | x == n-1 -> negate $ src (R.Z R.:. (n-2) R.:. y R.:. z)
                                        | otherwise -> 0
                                   )

setCorners :: (R.Source r Double) => R.Array r R.DIM3 Double -> R.Array R.D R.DIM3 Double
setCorners arr = R.traverse arr id (\src (R.Z R.:. x R.:. y R.:. z) -> if [x,y,z] `elem` cornerIndicies then
    let [x',y',z'] = map (\coord -> if coord == 0 then 1 else n-1) [x,y,z] in
        ((arr R.! (R.Z R.:. x' R.:. y R.:. z)) + (arr R.! (R.Z R.:. x R.:. y' R.:. z)) + (arr R.! (R.Z R.:. x R.:. y R.:. z')) / 3)
else
    arr R.! (R.Z R.:. x R.:. y R.:. z)) where
        (R.Z R.:. n R.:. _ R.:. _) = R.extent arr
        cornerIndicies = mapM (const [0,n-1]) [1,2,3]
        cnvList2Shape [x,y,z] = (R.Z R.:. x R.:. y R.:. z)



m=(R.computeP $ setBoundsX $ velocityX testFc) :: IO(R.Array R.U R.DIM3 Double)
t=(R.computeP $ setCorners $ velocityX testFc) :: IO(R.Array R.U R.DIM3 Double)
y=(R.computeP $ setCorners $ setBoundsX $ velocityX testFc) :: IO(R.Array R.U R.DIM3 Double)
