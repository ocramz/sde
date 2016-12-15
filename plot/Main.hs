{-# language NoMonomorphismRestriction, FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Primitive
-- import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Plots
import Plots.Axis
import Plots.Axis.Render
import Plots.Style
import Plots.Types
import Plots.Types.Line

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Data.Typeable


import System.Random.MWC.Probability

mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--   linePlot' mydata1
--   linePlot mydata2 $ do
--     key "data 2"
--     plotColor .= black
--   linePlot mydata3 $ key "data 3"

main :: IO ()
main = do
  dat <- asdf 25
  r2AxisMain $ myaxis dat
  

-- myaxis :: Axis B V2 Double
myaxis :: [(Double, Double)] -> Axis B V2 Double
myaxis dat = r2Axis &~ do
    linePlot dat $ do
      key "SDE"
      plotColor .= black


hierarchicalModel :: PrimMonad m => Prob m Double
hierarchicalModel = do
  [c, d, e, f] <- replicateM 4 $ uniformR (1, 10)
  a <- gamma c d
  b <- gamma e f
  -- p <- beta a b
  beta a b 
  -- n <- uniformR (5, 10)
  -- binomial n p

-- asdf :: PrimMonad m => Int -> m [(Double, Double)]
-- asdf :: Int -> StateT [(Double, Double)] Identity
asdf n = do
  g <- create
  zip [0 .. n'-1] <$> samples n' hierarchicalModel g where n' = fromIntegral n
