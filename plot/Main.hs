{-# language NoMonomorphismRestriction, FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Primitive

import Control.Monad.Trans.State
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





main :: IO ()
main = mainWith myaxis --(myaxis :: Int -> IO (Axis B V2 Double))


myaxis :: Int -> Double -> IO (Axis B V2 Double)
myaxis n lw = execStateT ?? r2Axis $ do
    dat <- genDataset n
    xMin ?= 0
    xMax ?= 200
    linePlot dat $ do
      key "SDE"
      plotColor .= red
      lineStyle %= lwN lw
      -- lineStyle %= (dashingG [0.3, 0.5] 0 #
      --               lwN 0.01)


-- | Data generation

genDataset :: (PrimMonad m, Num a, Enum a) => Int -> m [(a, Double)]
genDataset n = do
  g <- create
  zip [0 .. n'-1] <$> samples n' hierarchicalModel g  where n' = fromIntegral n

hierarchicalModel :: PrimMonad m => Prob m Double
hierarchicalModel = do
  [c, d, e, f] <- replicateM 4 $ uniformR (1, 10)
  a <- gamma c d
  b <- gamma e f
  -- p <- beta a b
  beta a b 
  -- n <- uniformR (5, 10)
  -- binomial n p






-- | example

-- stock :: MonadIO m => String -> m (Response ByteString)
-- stock s = liftIO $ get ("http://ichart.yahoo.com/table.csv?s=" ++ s)

-- myaxis :: IO (Axis B V2 Double)
-- myaxis = execStateT ?? r2Axis $ do
--   goog <- stock "GOOG"
--   appl <- stock "AAPL"
--   let stocks r = filterStocks . parseStocks $ r ^. responseBody
--   linePlot (stocks goog) $ key "google"
--   linePlot (stocks appl) $ key "apple"
--   xAxis . tickLabelFunction .= autoTimeLabels

--   xLabel .= "date"
--   yLabel .= "closing (dollars)"

--   yMin ?= 0

-- main :: IO ()
-- main = mainWith myaxis
