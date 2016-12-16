{-# language NoMonomorphismRestriction, FlexibleContexts #-}
module Main where

import Lib

import Control.Monad
import Control.Monad.Primitive

import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class

import Plots
import Plots.Axis
import Plots.Axis.Render
import Plots.Style
import Plots.Types
import Plots.Types.Histogram
import Plots.Types.Line

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

-- import Data.Typeable

import System.Random.MWC.Probability

import System.Environment (getArgs, withArgs)




main :: IO ()
main = do
  (plotType:n:args) <- getArgs
  -- putStrLn $ unwords [plotType, show (read n :: Int)]
  dat <- genDataset (read n :: Int) (alphaStable 1.885 1)
  let datp = indexed dat
  withArgs args $ case plotType of
    "series" -> mainWith (timeSeriesPlot datp)
    "hist"  -> mainWith (histPlot dat)
    _ -> mainWith (timeSeriesPlot datp)
    where
      timeSeriesPlot :: [(Double, Double)] -> IO (Axis B V2 Double)        
      timeSeriesPlot d = execStateT ?? r2Axis $ do
        xMin ?= 0
        linePlot d $ do
          key "Time series"
          plotColor .= red
          lineStyle %= lwN 0.001
          -- lineStyle %= (dashingG [0.3, 0.5] 0 #
          --               lwN 0.01)
      histPlot :: [Double] -> IO (Axis B V2 Double)
      histPlot d = execStateT ?? r2Axis $ 
       histogramPlot d $ do
         -- key "SDE"
         plotColor .= blue
         areaStyle . _opacity .= 0.5


      




-- | Data generation

genDataset :: (PrimMonad m, RealFloat a) => Int -> Prob m a -> m [a]
genDataset n model = do
  g <- create
  filter (not . isNaN) <$> samples n' model g  where
    n' = fromIntegral n

-- genDataset :: PrimMonad m => Int -> m [Double]
-- genDataset n = do
--   g <- create
--   filter (not . isNaN) <$> samples n' model g  where
--     n' = fromIntegral n
--     model = alphaStable 1.885 1


-- hierarchicalModel :: PrimMonad m => Prob m Double
-- hierarchicalModel = do
--   [c, d, e, f] <- replicateM 4 $ uniformR (1, 10)
--   a <- gamma c d
--   b <- gamma e f
--   -- p <- beta a b
--   beta a b 
--   -- n <- uniformR (5, 10)
--   -- binomial n p






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




-- | Utilities

indexed :: (Num a, Enum a) => [b] -> [(a, b)]
indexed dd = let n = fromIntegral $ length dd  in zip [0 .. n-1] dd
