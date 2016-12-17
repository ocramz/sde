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
import Plots.Legend
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
  (plotType:ns:alps:bets:args) <- getArgs
  -- putStrLn $ unwords [plotType, show (read n :: Int)]
  let n = read ns :: Int
      alp = read alps :: Double
      bet = read bets :: Double
  dat <- genDataset n (alphaStableWD 0 alp bet)
  let datp = indexed dat
      ds = unwords ["alpha",show alp,"beta",show bet]
  withArgs args $ case plotType of
    "series" -> mainWith (timeSeriesPlot ds datp)
    "hist"  -> mainWith (histPlot ds dat)
    _ -> mainWith (timeSeriesPlot ds datp)
    where
      timeSeriesPlot :: String -> [(Double, Double)] -> IO (Axis B V2 Double)        
      timeSeriesPlot descStr d = execStateT ?? r2Axis $ do
        xMin ?= 0
        linePlot d $ do
          key descStr
          plotColor .= red
          lineStyle %= lwN 0.001
        legendStyle . _lw .= 0
        legendTextWidth *= 2
          -- lineStyle %= (dashingG [0.3, 0.5] 0 #
          --               lwN 0.01)
      histPlot :: String -> [Double] -> IO (Axis B V2 Double)
      histPlot descStr d = execStateT ?? r2Axis $ do
       histogramPlot d $ do
         key descStr
         plotColor .= blue
         areaStyle . _opacity .= 0.5
       legendStyle . _lw .= 0
       legendTextWidth *= 2


      




-- | Data generation

genDataset :: (PrimMonad m, RealFloat a) => Int -> Prob m a -> m [a]
genDataset n model = do
  g <- create
  samples n' model g  where
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
