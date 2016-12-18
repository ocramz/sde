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
-- import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Postscript
import Diagrams.Backend.Postscript.CmdLine

-- import Data.Typeable

import System.Random.MWC.Probability

import System.Environment (getArgs, withArgs)




main :: IO ()
main = do
  -- (plotType:ns:alps:bets:args) <- getArgs
  -- let alp = read alps :: Double
  --     bet = read bets :: Double
  --     md = alphaStableWD 0 alp bet
  -- let ds = unwords ["alpha",show alp,"beta",show bet]
  (plotType:ns:aa:bb:sigs:alps:args) <- getArgs
  let n = read ns :: Int
      a = read aa :: Double
      b = read bb :: Double
      sig = read sigs :: Double
      alp = read alps :: Double
      -- model = stochVolatility1 a b sig alp
      -- md = create >>= samplesTrans n (stochVolatility1 a b sig alp) 0
      -- md = withIOGen (sampleSDEn n (brownian 0.1) 1)
      md = map sv1y <$> withIOGen (sampleSDEn n (stochVolatility1 a b sig alp) (SV1 0 0))
      ds = unwords ["a",show a,"b",show b,"sigma",show sig,"alpha",show alp]
  -- let n = read ns :: Int
  dat <- md
  let datp = indexed dat
  withArgs args $ case plotType of
    "series" -> mainWith (timeSeriesPlot ds datp)
    "hist"  -> mainWith (histPlot ds dat)
    _ -> mainWith (timeSeriesPlot ds datp)

timeSeriesPlot :: String -> [(Double, Double)] -> IO (Axis B V2 Double)
timeSeriesPlot descStr d = execStateT ?? r2Axis $ do
        xMin ?= 0
        xMax ?= fromIntegral (length d)
        linePlot d $ do
          -- -- key descStr
          plotColor .= blue
          lineStyle %= lwN 0.0001
        legendStyle . _lw .= 0
        legendTextWidth *= 4
          -- lineStyle %= (dashingG [0.3, 0.5] 0 #
          --               lwN 0.01)

histPlot :: String -> [Double] -> IO (Axis B V2 Double)
histPlot descStr d = execStateT ?? r2Axis $ do
       histogramPlot d $ do
         -- key descStr
         plotColor .= blue
         areaStyle . _opacity .= 0.5
         numBins .= 50
       legendStyle . _lw .= 0
       legendTextWidth *= 4


withPlotType :: (Int -> IO [b1]) -> (String -> IO b) -> IO b
withPlotType fm f = do
  (plotType:ns:args) <- getArgs
  let n = read ns :: Int
  dat <- fm n
  let datp = indexed dat
  withArgs args (f plotType)

      
-- create >>= samplesTrans 100 (stochVolatility1 0.0042 0.991 0.104 1.885) 0



-- | Data generation



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
