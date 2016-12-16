{-# language FlexibleContexts #-}
module Lib where

import Control.Applicative

import Control.Monad.Primitive

import System.Random.MWC.Probability


-- stochVolatility a b sig alpha xtm ut = a * exp (xt / 2) * vt where
--   xt = b * xtm + sig * ut

-- stochV a b sig alpha xtm = do
--   ut <- normal 0 1
--   vt <- alphaStable100 alpha
--   -- let xt = b * xtm + (sig * ut)
--   let xt = (\u -> b * xtm + u * sig ) <$> ut
--   -- return $ a * exp (xt / 2) * vt
--   liftA2 (\x y -> a * exp (x/2) * y) xt vt

-- stochV a b sig alpha xtm = do
--   ut <- normal 0 1
--   vt <- alphaStable


                        

genAlphaStable ::
  PrimMonad m => Double -> Double -> Int -> m [Double]
genAlphaStable al be n = do
  g <- create
  samples n (alphaStable al be) g

-- | The Chambers-Mallows-Stuck algorithm for producing a S_alpha(beta) stable r.v., using the continuous reparametrization around alpha=1
alphaStable :: PrimMonad m => Double -> Double -> Prob m Double
alphaStable al be = do
  u <- normal (-0.5 * pi) (0.5 * pi)  -- Phi
  w <- exponential 1
  let eps = 1 - al
      k = 1 - abs eps
      phi0 = - 0.5 * pi * be * k / al
      tap0 = tan (al * phi0)
      z = (cos (eps * u) - tap0 * sin (eps * u)) / (w * cos u)
      ze = z**(eps / al)
  return $ (sin(al*u)/cos u - tap0 * (cos (al * u) /cos u - 1))*ze + tap0*(1-ze)



-- Not functional :

-- -- | The Chambers-Mallows-Stuck algorithm for producing a S_alpha(beta, c, mu) stable r.v., as reported in https://en.wikipedia.org/wiki/Stable_distribution#Simulation_of_stable_variables
-- alphaStable :: PrimMonad m => Double -> Double -> Double -> Double -> Prob m Double
-- alphaStable al be c mu = do
--   u <- normal (-0.5 * pi) (0.5 * pi)
--   w <- exponential 1
--   let zeta = be*tan(pi*al/2)
--   return $ case al of 1 ->
--                         let xi = pi/2
--                             s = pi/2 + be*u
--                             x = 1/xi*(s*tan u - be*log((pi/2*w*cos u)/s))
--                         in c*x + mu + (2/pi)*be*c*log c
--                       _ ->
--                         let xi = 1/al*atan(-zeta)
--                             t = u + xi
--                             t1 = (1 + zeta**2)**(1/(2*al))
--                             t2 = sin (al * t) / cos u**(1/al)
--                             t3 = (cos(u - al*t)/w)**((1-al)/al)
--                             x = t1 * t2 * t3
--                         in c*x + mu
