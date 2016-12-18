{-# language FlexibleContexts #-}
module Lib where

import Control.Applicative
import Control.Monad

import Control.Monad.Primitive
-- import qualified Control.Monad.State as S

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, evalStateT)

import System.Random.MWC.Probability

-- import Pipes (Producer, yield, (>->), (>~), await, runEffect)
-- import qualified Pipes.Prelude as P (take, mapM_, mapM, sequence)



-- newtype Prob m a = Prob { sample :: Gen (PrimState m) -> m a }
-- newtype Transition m a = Trans { runTrans :: StateT a (Prob m) a}

newtype Transition m a = Trans { runTrans :: Gen (PrimState m) -> StateT a m a}


    
-- *** state transformers

-- | Template for a time-discrete SDE
sampleSDE ::
   Monad m => Prob m a -> (b -> a -> b) -> Gen (PrimState m) -> StateT b m b
sampleSDE msf f g = do
  x <- get
  w <- lift $ sample msf g
  let z = f x w
  put z
  return z

stepN :: Monad m => Int -> StateT s m a -> s -> m [a]
stepN n mm = evalStateT (replicateM n mm)

sampleSDEn ::
  Monad m => Int -> Transition m a -> a -> Gen (PrimState m) -> m [a]
sampleSDEn n sde x0 g = stepN n (runTrans sde g) x0











-- * Brownian random walk
-- | NB : the position at a time `t > t0` of a Brownian walker is the integral in time of a Wiener process (i.e. the sample path up to time `t` is cumulative sum of the Brownian displacements)
brownian ::
  PrimMonad m => Double -> Transition m Double
brownian sig = Trans (sampleSDE (normal 0 sig) (+))





-- * Stochastic volatility model (from Kang and Oestergaard, 2016)

data SV1 = SV1 {sv1x :: Double, sv1y :: Double} deriving (Eq)
instance Show SV1 where show (SV1 a b) = show (a, b)

stochVolatility1 :: PrimMonad m =>
   Double -> Double -> Double -> Double -> Transition m SV1
stochVolatility1 a b sig alpha = Trans (sampleSDE randf f) where
  randf = (,) <$> normal 0 1
              -- <*> alphaStable alpha 1
              <*> alphaStable100 alpha
  f (SV1 x _) (ut, vt) = let xt = b * x + sig * ut
                             yt = a * exp (xt / 2) * vt
                         in SV1 xt yt


  
  



-- * Levy-stable distribution
-- | 
-- genAlphaStable ::
--   PrimMonad m => Double -> Double -> Int -> m [Double]
-- genAlphaStable al be n = do
--   g <- create
--   samples n (alphaStable al be) g

-- genAlphaStable' :: Double -> Double -> Int -> IO [Double]
-- genAlphaStable' al be n = withSystemRandom . asGenIO $ \g -> samples n (alphaStable al be) g



  
-- | The Chambers-Mallows-Stuck algorithm for producing a S_alpha(beta) stable r.v., using the continuous reparametrization around alpha=1
alphaStable :: PrimMonad m => Double -> Double -> Prob m Double
alphaStable al be = do
  u0 <- uniform 
  w <- exponential 1
  let u = pi * u0 - 0.5 * pi -- uniform on (-pi/2, pi/2)
      eps = 1 - al
      k = 1 - abs eps
      phi0 = - 0.5 * pi * be * k / al
      tap0 = tan (al * phi0)
      z = (cos (eps * u) - tap0 * sin (eps * u)) / (w * cos u)
      ze = z**(eps / al)
  return $ (sin(al*u)/cos u - tap0 * (cos (al * u) /cos u - 1))*ze + tap0*(1-ze)


-- -- | replaces all NaNs with a default value
-- alphaStableWD :: PrimMonad m => Double -> Double -> Double -> Prob m Double
-- alphaStableWD defv al be = whenNaN defv <$> alphaStable al be


-- | Formula given by Li & Oestergaard for S_alpha(1, 0, 0) (i.e. stable distribution with beta=1)
alphaStable100 :: PrimMonad m => Double -> Prob m Double
alphaStable100 al = do
  u0 <- uniform
  w <- exponential 1
  let u = pi * u0 - 0.5 * pi -- uniform on (-pi/2, pi/2)
      t1 = sin (al * u)
      t2 = cos u**(-1/al)
      t3 = (cos ((al-1) * u) / w)**((1-al)/al)
      z = t1 * t2 * t3
  case al of 1 -> return $ tan u
             _ -> return z






-- ** Utilities

-- | Replace NaN with a default value
whenNaN :: RealFloat a => a -> a -> a
whenNaN val x
  | isNaN x   = val
  | otherwise = x

withIOGen:: (Gen RealWorld -> IO a) -> IO a
withIOGen = withSystemRandom . asGenIO














-- -- Pipes-based (follows https://hackage.haskell.org/package/mighty-metropolis-1.0.4/docs/src/Numeric-MCMC-Metropolis.html#mcmc )

-- chain :: Monad m => Transition m a -> a -> Gen (PrimState m) -> Producer a m ()
-- chain mm = loop where
--   loop s g = do
--     next <- lift $ sample (execStateT (runTrans mm) s) g
--     yield next
--     -- return next
--     loop next g

-- runChain n t x0 g = runEffect $ chain t x0 g >-> P.take n

-- runChain' n t x g = runEffect $ chain t x g >-> P.take n

-- -- -- `mcmc-types` introduces a Transition type: 
-- -- -- type Transition m a = StateT a (Prob m) ()






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
