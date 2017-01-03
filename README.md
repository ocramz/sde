# sde

[![Build Status](https://travis-ci.org/ocramz/sde.png)](https://travis-ci.org/ocramz/sde)

Numerical experiments with stochastic differential equations in Haskell


![stochastic-volatility-1](https://rawgit.com/ocramz/sde/master/doc/stoch_volatility1.png)

In the figure above, a sample path (left) and histogram of the variable `y_t` as formulated in the following stochastic volatility model [1]:

    y_t = a exp(x_t / 2) v_t           (1)

    x_t = b x_{t-1} + sigma u_t

where

    u_t ~ N(0, 1)
    
    v_t ~ S_alpha(1, 0, 0).

`S_alpha(1, 0, 0)` denotes the Levy-stable distribution with parameters `(alpha, 1)`, where `0 < alpha <= 2`. The special cases `alpha=1` and `alpha=2` correspond to the Cauchy-Lorentz and Gaussian distributions, respectively. Values of the `alpha` parameter smaller than 2 result in sudden large deviations typical of "heavy tailed" distributions, which can be used to model shocks or phase changes in underlying phenomena.


## Implementation details

The library relies on `mwc-probability` for its primitive sampling functionality, and on the `StateT` monad transformer. 

The SDE integration process can be seen as an interleaved sequence of random sampling and state transformation. This idea is captured in the `sampleSDE` function, shown below:

    sampleSDE ::
       Monad m => Prob m a -> (b -> a -> b) -> Gen (PrimState m) -> StateT b m b
    sampleSDE msf f g = do
      x <- get
      w <- lift $ sample msf g
      let z = f x w
      put z
      return z


## Notes

Currently, only the Euler-Maruyama integration method has been implemented (the `sampleSDE` function shown above); this works for some models but is inaccurate and unstable for others; therefore one of the next development goals will be to include higher-order integration schemes such as those shown in Wilkie [2].







[1] Li, K. and Oestergaard, J. - Inference for a stochastic volatility model using MCMC with ABC-SMC approximation

[2] Wilkie, J. - Numerical methods for stochastic differential equations (https://arxiv.org/abs/quant-ph/0407039)