# sde

[![Build Status](https://travis-ci.org/ocramz/sde.png)](https://travis-ci.org/ocramz/sde)

Numerical experiments with stochastic differential equations in Haskell


![stochastic-volatility-1](https://rawgit.com/ocramz/sde/master/doc/stoch_volatility1.png)

In the figure above, a sample path (left) and histogram of the stochastic volatility model of Equation 1:

y_t = a exp(x_t / 2) v_t           (1)
x_t = b x_{t-1} + sigma u_t

where

u_t ~ N(0, 1)
v_t ~ S_alpha(1, 0, 0)

discussed in [1]








[1] K.Li and Oestergaard, J. - Inference for a stochastic volatility model using MCMC with ABC-SMC approximation