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

`S_alpha(1, 0, 0)` denotes the Levy-stable distribution with parameters `(alpha, 1)`, where `0 < alpha <= 2`. The special cases `alpha=1` and `alpha=2` correspond to the Cauchy-Lorentz and Gaussian distributions, respectively. Values of the `alpha` parameter smaller than 1 result in sudden large deviations typical of "heavy tailed" distributions, which can be used to model shocks or phase changes in underlying phenomena.








[1] K.Li and Oestergaard, J. - Inference for a stochastic volatility model using MCMC with ABC-SMC approximation