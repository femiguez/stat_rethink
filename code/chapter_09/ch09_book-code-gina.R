# Gina working through book code
# aug 4 2020
# chapter 9, mcmc!

library(tidyverse)
library(rethinking)
theme_set(theme_bw())


# taming a wild chain -----------------------------------------------------

y <- c(-1, 1)

#--does it have to be connected to the internet?
m9.2 <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha, 
    alpha ~ dnorm(0, 1000), #--uninformative prior
    sigma ~ dexp(0.0001)
  ), data = list(y = y), chains = 3)
