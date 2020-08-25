# working thru c-12 book code
# aug 24 2020

library(rethinking)
library(tidyverse)
theme_set(theme_bw())


# simulate zero-inflated poisson ------------------------------------------

prob_drink <- 0.2
rate_work <- 1

N <- 365

set.seed(365)
drink <- rbinom( N, 1, prob_drink )

y <- (1-drink)*rpois( N, rate_work)

hist(y)

m12.4 <- quap(
  alist(
    y ~ dzipois(p, lambda), #--p is prob of 0 (drinking), lambda is mean of poisson
    logit(p) <- ap, # logit to make it btwn 0-1. Why not use that beta thing?
    log(lambda) <- al, # log to make it linear positive
    ap ~ dnorm(-1.5, 1), 
    al ~ dnorm(1, 0.5)
  ), data = list(y = as.integer(y)))

precis(m12.4)

post <- extract.samples(m12.4)

inv_logit(-1.16) # prob drink, precis
mean(inv_logit(post$ap)) # prob drink, posterior -- why are these different?
exp(0.03)
