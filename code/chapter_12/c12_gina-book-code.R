# working thru c-12 book code
# aug 24 2020
# updated oct 5 for anabelle's talk

library(rethinking)
library(tibble)
library(dplyr)
theme_set(theme_bw())


# explore what a beta dist looks like -------------------------------------

pbar <- 0.5
theta <- 2



curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "prob", ylab = "density")

#--this isn't right. why?
tibble(x = seq(from = 0, to = 1, by = 0.1)) %>% 
  mutate(y = dbeta(x, pbar, theta)) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

#--if I use his dbeta2 function it's fine. What's up?
tibble(x = seq(from = 0, to = 1, by = 0.1)) %>% 
  mutate(y = dbeta2(x, pbar, theta)) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

#-ohhhh. You don't feed the beta dist prob and theta directly. 
# # THis is his function
# dbeta2 <- function( x , prob , theta , log=FALSE ) {
#   a <- prob * theta
#   b <- (1-prob) * theta
#   dbeta( x , shape1=a , shape2=b , log=log )
# }

#-ummm what's the difference between the rethinking dbeta2 and stats dbeta?


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
