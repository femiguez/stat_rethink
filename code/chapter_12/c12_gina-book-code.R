# working thru c-12 book code
# aug 24 2020
# updated oct 5 for anabelle's talk

# I updated R in an attempt to get ulam to work on my laptop
# This writeLines thing doesn't work
# https://cran.r-project.org/bin/windows/Rtools/

#install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
#devtools::install_github("rmcelreath/rethinking")
# NOTE: when reinstalling stan, don't do the makevars.win thing. 
# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
#pkgbuild::has_build_tools(debug = TRUE)
#library("rstan") # observe startup messages
# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)
# 
# schools_dat <- list(J = 8, 
#                     y = c(28,  8, -3,  7, -1,  1, 18, 12),
#                     sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
# 
# fit <- stan(file = 'schools.stan', data = schools_dat)
# print(fit)
# plot(fit)
# pairs(fit, pars = c("mu", "tau", "lp__"))


library(rethinking)
library(dplyr)
library(tidyr)

# explore what a beta dist looks like -------------------------------------

pbar <- 0.5
theta <- 2



curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "prob", ylab = "density")

#--this isn't right. why?
tibble::tibble(x = seq(from = 0, to = 1, by = 0.1)) %>% 
  dplyr::mutate(y = dbeta(x, pbar, theta)) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

#--if I use his dbeta2 function it's fine. What's up?
tibble::tibble(x = seq(from = 0, to = 1, by = 0.1)) %>% 
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

## see if I can get ulam working...

data(rugged)

d <- rugged %>% 
  mutate(log_gdp = log(rgdppc_2000)) %>% 
  drop_na %>% 
  mutate(log_gdp_std = log_gdp/mean(log_gdp),
         rugged_std = rugged/max(rugged),
         cid = ifelse(cont_africa == 1, 1, 2))

m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = d)

precis(m8.3)

dat_slim <- list(
  log_gdp_std = d$log_gdp_std,
  rugged_std = d$rugged_std,
  cid = as.integer(d$cid)
)

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = dat_slim, chains = 1)

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
