# Gina working through book code
# aug 4 2020
# chapter 9, mcmc!

library(dplyr)
library(tidyr)
library(rethinking)
theme_set(theme_bw())


# ulam --------------------------------------------------------------------

data(rugged)

d <- rugged %>% 
  mutate(log_gdp = log(rgdppc_2000),
         log_gdp_std = log_gdp/mean(log_gdp, na.rm = T),
         rugged_std = rugged/max(rugged),
         cid = ifelse(cont_africa == 1, 1, 2)) %>% 
  select(log_gdp_std, rugged_std, cid) %>% 
  na.omit()


head(d)


m8.3 <- 
  quap(
    alist(
      log_gdp_std ~ dnorm(mu, sigma),
      mu <- a[cid] + b[cid]*(rugged_std - 0.215),
      a[cid] ~ dnorm(1, 0.1),
      b[cid] ~ dnorm(0, 0.3),
      sigma ~ dexp(1)
    ), 
    data = d
  )
precis(m8.3, depth = 2)


dat_slim <- list(
  log_gdp_std = d$log_gdp_std,
  rugged_std = d$rugged_std,
  cid = as.integer(d$cid)
)

str(dat_slim)

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim,
  chains = 1
)

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
