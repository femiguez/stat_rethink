# Gina working through book code
# july 3 2020
# chapter 6, multiple regression contin.

library(tidyverse)
library(rethinking)
theme_set(theme_bw())


# 6.1 multicollinearity ---------------------------------------------------

N <- 100

d <- tibble(height = rnorm(N, 10, 2),
       leg_prop = runif(N, 0.4, 0.5),
       leg_left = height * leg_prop + rnorm(N, 0, 0.2),
       leg_right = height * leg_prop + rnorm(N, 0, 0.2))


m6.1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a <- dnorm(10, 10), #--this was a very bad prior, i changed it
    bl <- dnorm(2, 10),
    br <- dnorm(2, 10),
    sigma ~ dexp(1)),
  data = d)

precis(m6.1)
plot(precis(m6.1))

post <- extract.samples(m6.1)
head(post)

#--the sum of bl and br is well estimated (~2, meaning height is 2x leg legnth)
post %>% 
  as_tibble() %>% 
  mutate(blbr = bl + br) %>% 
  ggplot(aes(blbr)) + 
  geom_density()

#--fit a model using one leg only
m6.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left,
    a <- dnorm(10, 10), 
    bl <- dnorm(2, 10),
    sigma ~ dexp(1)),
  data = d)

plot(precis(m6.2))


# 6.1 milk ----------------------------------------------------------------
rm(list = ls())

data(milk)
d <- milk %>% 
  mutate(K = standardize(kcal.per.g),
         F = standardize(perc.fat),
         L = standardize(perc.lactose))


#--fit a model using only fat
m6.3 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bF*F,
    a <- dnorm(0, 0.2), 
    bF <- dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d)

#--fit a model using only fat
m6.4 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bL*L,
    a <- dnorm(0, 0.2), 
    bL <- dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d)

plot(precis(m6.3))
plot(precis(m6.4))

#--fit a model using both
m6.5 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bL*L + bF*F,
    a <- dnorm(0, 0.2), 
    bL <- dnorm(0, 0.5),
    bF <- dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d)

plot(precis(m6.5))

#--it must be marginal, order doesn't matter
m6.5b <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a +  bF*F + bL*L,
    a <- dnorm(0, 0.2), 
    bL <- dnorm(0, 0.5),
    bF <- dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d)

plot(coeftab(m6.5, m6.5b))
