# Gina working through book code
# july 27 2020
# chapter 8, interactions

library(tidyverse)
library(rethinking)
theme_set(theme_bw())

data("rugged")
raw_d <- rugged %>% 
  as_tibble() %>% 
  filter(!is.na(rgdppc_2000)) %>% 
  mutate(log_gdp = log(rgdppc_2000),
         log_gdp_std = log_gdp/mean(log_gdp), #-centers it about the mean. So 0.9 means 90% of the mean
         rugged_std = rugged/max(rugged),  #--scales it to 0-1
         cid = ifelse(cont_africa == 1, 1, 2)) #--africa indicator variable

d <- 
  raw_d %>% 
  select(log_gdp, log_gdp_std, rugged, rugged_std, cont_africa, cid)

d

#--what is the mean ruggedness?
d %>% 
  summarise(mean_rugged = mean(rugged_std)) 

d %>% 
  ggplot(aes(rugged_std)) + 
  geom_histogram()

#-oh dominica. Fernando's been there I think.
raw_d %>% 
  filter(rugged_std == min(rugged_std)) %>% 
  select(country)

#--fit the simple model
m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a * b*(rugged_std - 0.215), #--this just makes it so our prior for b is centered on 0
    a ~ dnorm(1, 1), #--we made it so the mean is 1
    b ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d)

set.seed(7)
m8.1_prior <- extract.prior(m8.1)


# question, what am I doing here? -----------------------------------------

#--not using the link function, I'm just looking at the results of the priors I gave it, right?
# these slopes are crazy!
  ggplot(data = d, aes(x = rugged_std, y = log_gdp_std)) +
    geom_point() +
  #--use the extracted prior here
  geom_abline(intercept = m8.1_prior$a[1:100],
              slope = m8.1_prior$b[1:100],
              size = 1/3, alpha = .3) +
  labs(title = "b ~ norm(0, 1)")


#--he uses the link function. I'm not quite sure why you can't just use the prior?

#--what he does
rugged_seq <- seq(from = -0.1, to = 1.1, length.out = 30)
m8.1_mu <- link(m8.1, post = m8.1_prior, data = data.frame(rugged_std = rugged_seq))


# his example --------------------------------------------------------------------

# data(chimpanzees)
# 
# d <- list( 
#   pulled_left = chimpanzees$pulled_left ,
#   prosoc_left = chimpanzees$prosoc_left ,
#   condition = chimpanzees$condition ,
#   actor = as.integer( chimpanzees$actor ) ,
#   blockid = as.integer( chimpanzees$block )
# )
# 
# m <- quap(
#   alist(
#     pulled_left ~ dbinom(1,theta),
#     logit(theta) <- a + aj[actor] + bp*prosoc_left + bpc*condition*prosoc_left,
#     aj[actor] ~ dnorm( 0 , 1 ),
#     a ~ dnorm(0,2),
#     bp ~ dnorm(0,1),
#     bpc ~ dnorm(0,1)
#   ) ,
#   data=d )
# 
# prior <- extract.prior(m,n=1e4)
# post <- extract.samples(m)
# ps <- par("bty")
# par(bty="n")
# plot( precis(prior,2) , col.ci="gray" , xlim=c(-3,3.5) , bty="n" )
# plot( precis(post,2) , add=TRUE , pch=16 )
# par(bty=ps)               

               

m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a * b*(rugged_std - 0.215), #--this just makes it so our prior for b is centered on 0
    a ~ dnorm(1, 0.1),
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = d)

set.seed(7)
m8.1_prior <- extract.prior(m8.1)

 #--tighten priors
ggplot(data = d, aes(x = rugged_std, y = log_gdp_std)) +
  geom_point() +
  #--use the extracted prior here
  geom_abline(intercept = m8.1_prior$a[1:100],
              slope = m8.1_prior$b[1:100],
              size = 1/3, alpha = .3) +
  labs(title = "b ~ norm(0, 0.3)")

precis(m8.1)



# allow intercept to vary  ------------------------------------------------



m8.2 <- 
  quap(
    alist(
      log_gdp_std ~ dnorm(mu, sigma),
      mu <- a[cid] + b*(rugged_std - 0.215),
      a[cid] ~ dnorm(1, 0.1),
      b ~ dnorm(0, 0.3),
      sigma ~ dexp(1)
    ), 
    data = d
    )

compare(m8.1, m8.2)
precis(m8.2)
precis(m8.2, depth = 2)


#--posterior contrast
m8.2_post <- extract.samples(m8.2)
diff_a1_a2 <- m8.2_post$a[,1] - m8.2_post$a[,2]
PI(diff_a1_a2) #--remember PI is the compatability interval. confidently negative

#--graph it

rugged_seq <- seq(from = -0.1, 1.1, length.out = 30)

mu.O <- link(m8.2, data = data.frame(cid = 2, rugged_std = rugged_seq))
mu.A <- link(m8.2, data = data.frame(cid = 1, rugged_std = rugged_seq))

mu.O_mu <- apply(mu.O, 2, mean)
mu.A_mu <- apply(mu.A, 2, mean)

ggplot() + 
  geom_point(data = d, aes(rugged_std, log_gdp_std, color = as.factor(cid))) + 
  geom_line(aes(x = rugged_seq, y = mu.O_mu), color = "blue", size = 5) + 
  geom_line(aes(x = rugged_seq, y = mu.A_mu), color = "pink", size = 5) 


# allow slope to vary -----------------------------------------------------


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

compare(m8.1, m8.2, m8.3, func = PSIS)

#--the fact that model 2 gets a little bit of weight is an indiation of possible over fitting
#--look at the k values from teh PSIS
plot(PSIS (m8.3, pointwise = TRUE)$k)


#--make the interaction plot, try to include the PIs this time
mu.O <- link(m8.3, data = data.frame(cid = 2, rugged_std = rugged_seq))
mu.A <- link(m8.3, data = data.frame(cid = 1, rugged_std = rugged_seq))

mu.O_mu <- apply(mu.O, 2, mean)
mu.A_mu <- apply(mu.A, 2, mean)


mu.O_PI <- apply(mu.O, 2, PI)
mu.A_PI <- apply(mu.A, 2, PI)

ggplot() + 
  geom_point(data = d, aes(rugged_std, log_gdp_std, color = as.factor(cid))) + 
  geom_line(aes(x = rugged_seq, y = mu.O_mu), color = "blue", size = 5) + 
  geom_line(aes(x = rugged_seq, y = mu.A_mu), color = "pink", size = 5) +
  geom_ribbon(aes(x = rugged_seq, ymin = mu.O_PI[1,], ymax = mu.O_PI[2,]), alpha = 0.2) + 
  geom_ribbon(aes(x = rugged_seq, ymin = mu.A_PI[1,], ymax = mu.A_PI[2,]), alpha = 0.2)



# graph it the mind blowing way -------------------------------------------

muA <- link(m8.3, data = data.frame(cid = 1, rugged_std = rugged_seq))
muN <- link(m8.3, data = data.frame(cid = 2, rugged_std = rugged_seq))
delta <- muA - muN

mus <- apply(delta, 2, mean)
pis <- apply(delta, 2, PI)

tibble(rugged_seq = rugged_seq,
       mus = mus,
       pi_lo = pis[1,],
       pi_hi = pis[2,]) %>% 
ggplot(aes(x = rugged_seq, y = mus)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = pi_lo, ymax = pi_hi), alpha= 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "At low ruggedness, moving a nation to africa hurts it's GDP\n at high rugg, moving a nation to Africa helps it")


# tulips ------------------------------------------------------------------

data(tulips)  
dt <- tulips %>% 
  mutate(blooms_std = blooms / max(blooms), #-don't standardize if 0 is meaninful. Like with ruggedness, only scale it
         water_cent = water - mean(water),
         shade_cent = shade - mean(shade))

#--picking reasonable priors
# if we say the bloom size is centered at 0.5, it has to be between 0-1. What sigma would give us that?

a <- rnorm(1000, 0.5, 1) #--say sigma = 1
sum( a < 0 | a > 1) / length(a) #--how often is a less than 0 or greater than 1?
# a lot

a <- rnorm(1000, 0.5, 0.25) #--say sigma = 0.25
sum( a < 0 | a > 1) / length(a) #--how often is a less than 0 or greater than 1?
# 5%. Tolerable. 

# think about slopes. Biggest slope would be 1/2 or 0.5 (or -0.5). 
a <- rnorm(1000, 0, 0.25) #--say sigma = 0.25
sum( a < -0.5 | a > 0.5) / length(a) #--how often is a less than 0 or greater than 1?

m8.4 <- quap(
  alist(
    blooms_std <- dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent,
    a ~ dnorm(0.5, 0.25),
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = dt)

# look at prior
m8.4_prior <- extract.prior(m8.4)

#--um actually what goes on the x axis?

#--make an interaction model

m8.5 <- quap(
  alist(
    blooms_std <- dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a ~ dnorm(0.5, 0.25),
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    bws ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = dt)


# how to plot interactions ------------------------------------------------


