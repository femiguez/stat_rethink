library(tidyverse)
library(rethinking)
library(ggrepel)

# brain size and body mass ------------------------------------------------

#-make a tibble
d <- tibble(
  spname = c("Afar", "Africf", "Habi", "Bois", "Rudo", "Erga", "Sapi"),
  brainvol_cc = c(438, 452, 612, 521, 752, 871, 1350),
  mass_g = c(37, 35.5, 34.5, 41.5, 55.5, 61, 53.5)
  ) %>% 
  mutate(mass_std = (mass_g - mean(mass_g))/sd(mass_g),
         brain_std = brainvol_cc/max(brainvol_cc))

d 

d %>% 
  ggplot(aes(mass_std, brain_std)) + 
  geom_point() + 
  geom_label_repel(aes(label = spname))


#--fit with quap
m7.1 <- quap(
  alist(
    brain_std ~ dnorm(mu, exp(log_sigma)),
    mu <- a + b * mass_std,
    a ~ dnorm(0.5, 1),
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1) #--why not just use the dlnorm?
  ), 
  data = d)

s <- sim(m7.1) #-what does this give us again? the distribution around each data point?

#--his way
r <- apply(s, 2, mean) - d$brain_std
resid_var <- var2(r)

#--my way
sim_dat <- 
  s %>% 
  as_tibble() %>% 
  summarise_all(mean) %>% 
  pivot_longer(V1:V7) %>% 
  bind_cols(d) %>% 
  mutate(r = value - brain_std)

resid_var <- 
  sim_dat %>% 
  pull(r) %>% 
  var2()

outcome_var <- 
  sim_dat %>% 
  pull(brain_std) %>% 
  var2()

resid_var/outcome_var


R2_is_bad <- function(quap_fit){
  set.seed(17)
  s <- sim(quap_fit, refresh = 0)
  r <- apply(s, 2, mean) - d$brain_std
  1 - var2(r)/var2(d$brain_std)
}

R2_is_bad(m7.1)


#--fit with quap but use dlnorm
m7.1a <- quap(
  alist(
    brain_std ~ dnorm(mu, sigma),
    mu <- a + b * mass_std,
    a ~ dnorm(0.5, 1),
    b ~ dnorm(0, 10),
    sigma ~ dlnorm(0, 1) #--why not just use the dlnorm? p 98
  ), 
  data = d)
# It's slightly different

R2_is_bad(m7.1a)

