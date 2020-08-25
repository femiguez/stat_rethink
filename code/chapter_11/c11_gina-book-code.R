# working thru c-11 book code
# aug 17 2020

library(rethinking)
library(tidyverse)
theme_set(theme_bw())


data("chimpanzees")
d <- chimpanzees %>% 
  mutate(trt = 1 + prosoc_left + 2*condition,
         treatment = trt)

xtabs(~trt + prosoc_left + condition, d)

#--pulled left as function of something
m11.1 <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ) , data = d )

#--sample from the prior
prior <- extract.prior(m11.1, n = 1e4) %>% 
  as.data.frame() %>% 
  mutate(a_logit = a,
         a_prob = inv_logit(a_logit))


#--pulled left as function of something, better prior
m11.1b <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 1.5)
  ) , data = d )

prior2 <- extract.prior(m11.1b, n = 1e4)

#--on logit scale
prior %>% 
  mutate(b_logit = prior2$a,
         b_prob = inv_logit(b_logit)) %>% 
  ggplot(aes(x = a_logit)) + 
  geom_density() + 
  geom_density(aes(x = b_logit), color = "red")
  
#--on prob scale
prior %>% 
  mutate(b_logit = prior2$a,
         b_prob = inv_logit(b_logit)) %>% 
  ggplot(aes(x = a_prob)) + 
  geom_density() + 
  geom_density(aes(x = b_prob), color = "red")


# now look at slope prior -------------------------------------------------

m11.2 <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[trt],
    a ~ dnorm(0, 1.5),
    b[trt] ~ dnorm(0, 10) 
  ), data = d)

prior11.2 <- extract.prior(m11.2, n = 1e4)  

thing <- 
  prior11.2 %>% 
  as.data.frame() %>% 
  #head() %>% 
  pivot_longer(b.1:b.4) %>% 
  mutate(prob = inv_logit(a + value),
         grp = rep(1:10000, each = 4)) %>% 
  filter(name %in% c("b.1", "b.2")) %>% 
  select(grp, name, prob) %>% 
  pivot_wider(names_from = name, values_from = prob) %>% 
  mutate(diff = abs(b.1 - b.2))

thing %>% 
  ggplot(aes(x=diff)) +
  geom_density()
  

m11.2b <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[trt],
    a ~ dnorm(0, 1.5),
    b[trt] ~ dnorm(0, 0.5) 
  ), data = d)

prior11.2b <- extract.prior(m11.2b, n = 1e4)  



prior11.2b %>% 
  as.data.frame() %>% 
  #head() %>% 
  pivot_longer(b.1:b.4) %>% 
  mutate(prob = inv_logit(a + value),
         grp = rep(1:10000, each = 4)) %>% 
  filter(name %in% c("b.1", "b.2")) %>% 
  select(grp, name, prob) %>% 
  pivot_wider(names_from = name, values_from = prob) %>% 
  mutate(diff2 = abs(b.1 - b.2)) %>% 
  select(grp, diff2) %>% 
  left_join(thing) %>% 
  ggplot(aes(x=diff)) +
  geom_density() + 
  geom_density(aes(x = diff2), color = "red")


#--what is average different?
prior11.2b %>% 
  as.data.frame() %>% 
  #head() %>% 
  pivot_longer(b.1:b.4) %>% 
  mutate(prob = inv_logit(a + value),
         grp = rep(1:10000, each = 4)) %>% 
  filter(name %in% c("b.1", "b.2")) %>% 
  select(grp, name, prob) %>% 
  pivot_wider(names_from = name, values_from = prob) %>% 
  mutate(diff2 = abs(b.1 - b.2)) %>% 
  summarise(mn = mean(diff2))

#--good, we want our priors to be skeptical of large diffs


# use hamiltonian mc ------------------------------------------------------

dat_list <- 
  list(
    pulled_left = d$pulled_left,
    actor = d$actor,
    treatment = as.integer(d$trt) )

dat_list

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5) 
  ), data = dat_list, chains = 4, log_lik = T )

precis( m11.4, depth = 2)

post11.4 <- extract.samples(m11.4)
p_left <- inv_logit(post11.4$a)
#--just shows some chimps have handed preferences
plot(precis(as.data.frame(p_left)), xlim = c(0,1))


#--look at preds and real data
obs <- 
  d %>% 
  select(pulled_left, actor, trt) %>% 
  group_by(actor, trt) %>% 
  summarise(mn = mean(pulled_left)) %>% 
  mutate(partner = ifelse(trt %in% c(3, 4), "partner", "nopartner"),
         food = ifelse(trt %in% c(1, 3), "nofood", "food"))

obs %>% 
  ggplot(aes(partner, mn, group = food)) + 
  geom_point(aes(pch = food)) + 
  geom_line(aes(color = food)) + 
  facet_grid(.~actor)

#--get posterior preds using link
p_post <- link(m11.4, data = d)
p_post %>% 
  as.data.frame %>% 
  as_tibble()

p_mu <- apply(p_post, 2, mean)
p_ci <- apply(p_post, 2, PI)

#--I don't know what to do with theses....
obs %>% 
  mutate(mu = p_mu,
         cilo = p_ci[1,],
         cihi = p_ci[2,]) %>% 
  as_tibble() %>% 
  ggplot(aes(partner, mu, group = food)) + 
  geom_point(aes(pch = food)) + 
  geom_linerange(aes(ymin = cilo, ymax = cihi, color = food)) +
  facet_grid(.~actor)
