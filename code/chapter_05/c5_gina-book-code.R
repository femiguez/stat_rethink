# C-5 book code work through
# Gina

library(ggplot2)
library(dplyr)
library(tidyr)
library(rethinking)
library(tibble)

data("WaffleDivorce")
dwafraw <- WaffleDivorce

# standardize variabes (using rethinking function)

dwaf <- 
  dwafraw %>% 
  mutate(div_rate = standardize(Divorce),
         marr_rate = standardize(Marriage),
         marr_age = standardize(MedianAgeMarriage))

#--let's think carefully about the priors of our linear model. 

# we standardized things, so the intercept should be close to 0. 
# a ~ normal(0, 0.2) is a tight distribution
rnorm(1000, 0, 0.2) %>% 
  as_data_frame() %>% 
  mutate(value2 = rnorm(1000, 0, 1)) %>% 
  ggplot(aes(value)) +
  geom_density() + 
  geom_density(aes(value2), color = "red") + 
  labs(title = "difference btwn norm sd of 1 and 0.2")

# you can read a normal distribution as


# a slope of 1 would indicate a 1sd inc in marr_rate would = a 1sd inc in med age. What is the sd of median age?

sd(dwaf$MedianAgeMarriage) #--1.2 years

# so an increase 1.2 years of med marriage age means a 1 sd inc in divorce rate. Seems extreme. 
# He cuts that in half. 

# bi ~ normal(0, 0.5) means 5% of slopes are more extreme than 1. What?

dnorm(x = 1, mean = 0, sd = 0.5) #--this seems to say it's 11% ? What am I doing wrong here?
dnorm(1, 0, 1)

# Hmmm. What he's saying is 95% of the data lies within 2stds of the mean. 
# So a sd of 0.5 means 95% of the slopes are less than 1. 

# when things are standardized, exp(1) for sigma is a safe bet. 

m5.1 <- 
  quap(
    alist(
      div_rate ~ dnorm (mu, sigma),
    mu <- a + bA * marr_age,
    a ~ dnorm (0, 0.2),
    bA ~ dnorm (0, 0.5),
    sigma ~ dexp(1)
    ) , 
    data = dwaf)

#--let's see what we've created

m5.1_prior <- extract.prior(m5.1) #--note we get a list, with a vector for a bA and sigma (1000 of them)
#--I think here he is just trying to get...I don't know. Why is he doing this. 
mu_plaus <- link(m5.1, post = m5.1_prior, data = list(marr_age = c(-2, 0, 2))) #--he's just doing it for age = -2 and 2 (stds from mean)

#--what are the two ouputs? 
mu_plaus[1:5,]
# each row is a sample from the posterior dist
# each column is a row in the data. So we have two columns because we fed it two rows of data
# so in the below code, we are just feeding it values from the first row (?)

#--ok, so
plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:50) lines(c(-2, 0, 2), mu_plaus[i,], col = col.alpha("black", 0.4))
# he's creating a line through x values of -2, 0, and 2, and their corresponding y values in the ith row of mu_plau

#--why can't I just use the prior I extracted? Why do I need to use the link function?
#--the link function gives us the mu values...ugh. 
p1 <- 
  ggplot(data = dwaf[1:50 , ],
       aes(x = marr_age, div_rate)) +
  geom_point() +
  #--use the extracted prior here
  geom_abline(intercept = m5.1_prior$a[1:100],
              slope = m5.1_prior$bA[1:100],
              size = 1/3, alpha = .3) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  labs(title = "bA ~ dnorm(0, 0.5)")

#--let's try it with different priors

m5.1a <- 
  quap(
    alist(
      div_rate ~ dnorm (mu, sigma),
      mu <- a + bA * marr_age,
      a ~ dnorm (0, 0.2),
      bA ~ dnorm (0, 1),
      sigma ~ dexp(1)
    ) , 
    data = dwaf)

m5.1a_prior <- extract.prior(m5.1a)

p2 <- 
  ggplot(data = dwaf[1:50 , ],
       aes(x = marr_age, div_rate)) +
  geom_point() +
  #--use the extracted prior here
  geom_abline(intercept = m5.1a_prior$a[1:100],
              slope = m5.1a_prior$bA[1:100],
              size = 1/3, alpha = .3) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  labs(title = "bA ~ dnorm(0, 1)")

#-smaller
m5.1b <- 
  quap(
    alist(
      div_rate ~ dnorm (mu, sigma),
      mu <- a + bA * marr_age,
      a ~ dnorm (0, 0.2),
      bA ~ dnorm (0, 0.2),
      sigma ~ dexp(1)
    ) , 
    data = dwaf)

m5.1b_prior <- extract.prior(m5.1b)


p3 <- 
  ggplot(data = dwaf[1:50 , ],
         aes(x = marr_age, div_rate)) +
  geom_point() +
  #--use the extracted prior here
  geom_abline(intercept = m5.1b_prior$a[1:100],
              slope = m5.1b_prior$bA[1:100],
              size = 1/3, alpha = .3) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  labs(title = "bA ~ dnorm(0, 0.2)")


library(patchwork)
p1 + p2 + p3
#--notice that by restricting the slope so much, we also restricted the intercept


#--look at posterior predictions
A_seq <- seq(from = -3, to = 3.2, length.out = 30) #--here he is refining the x-scale he is 'linking'
my_mu <- link(m5.1, data = list(marr_age = A_seq))
my_mu_mean <- apply(my_mu, 2, mean) #--take the mean of each column, we have a mu for each value of x we fed it
mu.PI <- apply(my_mu, 2, PI) #--get the PI of each column
head(dwaf)
plot(div_rate ~ marr_age, data = dwaf, col = rangi2)
lines( A_seq, my_mu_mean, lwd = 2)
shade(mu.PI, A_seq)
precis(m5.1) #--bA is reliably negative


#--do same thing on marriage rate
m5.2 <- quap(
  alist(
  div_rate ~ dnorm(mu, sigma),
  mu <- a + bM * marr_rate,
  a ~ dnorm(0, 0.2),
  bM ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
  ),
  data = dwaf)

my_mu2 <- link(m5.2, data = list(marr_rate = A_seq))
my_mu_mean2 <- apply(my_mu2, 2, mean) #--take the mean of each column, we have a mu for each value of x we fed it
mu.PI2 <- apply(my_mu2, 2, PI) #--get the PI of each column
plot(div_rate ~ marr_rate, data = dwaf, col = rangi2)
lines( A_seq, my_mu_mean2, lwd = 2)
shade(mu.PI2, A_seq)
precis(m5.2) #--bM is reliably positive

#--but we can't just compare parameters
# they could provide independent value
# could be redundant (not likely given the different slopes)
# one could counteract the other


# create a mental model ---------------------------------------------------

m5.3 <- quap(
  alist(
    div_rate ~ dnorm(mu, sigma),
    mu <- a + bM * marr_rate + bA * marr_age,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dwaf)
precis(m5.3)

plot(coeftab(m5.3))


#--simulate what you think is happening
# NOTE: This didn't quite work for me...
dwaf_sim <- 
  tibble(age = rnorm(50),
         mar = rnorm(50, -age),
         div = rnorm(50, age)) #--this is copied from the book and I think it's wrong?

m5.3sim <- quap(
  alist(
    div ~ dnorm(mu, sigma),
    mu <- a + bM * mar + bA * age,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dwaf_sim)

plot(coeftab(m5.3sim, m5.3))


# HOw to plot multivariate posteriors -------------------------------------


#--fit a model with only the one predictor
m5.4 <- quap(
  alist(
    div_rate ~ dnorm(mu, sigma),
    mu <- a + bA * marr_age,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dwaf)

#--this is confusing to me
mu <- link(m5.4)
mu_mean <- apply(mu, 2, mean) #--take mean of each column
mu_resid <- dwaf$marr_rate - mu_mean

#--ohhhhh the actual response variable is never considered. I guess
# plot one predictor against the other
# the residuals are the distance of the y-axis from teh abline (?)
dwaf %>% 
  ggplot(aes(marr_age, marr_rate)) + 
  geom_point() + 
  geom_abline(slope = -1)

# see if the size of the resids has a pattern with response variable (div rate)
dwaf %>% 
  mutate(marr_rate_resid = marr_rate - mu_mean) %>% 
  ggplot(aes(marr_rate_resid, div_rate)) + 
  geom_point() + 
  labs(title = "div rate vs marr rate resids")

#--fit a model with only the one predictor
m5.5 <- quap(
  alist(
    div_rate ~ dnorm(mu, sigma),
    mu <- a + bB * marr_rate,
    a ~ dnorm(0, 0.2),
    bB ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dwaf)

#--this is confusing to me
mu <- link(m5.5)
mu_mean <- apply(mu, 2, mean) #--take mean of each column

# see if the size of the resids has a pattern with response variable (div rate)
dwaf %>% 
  mutate(age_resid = marr_age - mu_mean) %>% 
  ggplot(aes(age_resid, div_rate)) + 
  geom_point() + 
  labs(title = "div rate vs age resids")


# counter factual plots ---------------------------------------------------

#--making a simpler data frame for some reason. Why now? No idea. 
# why is it a list? and not a dataframe? also no idea
data("WaffleDivorce")
d <- list()
d$A <- standardize(WaffleDivorce$MedianAgeMarriage)
d$D <- standardize(WaffleDivorce$Divorce)
d$M <- standardize(WaffleDivorce$Marriage)
d

#--there is something wrong here. 
# I don't get the same results as him. 
m5.3_A <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm(mu, sigma),
    my <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1),
    ## A -> M (marriage rate is also affected by age)
    M ~ dnorm(mu_M, sigma_M),
    mu_M <- aM + bAM*A,
    aM ~ dnorm(0, 0.2),
    bAM ~ dnorm(0, 0.5),
    sigma_M ~ dexp(1)
  ), 
  data = d)

precis(m5.3_A)
plot(coeftab(m5.3_A))
#--this shows no effect of A on D. ?

#--see what would happen if we manipulate A
# define list of 30 imaginary interventions. 
A_seq <- seq(from = -2, to = 2, length.out = 30)

#--simulate data, note the order matters
sim_dat <- data.frame(A = A_seq)
s <- sim( m5.3_A, data = sim_dat, vars = c("M", "D")) #--simulate M, then D, this says

#--his code
plot(sim_dat$A, colMeans(s$D), ylim = c(-2, 2), type = "l")
shade(apply(s$D, 2, PI), sim_dat$A)

#--ggplot it (sort of)
sim_dat2 <- 
  sim_dat %>% 
  mutate(
    simD = colMeans(s$D),
         loCID = apply(s$D, 2, PI)[1,],
         hiCID = apply(s$D, 2, PI)[2,],
    simM = colMeans(s$M),
    loCIM = apply(s$M, 2, PI)[1,],
    hiCIM = apply(s$M, 2, PI)[2,]
    ) 

sim_dat2 %>%  
  ggplot(aes(A, simD)) + 
  geom_line() +
  geom_ribbon(aes(ymin = loCID, ymax = hiCID), alpha = 0.2) +
  coord_cartesian(ylim = c(-2, 2)) + 
  labs(title = "Confused")

sim_dat2 %>%  
  ggplot(aes(A, simM)) + 
  geom_line() +
  geom_ribbon(aes(ymin = loCIM, ymax = hiCIM), alpha = 0.2) +
  coord_cartesian(ylim = c(-2, 2)) + 
  labs(title = "Confused")

#--well apparently age has no effect, but if it did we could calculate it

sim2_dat <- data.frame(A = (c(20, 30) - 26.1)/1.24)
s2 <- sim(m5.3_A, data = sim2_dat, vars = c("M", "D"))
mean(s2$D[,2] - s2$D[,1]) #--yeah basically no effect



# masked variables --------------------------------------------------------
library(dplyr)
library(tidyr)
library(rethinking)

data("milk")
milk <- 
  milk %>% 
  select(kcal.per.g, neocortex.perc, mass) %>% 
  mutate(K = standardize(kcal.per.g),
         N = standardize(neocortex.perc),
         M = standardize(log(mass))
  ) %>% 
  #--drops all rows w/NAs
  drop_na()

#--example of regressing one predictor on K
m5.6 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = milk)

precis(m5.6)
plot(coeftab(m5.6))


#--add both predictors
m5.7 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bM*M + bN*N,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = milk)

precis(m5.7)
plot(coeftab(m5.7))

#--make a counterfactual plot
xseq <- seq(from = min(milk$M)-0.15, to = max(milk$M) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(M = xseq, N = 0)) #--ummm, doing it at mean N?
#--we didn't have to use sim this time? only link? so confused. 

data.frame(M = xseq,N = 0) %>% 
  mutate(mu_mean = apply(mu, 2, mean),
         loPI = apply(mu, 2, PI)[1,],
         hiPI = apply(mu, 2, PI)[2,]) %>% 
  ggplot(aes(M, mu_mean)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = loPI, 
                  ymax = hiPI), alpha = 0.2) + 
  labs(title = "Counterfactual holding N = 0")


#--try holding M = 0
xseq <- seq(from = min(milk$N)-0.15, to = max(milk$N) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(N = xseq, M = 0))

data.frame(N = xseq, M = 0) %>% 
  mutate(mu_mean = apply(mu, 2, mean),
         loPI = apply(mu, 2, PI)[1,],
         hiPI = apply(mu, 2, PI)[2,]) %>% 
  ggplot(aes(N, mu_mean)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = loPI, 
                  ymax = hiPI), alpha = 0.2) + 
  labs(title = "Counterfactual holding M = 0")
 # NOTE: I think there is a typo on page 152
# another typo on p 156?


# categorical vars --------------------------------------------------------

data(milk)
d <- 
  milk %>% 
  mutate(clade_id = as.integer(clade),
         K = standardize(kcal.per.g))

m5.9 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d)

#--I can't get it out of his format
precis(m5.9, depth = 2) %>%
  as_tibble() 

labels <- paste("a[", 1:4, "]:", levels(d$clade), sep = "")
plot(precis(m5.9, depth = 2, pars = "a"), labels = labels,
     xlab = "expected kcal")

set.seed(63)
d$house <- sample(rep(1:4, each = 8), size = nrow(d))

m5.10 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a[clade_id] + h[house],
    a[clade_id] ~ dnorm(0, 0.5),
    h[house] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d)

plot(precis(m5.10, depth = 2, pars = "a"),
     xlab = "expected kcal")
