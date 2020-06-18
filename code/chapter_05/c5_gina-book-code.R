# C-5 book code work through
# Gina

library(ggplot2)
library(dplyr)
library(rethinking)

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
    ) , data = dwaf)

