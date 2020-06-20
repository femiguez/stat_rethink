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

#--ugh this isn't working. can I ggplot2-ize it?
plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:50) lines(c(-2), mu_plaus[i], col = col.alpha("black", 0.4))


#--why can't I just use the prior I extracted? Why do I need to use the link function?
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