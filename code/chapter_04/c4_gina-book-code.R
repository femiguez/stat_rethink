# Gina
# work through chapter 4 code examples

library(tidyverse)
library(rethinking)

#--data on !kung
data("Howell1")
d <- Howell1

str(d)

#--histograms don't work for me
precis(d, hist = F)

#--just keep data where age is >18 (adults)

d2 <- 
  d %>% filter(age >= 18)

#--we want to model the height

# this is confusing to me
curve( dnorm(x, 178, 20), from = 100, to = 250)

# If we say the mean is 178 and sd is 20, what does the curve look like?
# note this is the prior for our height mean. It is the curve representing the MEAN height. 
tibble(x = seq(100, 250, 1)) %>% 
  mutate(ht_dens = dnorm(x, 178, 20)) %>% 
  ggplot(aes(x, ht_dens)) + 
  geom_line()

# what does the sd look like?
tibble(x = seq(-10, 60, 1)) %>% 
  mutate(ht_dens = dunif(x, 0, 50)) %>% 
  ggplot(aes(x, ht_dens)) + 
  geom_line()

#--we know what our priors look like separately. What happens when we combine them?
# ie use our priors to simulate what our data would look like
tibble(sample_mu = rnorm(n = 1e4, mean = 178, sd = 20)) %>% 
  mutate(sample_sigma = runif(n = 1e4, min = 0, max = 50),
         prior_h = rnorm(n = 1e4, mean = sample_mu, sd = sample_sigma)) %>% 
  ggplot(aes(prior_h)) + 
  geom_density()


#--we can test what would happen if we changed our priors
#--try a sigma ranging from 0 to 100 instead
tibble(sample_mu = rnorm(n = 1e4, mean = 178, sd = 20)) %>% 
  mutate(sample_sigma = runif(n = 1e4, min = 0, max = 50),
         sample_sigma2 = runif(n = 1e4, min = 0, max = 100),
         prior_h = rnorm(n = 1e4, mean = sample_mu, sd = sample_sigma),
         prior_h2 = rnorm(n = 1e4, mean = sample_mu, sd = sample_sigma2)) %>%
  pivot_longer(prior_h:prior_h2) %>% 
  ggplot(aes(value)) + 
  geom_density(aes(color = name))

# you see the blue prior (sd of height 0-100) predicts negative values for height. Yuck. 
# pick your priors carefully


# use grid approx to run model --------------------------------------------

#--I don't get this. Puke, base R. 
mu.list = seq(150, 160, length.out = 100)
sigma.list = seq(7, 9, length.out = 100)
post <- expand.grid(mu = mu.list, sigma = sigma.list) %>% as_tibble()
# I get this, he has a df of all possible combos of mu and sigma

# this is nonsense to me
post$LL <- sapply( 1:nrow(post), function (i) sum(
  dnorm (d2$height, post$mu[i], post$sigma[i], log = TRUE)))
post$prod <- post$LL + dnorm( post$mu, 178, 20, TRUE) + 
  dunif( post$sigma, 0, 50, TRUE)
post$prob <- exp( post$prod - max(post$prod))


contour_xyz( post$mu, post$sigma, post$prob )



# i will try to translate it ----------------------------------------------

expand_grid(mu = seq(150, 160, length.out = 100),
            sig = seq(7, 9, length.out = 100)) %>% 
  #slice(1:10) %>% #--for testing
  rowwise() %>% 
  mutate(LL = sum(dnorm(d2$height, mu, sig, log = TRUE)),
         prod = LL + dnorm(mu, 178, 20, log = TRUE) + dunif(sig, 0, 50, log = TRUE)) %>% 
  ungroup() %>% #--to get rid of rowwise structure
  mutate(maxprod = max(prod),
         problog = prod - maxprod,
         prob = exp(problog)) %>% 
  ggplot(aes(mu, sig)) + 
  geom_point(aes(color = prob)) + 
  scale_color_viridis_c()
  
# ok I understand the code, but not the reasoning. What does LL stand for? 
# he adds them bc they are logs, and that's the same as multiplying. 


# moving on ---------------------------------------------------------------

# positive relationship here
plot(d2$weight, d2$height)

#--4.38
# look at all the possible relationships from our priors
N <- 100
a <- rnorm ( N, 178, 20)
b <- rnorm (N, 0, 10)

plot (NULL, xlim = range(d2$weight), ylim = c(-100, 400), 
      xlab = "weight", ylab = "height")
xbar <- mean(d2$weight)
for (i in 1:N) curve (a[i] + b[i] * (x - xbar), 
                      from = min(d2$weight), 
                      to = max(d2$weight), add = T,
                      col = col.alpha("black", 0.2))
# they suck!

# rlnorm forces a parameter to be positive
b <- rlnorm(1e4, 0, 1)
dens(b, xlim = c(0, 5), adj = 0.1)


#--4.41
# restrict it to positive slopes

N <- 100
a <- rnorm ( N, 178, 20) #--why don't we have to specify this is positive? 
# There are slopes in the above graph that are positive, but with negative intercepts. 
b <- rlnorm (N, 0, 1)

plot (NULL, xlim = range(d2$weight), ylim = c(-100, 400), 
      xlab = "weight", ylab = "height")
xbar <- mean(d2$weight)
for (i in 1:N) curve (a[i] + b[i] * (x - xbar), 
                      from = min(d2$weight), 
                      to = max(d2$weight), add = T,
                      col = col.alpha("black", 0.2))

#--4.42
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[ d$age >= 18, ]

# define avg weight
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

# table of params results (hard to visualize, but good start)
precis(m4.3)

# variance/covariance of things
round (vcov (m4.3), 3)
# info about one gives us no info about the other
pairs(m4.3)

# use these posteriors against the data
plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3) #--this does the mvnorm thing
a_map <- mean(post$a) # mean intercept
b_map <- mean(post$b) # mean slope
curve(a_map + b_map*(x - xbar), add = T)
# this does a poor job of communicating uncertainty

post[1:5,]

# visualizing uncertainty in the line
# start with just first 10 lines of data
N <- 10
dN <- d2[1:N,]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
  )

post <- extract.samples( mN, n = 20)
plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2,
     xlab = "weight",
     ylab = "height")
mtext(concat("N = ", N))

for (i in 1:20)
  curve(post$a[i] + post$b[i]*(x - mean(dN$weight)),
        col = col.alpha("black", 0.3), add = T)


# repeat for more data
N <- 50
dN <- d2[1:N,]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

post <- extract.samples( mN, n = 20)
plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2,
     xlab = "weight",
     ylab = "height")
mtext(concat("N = ", N))

for (i in 1:20)
  curve(post$a[i] + post$b[i]*(x - mean(dN$weight)),
        col = col.alpha("black", 0.3), add = T)

#--4.50
# let's get 10,000 values at just weight = 50
post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50 - xbar) #--results in a vector
dens(mu_at_50)
PI(mu_at_50, prob = 0.89)

#--4.53
# link does lots of work for you
mu <- link(m4.3)
str(mu)

#--4.54
# define the weights we want uncertainty dist for height on
weight.seq <- seq( from = 25, to = 70, by = 1)
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

plot(height ~ weight, d2, type = "n")
 for (i in 1:100)
   points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

# summarise the dist of mu at each point
mu.mean <- apply (mu, 2, mean) #--the 2 refers to the 2nd dimension, or column
mu.PI <- apply(mu, 2, PI, prob = 0.89)

plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)

# Hallelujah, someone did this for me
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/linear-models.html#a-gaussian-model-of-height

<<<<<<< Updated upstream
=======
# brms and rethinking share commands. Get the data, then detach rethinking
library(tidyverse)
rm(list = ls())
library(rethinking)
data(Howell1)
d <- Howell1
rm(Howell1)
detach(package:rethinking, unload = T)
library(brms)

#--adults only
d2 <-
  d %>%
  filter(age >= 18)

ggplot(data = tibble(x = seq(from = 100, to = 250, by = .1)),
       aes(x = x, y = dnorm(x, mean = 178, sd = 20))) +
  geom_line() +
  ylab("density")

n <- 1e4
set.seed(4)
tibble(sample_mu = rnorm(n, mean = 178, sd = 20),
       sample_sigma = runif(n, min = 0, max = 50)) %>% #--smaller dist
  mutate(x = rnorm(n, mean = sample_mu, sd = sample_sigma)) %>%
  ggplot(aes(x = x)) +
  geom_density(fill = "green2", size = 0) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = expression(paste("Prior predictive distribution for ", italic(h[i]))),
       x = NULL) +
  theme(panel.grid = element_blank())

#--use grid approximation (not real, but helpful)
n <- 200
d_grid <-
  tibble(mu = seq(from = 140, to = 160, length.out = n),
         sigma = seq(from = 4, to = 9, length.out = n)) %>%
  # we'll accomplish with `tidyr::expand()` what McElreath did with base R `expand.grid()`
  expand(mu, sigma)

# define our own function (I totally did this!)
grid_function <- function(mu, sigma){
  dnorm(d2$height, mean = mu, sd = sigma, log = T) %>%
    sum()
}


#--ok, yeah this is much slower than mcelreath's code....
# d_grid <-
#   d_grid %>% #--our my and sigma combos
#   mutate(log_likelihood = map2(mu, sigma, grid_function)) %>%
#   unnest() %>%
#   mutate(
#     prior_mu = dnorm(mu, mean = 178, sd = 20, log = T),
#     prior_sigma = dunif(
#       sigma,
#       min = 0,
#       max = 50,
#       log = T
#     )
#   ) %>%
#   mutate(product = log_likelihood + prior_mu + prior_sigma) %>%
#   mutate(probability = exp(product - max(product)))

# sampling from the posterior
# set.seed(4)
# d_grid_samples <-
# d_grid %>%
#   sample_n(size = 1e4, replace = T, weight = probability)
# d_grid_samples %>%
#   ggplot(aes(x = mu, y = sigma)) +
#   geom_point(size = .9, alpha = 1/15) +
#   scale_fill_viridis_c() +
#   labs(x = expression(mu[samples]),
#        y = expression(sigma[samples])) +
#   theme(panel.grid = element_blank())


options(mc.cores = parallel::detectCores())

#--this notation is definitely not as straight forward as the quap

b4.1 <-
  brm(data = d2, 
      family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 31000, warmup = 30000, chains = 4, #cores = 4, #--why do they have this?
      seed = 4)


# never mind. back to the book. -------------------------------------------


>>>>>>> Stashed changes
