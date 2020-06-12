## Problem 3M6
## This problem is about power analysis, so in the 
## frequentist framework, there are formulas and 
## packages that can easily do this. I bet there are pacakges
## in R that can also do this for the Bayesian framework, but have not looked into this
##
## I will use simulation to develop an intuitive understanding of this problem
library(rethinking)

# 3M6

## R code 3.2
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## In this case the interval is
PI(samples, prob = 0.99)
## interval width
(int.width1 <- max(PI(samples, prob = 0.99)) - min(PI(samples, prob = 0.99)))

## What if our sample size was 10 times as large?
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 60 , size=90 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## In this case the interval is
PI(samples, prob = 0.99)
## interval width
(int.width2 <- max(PI(samples, prob = 0.99)) - min(PI(samples, prob = 0.99)))

## I will now try 100 times larger
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 600 , size=900 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## In this case the interval is
PI(samples, prob = 0.99)
## interval width
(int.width3 <- max(PI(samples, prob = 0.99)) - min(PI(samples, prob = 0.99)))

## I will now try 250 times larger
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6*250 , size=9*250 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## In this case the interval is
PI(samples, prob = 0.99)
## interval width
(int.width4 <- max(PI(samples, prob = 0.99)) - min(PI(samples, prob = 0.99)))

## What about 1000 times larger?
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6000 , size=9000 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## In this case the interval is
PI(samples, prob = 0.99)
## interval width
(int.width5 <- max(PI(samples, prob = 0.99)) - min(PI(samples, prob = 0.99)))

## Let's make a graph
ss <- c(9, 90, 900, 9 * 250, 9 * 1e3) ## sample size
iw <- c(int.width1, int.width2, int.width3, int.width4, int.width5) ## Interval width

plot(ss, iw, xlab = "Sample Size", ylab = "99% Interval width", type = "o")
abline(h = 0.05, lty = 2)

## The answer I get is that we would need to toss the globe approximately 250 times
## more than we originally did, which is 9 * 250 = 2250. 
## (A little less maybe but a power analysis is an approximate answer that makes
## many assumptions, so we can't expect to be exact).

## Note: the solutions to exercises for the instructor does not contain an answer for this one
## so this entirely my answer. An alternative answer would be to perform a power analysis
## (many pacakges and resources for this), but the advantage here is that hopefully we develop 
## an intuitive understanding
