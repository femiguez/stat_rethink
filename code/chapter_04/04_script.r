## Script for chapter 4
## 
## Author: Fernando Miguez
library(rethinking)

## Just following the book
data("Howell1")
d <- Howell1
precis(d)
d2 <- d[d$age >= 18, ]

## R code 4.16
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

## R code 4.17
contour_xyz( post$mu , post$sigma , post$prob )

## R code 4.18
image_xyz( post$mu , post$sigma , post$prob )

## R code 4.19
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

## R code 4.20
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

## R code 4.21
dens( sample.mu )
dens( sample.sigma )

## R code 4.27
flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

## R code 4.28
m4.1 <- quap( flist , data=d2 )

## R code 4.29
precis( m4.1 )


## set up xbar
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
m4.3
## Classic way
fit.lm <- lm(height ~ I(weight - xbar), data = d2)
c(coef(fit.lm), sigma(fit.lm))

## quadp
precis(m4.3)

## code
plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )

### Fernando's way using bootstrap
### Important: bootstrap is not Bayesian, but it it the closest
### thing within the frequentist framework

fit.lm2 <- lm(height ~ weight, data = d2)

## Note: predict.lm can also produce these quantities
## It does not generate samples though, it uses the assumption
## of normality and it derives equations

## Simulation for the mean
fit.lm2.simd <- simulate_lm(fit.lm2, nsim = 1e2, value = "data.frame")
## Simulation for an observation
fit.lm2.simd2 <- simulate_lm(fit.lm2, psim = 2, nsim = 1e3, value = "data.frame")

upr <- aggregate(sim.y ~ weight, FUN = function(x) quantile(x, probs = 0.95),
                 data = fit.lm2.simd2)
names(upr) <- c("weight","upr")
lwr <- aggregate(sim.y ~ weight, FUN = function(x) quantile(x, probs = 0.05),
                 data = fit.lm2.simd2)
names(lwr) <- c("weight","lwr")
bands <- merge(upr, lwr)

ggplot() + 
  geom_line(data = fit.lm2.simd,
            aes(x = weight, y = sim.y, group = ii),
            color = "red", alpha = 0.5) + 
  geom_ribbon(data = bands, aes(x = weight, ymin = lwr, ymax = upr), 
              fill = "purple", alpha = 0.3) + 
  geom_abline(intercept = coef(fit.lm2)[1], slope = coef(fit.lm2)[2], 
              color = "blue") + 
  geom_point(data = d2, aes(x = weight, y = height)) + 
  xlab("Weight (kg)") + ylab("Height (cm)") + 
  ggtitle("Height vs. weight, fitted (blue), mean band (red) \n and prediction band (purple)")

### One possible equation is the Hill3
fit1 <- nls(height ~ SShill3(weight, Ka, n, a), data = Howell1)
## Weibull
fit2 <- nls(height ~ SSweibull(weight, Asym, D, lrc, k), data = Howell1)
## Gompertz
fit3 <- nls(height ~ SSgompertz(weight, Asym, b, c), data = Howell1)
## Bilinear
fit4 <- nls(height ~ SSblin(weight, a, b, c, d), data = Howell1)
## NRH
fit5 <- nls(height ~ SSnrh(weight, a, b, c, d), data = Howell1)

## Nonlinear fit to all of the data
ggplot(data = Howell1, aes(x = weight, y = height)) + geom_point()

## Visualize
ggplot(data = Howell1, aes(x = weight, y = height)) + 
  geom_point() + geom_line(aes(y = fitted(fit1)), color = "red") + 
  ggtitle("Hill3 equation")

## Visualize
ggplot(data = Howell1, aes(x = weight, y = height)) + 
  geom_point() + geom_line(aes(y = fitted(fit2)), color = "red") + 
  ggtitle("Weibull equation")

## Visualize
ggplot(data = Howell1, aes(x = weight, y = height)) + 
  geom_point() + geom_line(aes(y = fitted(fit3)), color = "red") + 
  ggtitle("Gompertz equation")

## Visualize
ggplot(data = Howell1, aes(x = weight, y = height)) + 
  geom_point() + geom_line(aes(y = fitted(fit4)), color = "red") + 
  ggtitle("Bi-linear equation")

## Visualize
ggplot(data = Howell1, aes(x = weight, y = height)) + 
  geom_point() + geom_line(aes(y = fitted(fit5)), color = "red") + 
  ggtitle("NRH equation")

