## Analysis of corn root depth data
##
## Authors: Gina Nichols and Fernando Miguez
## Date: Oct 12th 2020
## Updated: Oct 17th 2020

library(tidyverse)
library(maRsden)
library(nlraa)
library(brms)
library(tidybayes)
library(patchwork)
library(nlme)
library(emmeans)

## Analyze Ordonez data first
ro <- read_csv("gina-proj/ordonez-figdata.csv")

ro %>% 
  ggplot(aes(dap, rootdepth_cm)) + 
  geom_point(size = 3, alpha = 0.5) +
  labs(title= "Data From Ordonez et al. 2018",
       x = "Days After Planting",
       y = "Root Depth (cm)")

## Subset and fit nonlinear model
ro_max <- ro %>% 
  filter(rootdepth_cm <= max(rootdepth_cm))

fm2a <- nls(rootdepth_cm ~ SSlogis(dap, Asym, xmid, scal), data = ro_max)
confint(fm2a)

plot(fm2a)

## How to think about priors
plot(density(rnorm(50, 120, 20)))
plot(density(rlnorm(2e3, log(120), 0.5)))
plot(density(rexp(2e3, log(120))))

## How do we fit a nonlinear mixed model using a frequentist approach?
rd <- 
  mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>%
  arrange(year, doy, plot_id) %>% 
  mutate(dop = ifelse(rootdepth_cm == 0, doy, NA)) %>% 
  fill(dop) %>% 
  mutate(dap = doy - dop) 

fig_full <- 
  rd %>% 
  ggplot(aes(dap, rootdepth_cm)) + 
  geom_point(size = 3, alpha = 0.5, color = "blue") + 
  facet_grid(.~year) + 
  labs(title = "Full dataset, beta-growth candidate")

print(fig_full)

rd_max <- 
  rd %>% 
  filter(!(dap > 125 & year == 2020)) %>% 
  filter(!(dap > 100 & year == 2019)) 

fig_max <- 
  rd_max %>% 
  ggplot(aes(dap, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5, color = "red") + 
  facet_grid(.~year)  + 
  labs(title = "Trimmed dataset, logistic candidate")

print(fig_max)

## fig_full / fig_max

## Set up NLME
rd_max$year.f <- as.factor(rd_max$year)
rd_max$rotation <- as.factor(rd_max$rot_trt)
rd_max$rootdepth_m <- rd_max$rootdepth_cm * 1e-2
rd_max$plot_id <- as.factor(rd_max$plot_id)

## Typically we only have one observation per experimental unit
## it is no big deal but NLME sort of assumes this
rd_maxG <- groupedData(rootdepth_m ~ dap | plot_id, data = na.omit(rd_max))

plot(rd_maxG)

## Fit model to each individual curve
fitL <- nlsLMList(rootdepth_m ~ SSlogis(dap, Asym, xmid, scal), data = rd_maxG)

plot(fitL)

fm0 <- nlme(fitL, random = pdDiag(Asym + xmid + scal ~ 1))

plot(fm0)

## Incorporate the effect of rotation
fxf <- fixef(fm0)
## Update model
fm1 <- update(fm0, fixed = Asym + xmid + scal ~ rotation,
              start = c(fxf[1], 0, fxf[2], 0, fxf[3], 0))

anova(fm1) ## Not much of an effect?

fm2 <- update(fm1, random = list(year.f = pdDiag(Asym + xmid + scal ~ 1),
                                 plot_id = pdDiag(Asym + xmid + scal ~ 1)),
              groups = ~year.f/plot_id)

anova(fm2)
## The year accounts for most of the variability
## We do not need this effect at the level of plot for xmid and scal
fm2
## Simpler model
fm3 <- update(fm2, random = list(year.f = pdDiag(Asym + xmid + scal ~ 1),
                                 plot_id = pdDiag(Asym ~ 1)),
              groups = ~year.f/plot_id)

## Comparing models
anova(fm2, fm3)
## We can keep the simpler model
## Do we need to model the variance?
fm4 <- update(fm3, weights = varPower())
## Compare models again
anova(fm3, fm4)
## Yes, this is a better model
plot(fm4)

## Rotation does not have much of an effect on scal
anova(fm4)

## fm5 <- update(fm3, weights = varFixed(~dap))

## Visualize at the population level
sim0 <- simulate_nlme(fm4, level = 0, nsim = 1e3)

rd_maxG$prd0 <- apply(sim0, 1, median)
rd_maxG$lwr0 <- apply(sim0, 1, quantile, probs = 0.05)
rd_maxG$upr0 <- apply(sim0, 1, quantile, probs = 0.95)

ggplot(rd_maxG, aes(x = dap, y = rootdepth_m, color = rotation)) + 
  geom_point() + 
  geom_line(aes(y = prd0)) + 
  geom_ribbon(aes(ymin = lwr0, ymax = upr0, fill = rotation), alpha = 0.3)

## Test for asymptote
emmeans(fm4, ~rotation, param = "Asym")
contrast(emmeans(fm4, ~rotation, param = "Asym"), "pairwise")
plot(emmeans(fm4, ~rotation, param = "Asym"))

emmeans(fm4, ~rotation, param = "xmid")
contrast(emmeans(fm4, ~rotation, param = "xmid"), "pairwise")
plot(emmeans(fm4, ~rotation, param = "xmid"))

## Simulation for an individual year
sim1 <- simulate_nlme(fm4, level = 1, nsim = 1e3)
## sim1 <- simulate_nlme(fm4, level = 1, nsim = 1e3, value = "data.frame")

rd_maxG$prd1 <- apply(sim1, 1, median)
rd_maxG$lwr1 <- apply(sim1, 1, quantile, probs = 0.05)
rd_maxG$upr1 <- apply(sim1, 1, quantile, probs = 0.95)

ggplot(rd_maxG, aes(x = dap, y = rootdepth_m, color = rotation)) + 
  facet_wrap(~ year.f) + 
  geom_point() + 
  geom_line(aes(y = prd1)) + 
  geom_ribbon(aes(ymin = lwr1, ymax = upr1, fill = rotation), alpha = 0.3)

## The reality is that 2018 is messy

## How does the student-t distribution compare to the exponential?
## This is an example for sigma
rstd1 <- abs(rstudent_t(1e4, 100, 0, 30))
rexp1 <- rexp(1e4, 1/30)
c(max(rstd1), max(rexp1))
ggplot() + 
  geom_density(aes(x = rstd1, color = "Student-t")) + 
  geom_density(aes(x = rexp1, color = "Exponential"))

## Setting up priors for brms
priors <- prior(lognormal(0.1, 0.3), nlpar = "Asym", coef = "Intercept") + 
  prior(normal(0, 0.5), nlpar = "Asym", coef = "rotation4y") + 
  prior(normal(50, 20), nlpar = "xmid", coef = "Intercept") +
  prior(normal(0, 5), nlpar = "xmid", coef = "rotation4y") + 
  prior(normal(10, 5), nlpar = "scal", coef = "Intercept") +
  prior(normal(0, 1.5), nlpar = "scal", coef = "rotation4y") + 
  prior(student_t(10, 0, 1), nlpar = "Asym", coef = "Intercept", class = "sd", group = "year.f") + 
  prior(student_t(10, 0, 10), nlpar = "xmid", coef = "Intercept", class = "sd", group = "year.f") + 
  prior(student_t(10, 0, 4), nlpar = "scal", coef = "Intercept", class = "sd", group = "year.f") + 
  prior(student_t(30, 0, 0.15), nlpar = "Asym", coef = "Intercept", class = "sd", group = "year.f:plot_id") +
  prior(student_t(100, 0, 0.25), class = "sigma")

## Some of this is based on the insight from the NLME
bf1 <- bf(rootdepth_m ~ Asym / (1 + exp((xmid - dap)/scal)),
          Asym ~ rotation + (1 | year.f/plot_id),
          xmid + scal ~ rotation + (1 | year.f),
          nl = TRUE)

brm1 <- brm(bf1, data = rd_maxG, seed = 98, prior = priors,
            iter = 4000, cores = 3, chains = 3,
            control = list(adapt_delta = 0.99))

## Plots look good
plot(brm1, "^b_Asym")
plot(brm1, "^b_xmid")
plot(brm1, "^b_scal")
plot(brm1, "^sd_")
plot(brm1, "sigma")
pairs(brm1, "^b_")

plot(conditional_effects(brm1), points = TRUE)
plot(conditional_effects(brm1, effects = "rotation"), points = TRUE)

## Compare parameters
## nlme fm3 vs. brm1
round(fixef(fm3), 3)
round(fixef(brm1), 3)

## Let's look at predictions
ndat <- expand.grid(rotation = unique(rd_maxG$rotation), 
                    dap = seq(0, 85, length.out = 50))

pp <- predict(brm1, newdata = ndat, re_formula = NA, probs = c(0.05, 0.95))

ndat2 <- cbind(ndat, pp)

ggplot(data = ndat2, aes(x = dap, y = Estimate, color = rotation)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = Q5, ymax = Q95, fill = rot_trt), alpha = 0.2)

## My attempt to reproduce model fm4 has failed so far
## Sounds like we really need to model sigma
bf2 <- bf(rootdepth_cm ~ eta, nl = TRUE) + 
       nlf(eta ~ Asym / (1 + exp((xmid - dap)/scal))) + 
       lf(Asym ~ rotation + (1 | year.f/plot_id)) + 
       lf(xmid + scal ~ rotation + (1 | year.f)) + 
       nlf(sigma ~ sqrt(tau2 * eta^(2 * theta))) + 
       lf(tau2 ~ 1) + 
       lf(theta ~ 1)

priors2 <- prior(normal(80, 30), nlpar = "Asym", coef = "Intercept") + 
  prior(normal(0, 15), nlpar = "Asym", coef = "rotation4y") + 
  prior(normal(50, 20), nlpar = "xmid", coef = "Intercept") +
  prior(normal(0, 20), nlpar = "xmid", coef = "rotation4y") + 
  prior(normal(10, 5), nlpar = "scal", coef = "Intercept") +
  prior(normal(0, 7), nlpar = "scal", coef = "rotation4y") + 
  prior(student_t(3, 0, 30), nlpar = "Asym", coef = "Intercept", class = "sd", group = "year.f") + 
  prior(student_t(3, 0, 11), nlpar = "xmid", coef = "Intercept", class = "sd", group = "year.f") + 
  prior(student_t(3, 0, 4), nlpar = "scal", coef = "Intercept", class = "sd", group = "year.f") + 
  prior(student_t(10, 0, 5), nlpar = "Asym", coef = "Intercept", class = "sd", group = "year.f:plot_id") +
  prior(uniform(0.01, 1), nlpar = "tau2", lb = 0.01, ub = 1) + 
  prior(uniform(0, 5), nlpar = "theta", lb = 0, ub = 5)

brm2 <- brm(bf2, data = rd_maxG, seed = 9876, prior = priors2,
            iter = 3000, cores = 2, chains = 2,
            control = list(adapt_delta = 0.8))

plot(brm2, "^b_tau")
plot(brm2, "^b_theta")


###
## Looking at residuals
fttd <- fitted(fm4)
stds <- attr(fm4$residuals, "std")
theta <- fm4$modelStruct$varStruct[[1]]
stds2 <- sqrt(sigma(fm4)^2 * fttd ^ (2 * theta))
plot(stds, stds2)
abline(0,1)

