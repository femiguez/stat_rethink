# Gina
# try different non-linear curves for fitting to root depths
# 9/30/2020


rm(list = ls())
library(tidyverse)
library(maRsden)
library(nlraa)
library(nlme)
library(emmeans)

mrs_rootdepth


# look at it --------------------------------------------------------------

mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5) + 
  facet_grid(.~year) + 
  scale_y_reverse()

# change doy to days after planting (dap)
rd <- 
  mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  #filter(!(doy > 230 & year == 2020)) %>% 
  mutate(dop = ifelse(rootdepth_cm == 0, doy, NA)) %>% 
  fill(dop) %>% 
  mutate(dap = doy - dop) %>% 
  group_by(year) %>% 
  mutate(maxdepth = max(rootdepth_cm, na.rm = T),
         rd_sc = rootdepth_cm/maxdepth) %>% 
  filter(!is.na(rd_sc))

rd %>% 
  ggplot(aes(dap, rd_sc, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5) + 
  facet_grid(.~year) 

#--make alternative dataset that only includes max msmts

rd_max <- 
  mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  mutate(dop = ifelse(rootdepth_cm == 0, doy, NA)) %>% 
  fill(dop) %>% 
  mutate(dap = doy - dop) %>% 
  filter(!(dap > 150 & year == 2020)) %>% 
  filter(!(dap > 100 & year == 2019)) %>% 
  group_by(year) %>% 
  mutate(maxdepth = max(rootdepth_cm, na.rm = T),
         rd_sc = rootdepth_cm/maxdepth) %>% 
  filter(!is.na(rd_sc))


## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSbgrp/SSbgf (if we keep the last dip points 
## 2. SSweibull -- too many parameters?
## 3. SSlogis

#--beta-growth--

#--not converging
#fm1 <- nls(rd_sc ~ SSbgrp(dap, w.max, lt.e, ldt), data = rd)
#--this one works
fm1 <- nls(rd_sc ~ SSbgf(dap, w.max, t.e, t.m), data = rd)

#--weibull--
#fm2 <- nls(rd_sc ~ SSweibull(dap, Asym, Drop, lrc, pwr), data = rd_max)

#--logistic
fm3 <- nls(rd_sc ~ SSlogis(dap, Asym, xmid, scal), data = rd_max)



# work through certain fits -----------------------------------------------

md <- 
  rd %>% 
  select(year, dap, rd_sc, rot_trt) %>% 
  mutate(eu = paste0(year,"_", rot_trt)) %>% #--add an eu identifier
  mutate(year_id = as.factor(year),
         rot_trt = as.factor(rot_trt))

mdG <- groupedData(rd_sc ~ dap | eu, data = md)

md_max <- 
  rd_max %>% 
  select(year, dap, rd_sc, rot_trt) %>% 
  mutate(eu = paste0(year,"_", rot_trt)) %>% #--add an eu identifier
  mutate(year_id = as.factor(year),
         rot_trt = as.factor(rot_trt))

mdG_max <- groupedData(rd_sc ~ dap | eu, data = md_max)

####################### logisitic ###############################

#--fit model to each group
fmG <- nlsList(rd_sc ~ SSlogis(dap, Asym, xmid, scal), data = mdG_max) #--non-converg doesn't matter
plot(intervals(fmG))

#--a, b, and c could have random components added
#fmm1 <- nlme(fmG, random = pdDiag(a + b + c ~ 1))

#--I want the things to have random effects of year
fmm1 <- nlme(fmG, random = pdDiag(Asym + xmid + scal ~ 1))
plot(fmm1)
plot(fmm1, id = 0.01) #--eek. 

head(coef(fmm1)) #--why isn't scal different?
intervals(fmm1) #--random effect intervals are reasonable

fxf1 <- fixef(fmm1) #--we need these if we want to add an effect of rotation

#--note this an update, it keeps the random effects, we are just adding a fixed effect
fmm2 <- update(fmm1, 
               fixed = list(Asym + xmid + scal ~ rot_trt),
               start = c(fxf1[1], 0, #--Asym
                         fxf1[2], 0, #--scal
                         fxf1[3], 0)) #--rot_trt

fxf2 <- fixef(fmm2) #--now we have a value for 4 and 2yr rots

anova(fmm2) #--everything is not 0. not that informative
intervals(fmm2) #--still well constrained estimates
plot(fmm2, id = 0.01) #--outliers mean maybe we could model the residual variance better. Don't know how to do that.
plot(augPred(fmm2, level = 0:1))

#--fernando magic, tell it things are related within a year
fmm3a <- update(fmm2, random = list(year_id = pdDiag(Asym + xmid + scal ~ 1),
                                    eu = pdDiag(Asym + xmid + scal ~ 1)),
                groups = ~ year_id/eu)


## Parameter values and contrast among groups, another model
emmeans(fmm3a, ~ rot_trt, param = "Asym")
contrast(emmeans(fmm3a, ~ rot_trt, param = "Asym"), "pairwise")

emmeans(fmm3a, ~ rot_trt, param = "xmid")
contrast(emmeans(fmm3a, ~ rot_trt, param = "xmid"), "pairwise")

emmeans(fmm3a, ~ rot_trt, param = "scal")
contrast(emmeans(fmm3a, ~ rot_trt, param = "scal"), "pairwise")



####################### beta-growth ##############

#--fit model to each group
fmG <- nlsList(rd_sc ~ SSbgf(dap, w.max, t.e, t.m), data = mdG) #--non-converg doesn't matter
plot(intervals(fmG))

#--a, b, and c could have random components added
#fmm1 <- nlme(fmG, random = pdDiag(a + b + c ~ 1))

#--I want the things to have random effects of year
fmm1 <- nlme(fmG, random = pdDiag(w.max + t.e + t.m ~ 1))
plot(fmm1)
plot(fmm1, id = 0.000001) #--fernando likes things to be under 2
plot(fmm1, id = 0.01) #--eek. 

head(coef(fmm1)) #--notice xs is the same for everything
intervals(fmm1) #--random effect intervals are reasonable

fxf1 <- fixef(fmm1) #--we need these if we want to add an effect of rotation

#--note this an update, it keeps the random effects, we are just adding a fixed effect
fmm2 <- update(fmm1, 
               fixed = list(w.max + t.e + t.m ~ rot_trt),
               start = c(fxf1[1], 0, #--w.max
                         fxf1[2], 0, #--t.e
                         fxf1[3], 0)) #--t.m

fxf2 <- fixef(fmm2) #--now we have a value for 4 and 2yr rots

anova(fmm2) #--everything is not 0. not that informative
intervals(fmm2) #--still well constrained estimates
plot(fmm2, id = 0.01) #--outliers mean maybe we could model the residual variance better. Don't know how to do that.

#--this is too much to look at, not sure how to make it digestable
plot(augPred(fmm2, level = 0:1))


#--could try having the variance increase with higher values
fmm3 <- update(fmm2, weights = varPower())
plot(fmm3, id = 0.01)

#--compare them ?not working grr
par(mfrow=c(2,1))
plot(fmm3, id = 0.01, main="varPower")
plot(fmm2, id = 0.01, main="no variance modelling")

#--FEM does something I don't quite get. 
# instead of modelling the variance, try adding two random effect levels?
fmm3a <- update(fmm2, random = list(site_id = pdDiag(a + b + c ~ 1),
                                    eu = pdDiag(a + b + c ~ 1)),
                groups = ~ site_id/eu)

## Fewer outliers, this is a better model
plot(fmm3a, id = 0.001)

intervals(fmm3a) 
#--I feel like these are telling me something about between site variation and w/in site var.
#--It seems that site is absorbing more variance than the eu for a?
ranef(fmm3a)

# --interclass correlation
VarCorr(fmm3a)

tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(fmm3a)[2:4]),
       eu_var = as.numeric(VarCorr(fmm3a)[6:8]),
       res_var = as.numeric(VarCorr(fmm3a)[9])) %>% 
  mutate(tot = site_var + eu_var + res_var,#--where should I add the residual?
         pct_site = site_var/tot)

#--can I interpret this as there is more variation explained by site for the parameter a?

## Parameter values and contrast among groups
emmeans(fmm3a, ~ rotation, param = "a")
emmeans(fmm3a, ~ rotation, param = "b")
emmeans(fmm3a, ~ rotation, param = "xs")
emmeans(fmm3a, ~ rotation, param = "c")
## Contrasts
contrast(emmeans(fmm3a, ~ rotation, param = "a"), "pairwise")
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do depend on rotation: 
contrast(emmeans(fmm3a, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "c"), "pairwise")


# look at preds -----------------------------------------------------------

leachG$prds <- predict(fmm3a, level = 0)

ggplot(data = leachG, aes(x = nrate_kgha, y = prds)) + 
  geom_line(aes(color = rotation), size = 3)



fmm2.sim1 <- simulate_nlme(fmm3a, nsim = 100, psim = 1, level = 0)
leachG$mn.s <- apply(fmm2.sim1, 1, mean)
leachG$mxn.s <- apply(fmm2.sim1, 1, max)
leachG$mnn.s <- apply(fmm2.sim1, 1, min)


ggplot() + 
  #  geom_point(data = leachG, aes(x = nrate_kgha, y = leaching_kgha)) + 
  geom_ribbon(data = leachG, 
              mapping = aes(x = nrate_kgha, 
                            ymin = mxn.s, 
                            ymax = mnn.s, fill = rotation), 
              alpha = 0.5) + 
  geom_line(data = leachG, aes(x = nrate_kgha, 
                               y = prds, 
                               color = rotation), size = 2) +
  labs(y = "leaching_kgha")


