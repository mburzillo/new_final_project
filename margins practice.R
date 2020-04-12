mtcars
write.csv(mtcars, "mtcars.csv")

library(margins)
library(lme4)
library(tidyverse)

# 1: SAME

x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
(m <- margins(x))

summary(m)

plot(m)

# 2: SAME

x <- lm(mpg ~ cyl + hp * wt, data = mtcars)

margins(x, at = list(cyl = mean(mtcars$cyl, na.rm = T)))

# 3:  SAME

# stata: reg mpg cyl hp wt

# margins, dydx(*)

x <- lm(mpg ~ cyl + hp + carb, data = mtcars)

margins(x)

# 4:

# stata: reg mpg cyl hp i.carb

# margins, dydx(*)

x <- lm(mpg ~ cyl + hp + as.factor(carb), data = mtcars)

margins(x)

# 5:

# Stata: xtmixed mpg hp i.carb || cyl:
# margins, dydx(*)

mtcars$carb_factor <- as.factor(mtcars$carb)

mtcars

x <- lmer(mpg ~ hp + carb_factor + (1 | cyl), data = mtcars, REML = FALSE)

mtcars_sub <- head(mtcars, 4) 

mtcars_sub <- mtcars_sub %>%
  select(mpg, hp, carb)

x1 <- lm(mpg ~ hp + carb, data = mtcars_sub)

summary(x1)

m1 <- margins(x, type = "link")

summary(m1)

# 6:

# Stata: margins, at(hp=(66 243.5))

m <- margins(x1, at = list(hp = c(66, 243.5)))

print(m, digits = 10)

coef(m)[1]

summary(m, digits = 6)

plot(m)
plot(m1)

(66 - mean(mtcars_sub$hp)) * (-.0824)

margins_hp = -0.08235294118
low = 66
high = 243.5

mtcars_sub$predict_low = mtcars_sub$mpg + (low - mtcars_sub$hp) * (margins_hp)
mtcars_sub$predict_high = mtcars_sub$mpg + (high - mtcars_sub$hp) * (margins_hp)

mean(mtcars_sub$predict_low)
mean(mtcars_sub$predict_high)

dydx(mtcars_sub, x1, hp)

# the Stata command gives a point prediction of the dependent variable whereas
# the R command gives the average marginal effect, so to get the point
# prediction you have to multiply it by the change in the variable of interest
# and add that to the original point prediction!!!!!!!


# FINAL SUB MODEL

# Stata: xtmixed mpg wt i.carb || cyl:

# R: t4 <- lmer(mpg ~ wt + as.factor(carb) + (1 | cyl), data = mtcars, REML = FALSE)

# FINAL MODEL:





# STATA: margins, at((mean) _all H_citytract_multi_i=(.23 .54))




################################## regressions in R v. stata ###############################3


## model 1: exactly the same

# stata : reg mpg cyl hp wt

t1 <- lm(mpg ~ cyl + hp + wt, data = mtcars)

summary(t1)


## model 2: as.factor is the same thing as doing i. in stata!!!!

# stata: reg mpg i.cyl hp wt

t2 <- lm(mpg ~ as.factor(cyl) + hp + wt, data = mtcars)

summary(t2)


## model 3: THIS IS THE SAME

# stata: xtmixed mpg wt || geo_id2:

t3 <- lmer(mpg ~ wt + (1 | cyl), data = mtcars, REML = FALSE)

summary(t3)

## model 4: THIS IS THE SAME

# stata: xtmixed mpg wt || cyl:

t3 <- lmer(mpg ~ wt + (1 | cyl), data = mtcars, REML = FALSE)

summary(t3)

## model 4: THIS IS THE SAME

# stata xtmixed mpg wt i.carb || cyl:

t4 <- lmer(mpg ~ wt + as.factor(carb) + (1 | cyl), data = mtcars, REML = FALSE)

summary(t4)


# FINAL MODEL:

# xtmixed biggestsplit H_citytract_multi_i diversityinterp pctasianpopinterp
# pctblkpopinterp pctlatinopopinterp medincinterp pctrentersinterp
# pctcollegegradinterp biracial nonpartisan primary logpop i. year south midwest
# west if winner==1||geo_id2:

m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + 
             pctasianpopinterp + pctblkpopinterp + pctlatinopopinterp + 
             medincinterp + pctrentersinterp +  pctcollegegradinterp + 
             biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), 
              data = rp_1, REML=FALSE)





  
