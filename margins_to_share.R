library(margins)
library(tidyverse)

# use the included mtcars dataset

# run a simple regression

x1 <- lm(mpg ~ hp + carb, data = mtcars)

# use the margins command to get the output

m <- margins(x1, at = list(hp = c(66, 243.5)))

# print results to 10 digits

print(m, digits = 10)

# manually save the AME on hp (is there a way to pull this directly from m?)

margins_hp = -0.07290395619

# manually create a new variable for the point prediction given the high/low value for hp

mtcars$predict_low = mtcars$mpg + (66 - mtcars$hp) * (margins_hp)
mtcars$predict_high = mtcars$mpg + (243.5 - mtcars$hp) * (margins_hp)

# find the mean point prediction given the high/low value for hp

(mean(mtcars$predict_low))
(mean(mtcars$predict_high))

# the Stata command gives a point prediction of the dependent variable whereas
# the R command gives the average marginal effect, so to get the point
# prediction you have to multiply it by the change in the variable of interest
# and add that to the original point prediction!!!!!!!


# Stata: margins, at(hp=(66 243.5))