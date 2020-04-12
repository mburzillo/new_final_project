library(tidyverse)
library(lme4)
library(reprex)
library(plm)

# load data from plm package

data("EmplUK", package="plm")

# multi-level mixed effects regression with random effects for firm


m4 <- lmer(wage ~ as.factor(year) + sector + emp + capital + 
             output + (1 | firm), data = EmplUK)

# summarize

summary(m4)


# write to csv for use in stata

write.csv(EmplUK, file = "empUK.csv" )