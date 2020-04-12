library(haven)
library(tidyverse)
library(lme4)
library(reprex)

# load data

rp <- read_dta("/Users/mariaburzillo/Desktop/GOV1006/final_project/racial_polarization_winners.dta")


rp_sub <- rp %>%
  select(biggestsplit, H_citytract_multi_i, diversityinterp, pctasianpopinterp, 
         pctblkpopinterp, pctlatinopopinterp, medincinterp, pctrentersinterp,
         pctcollegegradinterp, biracial, nonpartisan, primary, logpop, year, 
         south, midwest, west, geo_id2, winner)

sub <- head(rp_sub, 15)


data <- tibble(
  biggestsplit = c(0.01600002, NA, NA, 0.18000001, 0.26999998, 0.43570000, 
                    0.52999997, 0.52990001, 0.50259995, 0.62000000, 0.35999998,
                    0.48999998, NA, NA, NA),
  H_citytract_multi_i = c(0.2547167, 0.2495996, 0.2418626, 0.1848550, 0.3819615, 0.3691695, 
                           0.3691695, 0.3548566, 0.3402055, 0.3402055, 0.3245400,
                           0.3245400, 0.3088745, 0.2584054, 0.2584054),
  diversityinterp = sub$diversityinterp,
  pctasianpopinterp = sub$pctasianpopinterp, 
  pctblkpopinterp = sub$pctblkpopinterp, 
  pctlatinopopinterp = sub$pctlatinopopinterp, 
  medincinterp = sub$medincinterp, 
  pctrentersinterp = sub$pctrentersinterp,
  pctcollegegradinterp = sub$pctcollegegradinterp, 
  year = sub$year, 
  south = sub$south, 
  midwest = sub$midwest, 
  west = sub$midwest, 
  geo_id2 = sub$geo_id2,
  winner= sub$winner
)

data

biracial, sub$biracial , 
nonpartisan, 
primary, 
logpop, 

# create factor variable for year

data$year.f <- as.factor(data$year)

# apply condition that winner == 1 as in Stata regression

data <- data %>%
  filter(winner == 1)

# multi-level mixed effects regression with random effects for geo_id2

m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp + 
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + year.f + 
             south + midwest + west + (1 | geo_id2), data = data)


# summarize

summary(m1)

write.csv(data, file = "to_test_rp.csv")


class(gapminder$year)

as.factor(gapminder$year)

m2 <- lmer (gdpPercap ~ lifeExp + pop + as.factor(year) + (1 | country), data = gapminder)

summary(m2)

write.csv(gapminder, file = "gapminder.csv")

data("Grunfeld", package="plm")
head(Grunfeld)
Grunfeld


m3 <- lmer(value ~ inv + year + capital + (1 | firm), data = Grunfeld)
summary(m3)


write.csv(Grunfeld, file = "Grunfeld.csv")

data("EmplUK", package="plm")

data("Produc", package="plm")

data("Wages", package="plm")


m4 <- lmer(wage ~ as.factor(year) + sector + emp + capital + 
              output + (1 | firm), data = EmplUK)

summary(m4)

write.csv(EmplUK, file = "empUK.csv" )
