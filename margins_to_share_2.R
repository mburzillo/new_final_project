library(margins)
library(tidyverse)
library(permutations)
library(broom)
library(rstanarm)
library(lme4)
library(sjPlot)

mtcars_sub <- mtcars %>%
  select(mpg, hp, carb, gear, wt, drat, cyl)

############# This shows that setting everything manually to the means does the
############# same thing as at_means in Stata

# Stata: margins, at(hp=(66 243.5))

x1 <- lm(mpg ~ hp + carb + gear + wt + drat + cyl, data = mtcars_sub)

m <- margins(x1, at = list(hp = c(66, 243.5),
                           carb = mean(mtcars_sub$carb, na.rm = T),
                           gear = mean(mtcars_sub$gear, na.rm = T),
                           wt = mean(mtcars_sub$wt, na.rm = T),
                           drat = mean(mtcars_sub$drat, na.rm = T),
                           cyl = mean(mtcars_sub$cyl, na.rm = T)
                           ))

print(m, digits = 10)
view(tidy(m))

margins_hp = -0.01581327874
low = 66
high = 243.5

mtcars_sub$predict_low = mtcars_sub$mpg + (low - mtcars_sub$hp) * (margins_hp)
mtcars_sub$predict_high = mtcars_sub$mpg + (high - mtcars_sub$hp) * (margins_hp)

mean(mtcars_sub$predict_low)
mean(mtcars_sub$predict_high)


new_data = data.frame(
  hp = 66,
  carb = mean(mtcars_sub$carb, na.rm = T),
  gear = mean(mtcars_sub$gear, na.rm = T),
  wt = mean(mtcars_sub$wt, na.rm = T),
  drat = mean(mtcars_sub$drat, na.rm = T),
  cyl = 8
)

predict(x1, new_data)

mtcars_sub %>%
  group_by(cyl) %>%
  summarise(n = n())

# the Stata command gives a point prediction of the dependent variable whereas
# the R command gives the average marginal effect, so to get the point
# prediction you have to multiply it by the change in the variable of interest
# and add that to the original point prediction!!!!!!!



########### HOW DO WE DEAL WITH FACTOR???????????

# I checked and the models are the same in R and Stata #

mtcars_sub$cyl_f <- as.factor(mtcars$cyl)

mtcars_2 <- mtcars_sub[3:7,]

x1 <- lm(mpg ~ hp + cyl_f, data = mtcars_2)
summary(x1)


write.csv(mtcars_2, "mtcars2.csv")


m <- margins(x1, at = list(hp = c(66, 243.5)))

print(m, digits = 10)
view(tidy(m))

margins_hp = -0.05918781726
low = 66
high = 243.5

mtcars_sub$predict_low = mtcars_sub$mpg + (low - mtcars_sub$hp) * (margins_hp)
mtcars_sub$predict_high = mtcars_sub$mpg + (high - mtcars_sub$hp) * (margins_hp)

mean(mtcars_sub$predict_low)
mean(mtcars_sub$predict_high)

new_data_high <- data.frame(
  hp = c(66, 66, 66),
  cyl_f = c("4", "6", "8")
)

new_data_low <- data.frame(
  hp = c(243.5, 243.5, 243.5),
  cyl_f = c("4", "6", "8")

)


########### WE CAN CALCULATE THE PREDICTED EFFECT BY FINDING THE PREDICTED
########### EFFECT FOR EACH FACTOR VALUE AND THEN TAKING A WEIGHTED AVERAGE OF
########### THEIR PREVALENCE BY FACTOR

predict(x1, new_data_high)
predict(x1, new_data_low)

.2 * 24.39807 + .4 * 22.20629  + .4 * 25.02305

####### WORK IN EXCEL SHOWS THAT TO DEAL WITH A FACTOR, STATA ADDS THE
####### COEFFICIENT WEIGHTED BY ITS PREVALENCE IN THE MODEL TIMES THE VARIABLE


mtcars_sub$cyl_f <- as.factor(mtcars$cyl)
mtcars_sub$carb_f <- as.factor(mtcars$carb)

mtcars_2 <- mtcars_sub[3:7,]

x1 <- lm(mpg ~ hp + drat + cyl_f, data = mtcars_2)
summary(x1)

write.csv(mtcars_2, "mtcars2.csv")


m <- margins(x1, at = list(hp = c(66, 243.5),
                           drat = mean(mtcars_2$drat)))

print(m, digits = 10)

margins_hp = -0.05918781726
low = 66
high = 243.5

mtcars_sub$predict_low = mtcars_sub$mpg + (low - mtcars_sub$hp) * (margins_hp)
mtcars_sub$predict_high = mtcars_sub$mpg + (high - mtcars_sub$hp) * (margins_hp)

mean(mtcars_sub$predict_low)
mean(mtcars_sub$predict_high)



# xtmixed mpg hp i.carb || cyl:
# margins, at((mean) hp=(66 243.5))

# R model

mnew <- lmer(mpg ~ hp + carb_f + (1 | cyl), data = mtcars_2, REML = FALSE)

m3 <- lmer(biggestsplit ~ H_citytract_NHW_i + diversityinterp + pctasianpopinterp + pctblkpopinterp + 
             pctlatinopopinterp + medincinterp + pctrentersinterp +  pctcollegegradinterp + biracial + 
             nonpartisan + primary + logpop + whiteideology_fill2 + year.f + south + midwest + west + 
             (1 | geo_id2), data = rp_1, REML=FALSE)

new_data_high <- data.frame(
  hp = 66,
  carb_f = mtcars_2$carb_f,
  cyl = mtcars_2$cyl
)

new_data_low <- data.frame(
  hp = 243.5,
  carb_f = mtcars_2$carb_f,
  cyl = mtcars_2$cyl
)

############### ALSO THE WEIGHTED AVERAGE FOR THE FACTORS USING LMER

predict(mnew, new_data_high)

3/5 * 25.86638 + 1/5 *33.86004 + 1/5 *39.19585

H_citytract_NHW_i + diversityinterp + pctasianpopinterp + pctblkpopinterp + 
  pctlatinopopinterp + medincinterp + pctrentersinterp +  pctcollegegradinterp + biracial + 
  nonpartisan + primary + logpop + whiteideology_fill2 + year.f + south + 
  midwest + west

rp_1
m_H_citytract_NHW_i <- mean(rp_1$H_citytract_NHW_i, na.rm = T)



m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp +
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), data = rp_1, REML=FALSE)

# means 
m_diversityinterp <- mean(rp_1$diversityinterp, na.rm = T)
m_pctasianpopinterp <- mean(rp_1$pctasianpopinterp, na.rm = T)
m_pctblkpopinterp <- mean(rp$pctblkpopinterp, na.rm = T)
m_pctlatinopopinterp <- mean(rp$pctlatinopopinterp, na.rm = T)
m_medincinterp <- mean(rp_1$medincinterp, na.rm = T)
m_pctrentersinterp <- mean(rp_1$pctrentersinterp, na.rm = T)
m_pctcollegegradinterp <- mean(rp_1$pctcollegegradinterp, na.rm = T)
m_biracial <- mean(rp_1$biracial, na.rm = T)
m_nonpartisan <- mean(rp_1$nonpartisan, na.rm = T)
m_primary <- mean(rp_1$primary, na.rm = T)
m_logpop <- mean(rp_1$logpop, na.rm = T)
m_whiteideology_fill2 <- mean(rp_1$whiteideology_fill2, na.rm = T)
m_south <- mean(rp_1$south, na.rm = T)
m_midwest <- mean(rp_1$midwest, na.rm = T)
m_west <- mean(rp_1$west, na.rm = T)

ten_percentile <- quantile(rp_1$H_citytract_multi_i, c(.10), na.rm = T)
ninety_percentile <- quantile(rp_1$H_citytract_multi_i, c(.90), na.rm = T)
  

length(unique(rp_1$year.f))

y = 1989

paste("new_dataset_low_", y, sep = "")

years = c("1999", "2003")

x = 0
for (i in 1:5 ) {
  print(x)
  x = x + 1 
}

m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp +
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), data = rp_1, REML=FALSE)

predictions <- rep(NA, nrow(rp_1))
y = min(rp_1$year)
n = unique(rp_1$year.f) + 1
for (i in 1:n) {
  df <- data.frame(
    H_citytract_multi_i = rep(ten_percentile, 21),
    diversityinterp = rep(m_diversityinterp, 21),
    pctasianpopinterp = rep(m_pctasianpopinterp, 21),
    pctblkpopinterp = rep(m_pctblkpopinterp, 21),
    pctlatinopopinterp = rep(m_pctlatinopopinterp, 21),
    medincinterp = m_medincinterp,
    pctrentersinterp = m_pctrentersinterp,
    pctcollegegradinterp = m_pctcollegegradinterp,
    biracial = m_biracial, 
    nonpartisan = m_nonpartisan,
    primary = m_primary,
    logpop = m_logpop,
    south = m_south,
    midwest = m_midwest, 
    west = m_west, 
    year.f = y,
    geo_id2 = rp_1$geo_id2
  )
  paste("year in df:", df %>% select(year.f) %>% head(1))
  low_df <- df %>% mutate(year.f = as.factor(year.f))
  print(low_df$year.f)
  predictions[y-1989 + 1] <- mean(predict(m1, low_df, allow.new.levels = TRUE, re.form = NULL))
  #paste("predicted value:", mean(predict(m1, low_df, allow.new.levels = TRUE)))
  y = y + 1
  paste("new year:", y)
}



predictions

simulate(m1, )
predictions <- rep(NA, nrow(rp_1))

df <- data.frame(
  H_citytract_multi_i = .23,
  diversityinterp = m_diversityinterp,
  pctasianpopinterp = m_pctasianpopinterp,
  pctblkpopinterp = m_pctblkpopinterp,
  pctlatinopopinterp = m_pctlatinopopinterp,
  medincinterp = m_medincinterp,
  pctrentersinterp = m_pctrentersinterp,
  pctcollegegradinterp = m_pctcollegegradinterp,
  biracial = m_biracial, 
  nonpartisan = m_nonpartisan,
  primary = m_primary,
  logpop = m_logpop,
  south = m_south,
  midwest = m_midwest, 
  west = m_west, 
  year.f = as.factor(1989),
  geo_id2 = rp_1$geo_id2
)

predictions <- predict(m1, df, allow.new.levels = TRUE)


df3 <- data.frame(
  H_citytract_multi_i = .23,
  diversityinterp = m_diversityinterp,
  pctasianpopinterp = m_pctasianpopinterp,
  pctblkpopinterp = m_pctblkpopinterp,
  pctlatinopopinterp = m_pctlatinopopinterp,
  medincinterp = m_medincinterp,
  pctrentersinterp = m_pctrentersinterp,
  pctcollegegradinterp = m_pctcollegegradinterp,
  biracial = m_biracial, 
  nonpartisan = m_nonpartisan,
  primary = m_primary,
  logpop = m_logpop,
  south = m_south,
  midwest = m_midwest, 
  west = m_west, 
  year.f = as.factor(1990),
  geo_id2 = rp_1$geo_id2
)

predictions <- predict(m1, df3, allow.new.levels = TRUE)

levels(df3$year.f)

levels(rp_1$year.f)





rp_1 %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  view()


paste("year in df:", df %>% select(year.f) %>% head(1))
low_df <- df %>% mutate(year.f = as.factor(year.f))
predictions <- predict(m1, low_df, allow.new.levels = TRUE)






rp_2 <- rp_1 %>%
  filter(!(is.na(biggestsplit)))


m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp +
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), data = rp_2, REML=FALSE)

view(rp_1 %>%
       select(biggestsplit))


cplot(m1, "H_citytract_multi_i", what = effect)
plot_model(m1, type = "pred", terms = "H_citytract_multi_i")
  

plot_model()

predictions <- rep(NA, 21)

y = 1990
df <- data.frame(
  H_citytract_multi_i = .23,
  diversityinterp = m_diversityinterp,
  pctasianpopinterp = m_pctasianpopinterp,
  pctblkpopinterp = m_pctblkpopinterp,
  pctlatinopopinterp = m_pctlatinopopinterp,
  medincinterp = m_medincinterp,
  pctrentersinterp = m_pctrentersinterp,
  pctcollegegradinterp = m_pctcollegegradinterp,
  biracial = m_biracial, 
  nonpartisan = m_nonpartisan,
  primary = m_primary,
  logpop = m_logpop,
  south = m_south,
  midwest = m_midwest, 
  west = m_west, 
  year.f = 1990,
  geo_id2 = rp_2$geo_id2
)

low_df <- df %>% mutate(year.f = as.factor(year.f))
predictions<- mean(predict(m1, low_df,allow.new.levels = TRUE))
predictions

df.p <- expand.grid(
  H_citytract_multi_i = .23,
  diversityinterp = m_diversityinterp,
  pctasianpopinterp = m_pctasianpopinterp,
  pctblkpopinterp = m_pctblkpopinterp,
  pctlatinopopinterp = m_pctlatinopopinterp,
  medincinterp = m_medincinterp,
  pctrentersinterp = m_pctrentersinterp,
  pctcollegegradinterp = m_pctcollegegradinterp,
  biracial = m_biracial, 
  nonpartisan = m_nonpartisan,
  primary = m_primary,
  logpop = m_logpop,
  south = m_south,
  midwest = m_midwest, 
  west = m_west, 
  year.f = as.factor(2010),
  geo_id2 = rp_1$geo_id2
  
)

# are the values not included in the regression and so not in the data that's being used in the model???

predict(m1, df.p, allow.new.levels = TRUE)


rp_1 %>%
  filter(year.f == "1990") %>%
  view()


new_dataset_high_i <- data.frame(
  H_citytract_NHW_i = ninety_percentile,
  diversityinterp = m_diversityinterp,
  pctasianpopinterp = m_pctasianpopinterp,
  pctblkpopinterp = m_pctblkpopinterp,
  pctlatinopopinterp = m_pctlatinopopinterp,
  medincinterp = m_medincinterp,
  pctrentersinterp = m_pctrentersinterp,
  pctcollegegradinterp = m_pctcollegegradinterp,
  biracial = m_biracial, 
  nonpartisan = m_nonpartisan,
  primary = m_primary,
  logpop = m_logpop,
  whiteideology_fill2 = m_whiteideology_fill2,
  south = m_south,
  midwest = m_midwest, 
  west = m_west 
)



