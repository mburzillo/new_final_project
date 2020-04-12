library("margins")
x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
(m <- margins(x))
summary(m)
plot(m)


m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp + 
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), data = rp)


marg1 <- margins(m1)
summary(marg1)
plot(marg1)


# 2

library("margins")
x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
(m <- margins(x, variables = "hp"))
summary(m)
plot(m)


m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp + pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  pctcollegegradinterp + biracial + nonpartisan + primary + logpop + year.f + south + midwest + west + (1 | geo_id2), data = rp)


marg1 <- margins(m1, variables = "H_citytract_multi_i")
summary(marg1)
plot(marg1)

# 3


library("margins")
x <- lm(mpg ~ cyl + hp * am + wt, data = mtcars)
(m <- margins(x, at = list(am = 0:1)))
summary(m)
plot(m)


m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp + 
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + biracial + nonpartisan + primary + logpop + year.f + 
             south + midwest + west + (1 | geo_id2), data = rp)


ten_percentile <- quantile(rp_sub$H_citytract_multi_i, c(.10), na.rm = T)
ninety_percentile <- quantile(rp_sub$H_citytract_multi_i, c(.90), na.rm = T)

marg1 <- margins(m1, at = list(H_citytract_multi_i = c(ten_percentile, ninety_percentile)))

summary(marg1)
plot(marg1)

diversityinterp = mean(diversityinterp) + mean(pctasianpopinterp) + mean(pctblkpopinterp)
mean(pctlatinopopinterp) + mean(medincinterp) + mean(pctrentersinterp) +  
mean(pctcollegegradinterp) + mean(biracial) + mean(nonpartisan) + mean(primary)
mean(logpop) + mean(year.f) + mean(south) + mean(midwest) + mean(west) 
mean(geo_id2)


marg2 <- margins(m1, at = list(H_citytract_multi_i = c(ten_percentile, ninety_percentile),
                               diversityinterp = mean(rp$diversityinterp, na.rm = T),
                               pctasianpopinterp = mean(rp$pctasianpopinterp, na.rm = T),
                               pctblkpopinterp = mean(rp$pctblkpopinterp, na.rm = T),
                               pctlatinopopinterp = mean(rp$pctlatinopopinterp, na.rm = T),
                               medincinterp = mean(rp$medincinterp, na.rm = T),
                               pctrentersinterp = mean(rp$pctrentersinterp, na.rm = T), 
                               pctcollegegradinterp = mean(rp$pctcollegegradinterp, na.rm = T),
                               biracial = mean(rp$biracial, na.rm = T),
                               nonpartisan = mean(rp$nonpartisan, na.rm = T),
                               primary = mean(rp$primary, na.rm = T),
                               logpop = mean(rp$logpop, na.rm = T),
                               south = mean(rp$south, na.rm = T),
                               midwest = mean(rp$midwest, na.rm = T),
                               west = mean(rp$west, na.rm = T) 
                               ))


library("margins")
x <- lm(mpg ~ cyl + hp * am + wt, data = mtcars)
(m <- margins(x, at = list(am = 0:1)))
summary(m)
plot(m)

#####################

sort(unique(rp$year))

rp$y1989 <- ifelse(rp$year == 1989, 1, 0)
rp$y1990 <- ifelse(rp$year == 1990, 1, 0)
rp$y1991 <- ifelse(rp$year == 1991, 1, 0)
rp$y1992 <- ifelse(rp$year == 1992, 1, 0)
rp$y1993 <- ifelse(rp$year == 1993, 1, 0)
rp$y1994 <- ifelse(rp$year == 1994, 1, 0)
rp$y1995 <- ifelse(rp$year == 1995, 1, 0)
rp$y1996 <- ifelse(rp$year == 1996, 1, 0)
rp$y1997 <- ifelse(rp$year == 1997, 1, 0)
rp$y1998 <- ifelse(rp$year == 1998, 1, 0)
rp$y1999 <- ifelse(rp$year == 1999, 1, 0)

rp$y2000 <- ifelse(rp$year == 2000, 1, 0)
rp$y2001 <- ifelse(rp$year == 2001, 1, 0)
rp$y2002 <- ifelse(rp$year == 2002, 1, 0)
rp$y2003 <- ifelse(rp$year == 2003, 1, 0)
rp$y2004 <- ifelse(rp$year == 2004, 1, 0)
rp$y2005 <- ifelse(rp$year == 2005, 1, 0)
rp$y2006 <- ifelse(rp$year == 2006, 1, 0)
rp$y2007 <- ifelse(rp$year == 2007, 1, 0)
rp$y2008 <- ifelse(rp$year == 2008, 1, 0)
rp$y2009 <- ifelse(rp$year == 2009, 1, 0)

rp %>%
  select(year, y1989) %>%
  head()

m1 <- lmer(biggestsplit ~ H_citytract_multi_i + diversityinterp + pctasianpopinterp + 
             pctblkpopinterp + pctlatinopopinterp + medincinterp + pctrentersinterp +  
             pctcollegegradinterp + biracial + nonpartisan + primary + logpop + y1989 + 
             y1990 + y1991 + y1992 + y1993 + y1994 + y1995 + y1996 + y1997 + y1998 + 
             y1999 + y2000 + y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 + 
             y2008 + y2009 + south + midwest + west + (1 | geo_id2), data = rp)

summary(m1)

ten_percentile <- quantile(rp_sub$H_citytract_multi_i, c(.10), na.rm = T)
ninety_percentile <- quantile(rp_sub$H_citytract_multi_i, c(.90), na.rm = T)

marg1 <- margins(m1, at = list(H_citytract_multi_i = c(ten_percentile, ninety_percentile)))

summary(marg1)
plot(marg1)

diversityinterp = mean(diversityinterp) + mean(pctasianpopinterp) + mean(pctblkpopinterp)
mean(pctlatinopopinterp) + mean(medincinterp) + mean(pctrentersinterp) +  
  mean(pctcollegegradinterp) + mean(biracial) + mean(nonpartisan) + mean(primary)
mean(logpop) + mean(year.f) + mean(south) + mean(midwest) + mean(west) 
mean(geo_id2)

############


marg2 <- margins(m1, at = list(H_citytract_multi_i = c(ten_percentile, ninety_percentile),
                               diversityinterp = mean(rp$diversityinterp, na.rm = T),
                               pctasianpopinterp = mean(rp$pctasianpopinterp, na.rm = T),
                               pctblkpopinterp = mean(rp$pctblkpopinterp, na.rm = T),
                               pctlatinopopinterp = mean(rp$pctlatinopopinterp, na.rm = T),
                               medincinterp = mean(rp$medincinterp, na.rm = T),
                               pctrentersinterp = mean(rp$pctrentersinterp, na.rm = T), 
                               pctcollegegradinterp = mean(rp$pctcollegegradinterp, na.rm = T),
                               biracial = mean(rp$biracial, na.rm = T),
                               nonpartisan = mean(rp$nonpartisan, na.rm = T),
                               primary = mean(rp$primary, na.rm = T),
                               logpop = mean(rp$logpop, na.rm = T)
))

rp_1 <- rp %>%
  filter(winner == 1)

mean(rp_1$logpop, na.rm = T)


summary(marg2)
