library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(DHARMa)

all.sp = read.csv('01_process_data/output//veg_all_predictors.csv') %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  mutate(asp.e = sin(pi * asp2 / 180),
         asp.n = cos(pi * asp2 / 180),
         c.slope = slope2 - mean(slope2),
         c.seast = seas_start_wd - mean(seas_start_wd))

##### Create training and testing data.

dece.train = all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
dece.valid = all.sp %>% filter(species %in% 'DECE' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

komy.train = all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
komy.valid = all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

gero.train = all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
gero.valid = all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

##### Fit frequentist models with temperature data

### Deschampsia

## Null model

dece.null = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + c.slope + (1 | plot) + 
                                              (1 | year) + (1 | obsno),
                  data = dece.train,
                  family = 'binomial')

summary(dece.null)

d.null.sr = simulateResiduals(dece.null, 250, seed = 123)

plot(d.null.sr)

# Test residuals.

plotResiduals(d.null.sr, dece.train$jja_mean1)
plotResiduals(d.null.sr, dece.train$jja_mean2)
plotResiduals(d.null.sr, dece.train$jja_mean3)
# maybe evidence of jja_mean3 being a good predictor

plotResiduals(d.null.sr, dece.train$season.gdd1)
plotResiduals(d.null.sr, dece.train$season.gdd2)
plotResiduals(d.null.sr, dece.train$season.gdd3)
# also possibly season gdd3 but not a great fit

plotResiduals(d.null.sr, dece.train$seas_start_wd)
# why is this being coded as a factor...?
# this actually looks marginally good...

dece.jja3 = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + c.slope + jja_mean3 +
                                              (1 | plot) + (1 | year) + (1 | obsno),
                  data = dece.train,
                  family = 'binomial')

summary(dece.jja3)
# n.s.
logLik(dece.jja3)

dece.gdd3 = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + c.slope + season.gdd3 +
                                              (1 | plot) + (1 | year) + (1 | obsno),
                  data = dece.train,
                  family = 'binomial')

summary(dece.gdd3)
# n.s.
logLik(dece.gdd3)

dece.seas = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + c.slope + c.seast +
                                              (1 | plot) + (1 | year) + (1 | obsno),
                  data = dece.train,
                  family = 'binomial')

summary(dece.seas)
# nada
logLik(dece.seas)

## Look at random intercepts

dece.train %>%
  select(c(year, jja_mean1, jja_mean2, jja_mean3, season.gdd1, 
           season.gdd2, season.gdd3, seas_start_wd)) %>%
  gather(key = metric, val = measure, -year) %>%
  merge(y = data.frame(year = .$year %>% unique() %>% sort(),
                       rint = ranef(dece.null)$year %>% unlist()),
        by = 'year') %>%
  ggplot(aes(x = rint, y = measure)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ metric, scales = 'free_y')
# none of these are especially promising
# maybe jja_mean1, jja_mean3 maybe


### Kobresia models

komy.null = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + (1 | plot) + (1 | year) + (1 | obsno),
                  data = komy.train,
                  family = 'binomial')

summary(komy.null)
logLik(komy.null)

k.null.sr = simulateResiduals(komy.null, 250, seed = 123)

plot(k.null.sr)

# Test residuals.

plotResiduals(k.null.sr, komy.train$jja_mean1)
plotResiduals(k.null.sr, komy.train$jja_mean2)
plotResiduals(k.null.sr, komy.train$jja_mean3)
# some funny stuff with skedasticity in here but nothing promisng

# high correlation probably means nothing interestng here
plotResiduals(k.null.sr, komy.train$season.gdd1)
plotResiduals(k.null.sr, komy.train$season.gdd2)
plotResiduals(k.null.sr, komy.train$season.gdd3)
# also no sign of this being compelling

plotResiduals(k.null.sr, komy.train$seas_start_wd)
# maybe this one?

komy.seas = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + seas_start_wd + (1 | plot) + (1 | year) + (1 | obsno),
                  data = komy.train,
                  family = 'binomial')

summary(komy.seas)
# ugh
logLik(komy.seas)
# nearly identical to null

komy.train %>%
  select(c(year, jja_mean1, jja_mean2, jja_mean3, season.gdd1, 
           season.gdd2, season.gdd3, seas_start_wd)) %>%
  gather(key = metric, val = measure, -year) %>%
  merge(y = data.frame(year = .$year %>% unique() %>% sort(),
                       rint = ranef(komy.null)$year %>% unlist()),
        by = 'year') %>%
  ggplot(aes(x = rint, y = measure)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ metric, scales = 'free_y')

# none of these look good at all

### Geum

## Null model

gero.null = glmer(cbind(n.obs, 100 - n.obs) ~ (1 | plot) + (1 | year) + (1 | obsno),
                  data = gero.train,
                  family = 'binomial')

summary(gero.null)
logLik(gero.null)

g.null.sr = simulateResiduals(gero.null, 250, seed = 123)

plot(g.null.sr)

# Test residuals.

plotResiduals(g.null.sr, gero.train$jja_mean1)
plotResiduals(g.null.sr, gero.train$jja_mean2)
plotResiduals(g.null.sr, gero.train$jja_mean3)
# these are shifted but not trending
# maybe try jja mean3

plotResiduals(g.null.sr, gero.train$season.gdd1)
plotResiduals(g.null.sr, gero.train$season.gdd2)
plotResiduals(g.null.sr, gero.train$season.gdd3)
# results are more or less identical

plotResiduals(g.null.sr, gero.train$seas_start_wd)
# not compelling

gero.jja3 = glmer(cbind(n.obs, 100 - n.obs) ~ jja_mean3 + (1 | plot) + (1 | year) + (1 | obsno),
                  data = gero.train,
                  family = 'binomial')

summary(gero.jja3)
# ooh...
logLik(gero.jja3)
# log likelihood reduction is very small.

gero.seas = glmer(cbind(n.obs, 100 - n.obs) ~ c.seast + (1 | plot) + (1 | year) + (1 | obsno),
                  data = gero.train,
                  family = 'binomial')

summary(gero.seas)
# eh
logLik(gero.seas)
# log likelihood reduction is very small.
# identical log likelhood

# Plot year random effects against weather stuff 

gero.train %>%
  select(c(year, jja_mean1, jja_mean2, jja_mean3, season.gdd1, 
           season.gdd2, season.gdd3, seas_start_wd)) %>%
  gather(key = metric, val = measure, -year) %>%
  merge(y = data.frame(year = .$year %>% unique() %>% sort(),
                       rint = ranef(gero.null)$year %>% unlist()),
        by = 'year') %>%
  ggplot(aes(x = rint, y = measure)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ metric, scales = 'free_y')

# try: jja_mean3

##### What we learned here

# All of these temperature variables are likely all pretty shitty at predicting this stuff.

