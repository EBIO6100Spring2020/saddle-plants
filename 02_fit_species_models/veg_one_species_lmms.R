# Script for setting up models looking at veg abundance
# Initially starting with all three sp. in the same script.
# february 2020 - 
# scott

# Load packages
library(rstanarm)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

# First, a crude function for calculating RMSE

rmse = function(x, xpred) sqrt(sum((x - xpred)^2) / length(x))

# Now, loading in data.

# Vegetation data
veg.focs = read.csv('01_process_data/output/veg_focals_top_sans96.csv')

# Spatial data (going to take out slope and aspect for now)
spatial.data = read.csv('00_raw_data/spatial_physical_data/Point_attributes.csv')

head(veg.focs)

with(veg.focs, table(USDA_code, year))

# Now, set aside year 2015 - 2017 as testing data, 2018 as mega-test
# This means 1989 - 2014 are training data

veg.train = veg.focs %>% filter(year %in% 1989:2014)
veg.valid = veg.focs %>% filter(year %in% 2015:2017)

# If we're going to run any binomial models, we'll need points sampled
pts.all = read.csv('01_process_data/output/veg_points_sampled_all.csv')

head(pts.all)
nrow(pts.all)
# 132K lol

pts.per = pts.all %>%
  group_by(year, plot) %>%
  summarise(n.samples = n())

table(pts.per$n.samples)
# All points are sampled 100 times! Woo. No incomplete sampling.

# As a first pass... pick Deschampsia (just because)

dece.train = veg.train %>% filter(USDA_code %in% 'DECE')
dece.valid = veg.valid %>% filter(USDA_code %in% 'DECE')

nrow(dece.train)
nrow(dece.valid)

# Wait! Shoot. If we want binomial or presence/absence,
# we need to have zeros.
# Should export sampling design from OG dataset.

# Feels bad to say that a plant is "missing" though if
# it isn't the top hit though no?

dece.pts.present = dece.train %>%
  group_by(year, plot) %>%
  summarise(n.obs = n()) %>%
  rbind(pts.per %>% filter(year %in% 1989:2014) %>% select(year, plot) %>% mutate(n.obs = 0)) %>%
  group_by(year, plot) %>%
  summarise(n.obs = sum(n.obs)) %>%
  ungroup() %>%
  merge(y = spatial.data %>% select(SDL_GRDP, Slope, Aspect),
        by.x = 'plot', by.y = 'SDL_GRDP') %>%
  filter(!is.na(Aspect))


head(dece.pts.present)
table(dece.pts.present$n.obs)

dece.pts.present.val = dece.valid %>%
  group_by(year, plot) %>%
  summarise(n.obs = n()) %>%
  rbind(pts.per %>% filter(year %in% 2015:2017) %>% select(year, plot) %>% mutate(n.obs = 0)) %>%
  group_by(year, plot) %>%
  summarise(n.obs = sum(n.obs)) %>%
  ungroup()  %>%
  merge(y = spatial.data %>% select(SDL_GRDP, Slope, Aspect),
                   by.x = 'plot', by.y = 'SDL_GRDP') %>%
  filter(!is.na(Aspect))

# A super null model
# Just glm, no random effects
dece.null = glm(cbind(n.obs, 100 - n.obs) ~ 1, 
                family = 'binomial',
                data = dece.pts.present)

summary(dece.null)
fitted.values(dece.null)[1]
# This model predicts that there will be Deschampsia present in 7 points per
# quadrat.

rmse.dece.null = rmse(x = dece.pts.present.val$n.obs, 
                      xpred = 100 * predict(dece.null, 
                                            newdata = dece.pts.present.val,
                                            type = 'response'))

rmse.dece.null
# 20.30057

# A model featuring a random effect of site
# THIS is a better null model from a statistical standpoint
# it includes a plot-level variable
# (because samples are equal how different is this from fixed effect?)
dece.rf.null = glmer(cbind(n.obs, 100 - n.obs) ~ 1 + (1 | plot),
                     family = 'binomial',
                     data = dece.pts.present)

summary(dece.rf.null)
fitted.values(dece.rf.null) %>%
  unique() %>%
  hist()
# cool
# got a nice spread there as expected

rmse.dece.rf.null = rmse(x = dece.pts.present.val$n.obs,
                         xpred = 100 * predict(dece.rf.null, 
                                               newdata = dece.pts.present.val,
                                               type = 'response'))
rmse.dece.rf.null
# 9.68749

# Huge reduction in RMSE.
# Continue with this random effect.

# A model with slope and plot-level random effects
dece.slp = glmer(cbind(n.obs, 100 - n.obs) ~ Slope + (1 | plot),
                 family = 'binomial',
                 data = dece.pts.present)
summary(dece.slp)

rmse.dece.slp = rmse(x = dece.pts.present.val$n.obs,
                     xpred = 100 * predict(dece.slp, 
                                           newdata = dece.pts.present.val,
                                           type = 'response'))
rmse.dece.slp
# 9.687705
# Not very helpful.
# Little evidence that slope is helpful.



##### Try with another species

gero.train = veg.train %>% filter(USDA_code %in% 'GEROT')
gero.valid = veg.valid %>% filter(USDA_code %in% 'GEROT')

gero.pts.present = gero.train %>%
  group_by(year, plot) %>%
  summarise(n.obs = n()) %>%
  rbind(pts.per %>% filter(year %in% 1989:2014) %>% select(year, plot) %>% mutate(n.obs = 0)) %>%
  group_by(year, plot) %>%
  summarise(n.obs = sum(n.obs)) %>%
  ungroup() %>%
  merge(y = spatial.data %>% select(SDL_GRDP, Slope, Aspect),
        by.x = 'plot', by.y = 'SDL_GRDP') %>%
  filter(!is.na(Aspect))

gero.pts.present.val = gero.valid %>%
  group_by(year, plot) %>%
  summarise(n.obs = n()) %>%
  rbind(pts.per %>% filter(year %in% 2015:2017) %>% select(year, plot) %>% mutate(n.obs = 0)) %>%
  group_by(year, plot) %>%
  summarise(n.obs = sum(n.obs)) %>%
  ungroup()  %>%
  merge(y = spatial.data %>% select(SDL_GRDP, Slope, Aspect),
        by.x = 'plot', by.y = 'SDL_GRDP') %>%
  filter(!is.na(Aspect))


# Null GEROT model

gero.null = glm(cbind(n.obs, 100 - n.obs) ~ 1,
                family = 'binomial',
                data = gero.pts.present)
fitted.values(gero.null)[1]

rmse.gero.null = rmse(x = gero.pts.present.val$n.obs,
                      xpred = 100 * predict(gero.null, 
                                            gero.pts.present.val,
                                            type = 'response'))
rmse.gero.null
# 16.457

# A null model for GEROT with a random effect for plot
gero.rf.null = glmer(cbind(n.obs, 100 - n.obs) ~ 1 + (1 | plot),
                     family = 'binomial',
                     data = gero.pts.present)

rmse.gero.rf.null = rmse(x = gero.pts.present.val$n.obs,
                         xpred = 100 * predict(gero.rf.null,
                                               gero.pts.present.val,
                                               type = 'response'))
rmse.gero.rf.null
# 4.7764

# Now, try a model with slope.

gero.slp = glmer(cbind(n.obs, 100 - n.obs) ~ Slope + (1 | plot),
                 family = 'binomial',
                 data = gero.pts.present)
summary(gero.slp)
# n.b. slope not informative.

rmse.gero.slp = rmse(x = gero.pts.present.val$n.obs,
                     xpred = 100 * predict(gero.slp,
                                           gero.pts.present.val,
                                           type = 'response'))
rmse.gero.slp
# 4.7764
# model is basically identical to a model without slope