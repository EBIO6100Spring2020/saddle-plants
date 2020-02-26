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

veg.focs = read.csv('01_process_data/output/veg_focals_top_sans96.csv')

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
  ungroup()

head(dece.pts.present)
table(dece.pts.present$n.obs)

dece.pts.present.val = dece.valid %>%
  group_by(year, plot) %>%
  summarise(n.obs = n()) %>%
  rbind(pts.per %>% filter(year %in% 2015:2017) %>% select(year, plot) %>% mutate(n.obs = 0)) %>%
  group_by(year, plot) %>%
  summarise(n.obs = sum(n.obs)) %>%
  ungroup()

# A super null model
# Just glm, no random effects
dece.null = glm(cbind(n.obs, 100 - n.obs) ~ 1, 
                family = 'binomial',
                data = dece.pts.present)

summary(dece.null)
fitted.values(dece.null)[1]
# This model predicts that there will be 

rmse.dece.null = rmse(x = dece.pts.present.val$n.obs, 
                      xpred = predict(dece.null, newdata = dece.pts.present.val))

rmse.dece.null
# 11.078

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
                         xpred = predict(dece.rf.null, newdata = dece.pts.present.val))
rmse.dece.rf.null
# 12.41678

# *spits out water*
# WHAT
