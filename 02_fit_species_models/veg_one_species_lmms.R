# Script for setting up models looking at veg abundance
# Initially starting with all three sp. in the same script.
# february 2020 - 
# scott

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

veg.focs = read.csv('01_process_data/output/veg_focals_top_sans96.csv')

head(veg.focs)

with(veg.focs, table(USDA_code, year))

# Now, set aside year 2015 - 2017 as testing data, 2018 as mega-test
# This means 1989 - 2014 are training data

veg.train = veg.focs %>% filter(year %in% 1989:2014)
veg.valid = veg.focs %>% filter(year %in% 2015:2017)

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

