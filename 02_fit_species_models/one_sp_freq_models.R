# Second try at fitting models.
# sn 18 mar 2020

library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(merTools)

##### Read in data and check column completeness
all.sp = read.csv('01_process_data/output/veg_all_predictors.csv')

# Get rid of rows with any NAs for variables of interest.
filt.all.sp = all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  mutate(asp.e = sin(pi * asp2 / 180),
         asp.n = cos(pi * asp2 / 180),
         c.slope = slope2 - mean(slope2))

##### Create training and testing data.

# dece.train = all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015) %>% mutate(maxs = 100, obsno = 1:nrow(.))
dece.train = filt.all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015) %>% mutate(maxs = 100, obsno = 1:nrow(.))
dece.valid = filt.all.sp %>% filter(species %in% 'DECE' & year %in% 2016:2018) %>% mutate(maxs = 100, obsno = 1:nrow(.))

komy.train = all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015) %>% mutate(maxs = 100, obsno = 1:nrow(.))
komy.valid = all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018) %>% mutate(maxs = 100, obsno = 1:nrow(.))

gero.train = all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015) %>% mutate(maxs = 100, obsno = 1:nrow(.))
gero.valid = all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018) %>% mutate(maxs = 100, obsno = 1:nrow(.))


##### Try Deschampsia models

###
### Deschampsia null model
dece.fnull = glmer(cbind(n.obs, 100 - n.obs) ~ (1 | plot) + (1 | year) + (1 | obsno),
                  family = binomial,
                  data = dece.train)
summary(dece.null)

dece.null.eval = cbind(dece.train, 
                       predictInterval(dece.null, dece.valid,
                                       type = 'probability', seed = 12698))
with(dece.null.eval, mean(n.obs/100 < upr & n.obs/100 > lwr))

# Goddamnit this won't let you predict new levels
# welp
