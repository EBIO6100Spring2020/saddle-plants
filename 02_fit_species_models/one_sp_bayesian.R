# Second try at fitting models.
# sn 18 mar 2020

library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

##### Read in data and check column completeness
all.sp = read.csv('01_process_data/output/veg_all_predictors.csv')

head(all.sp)

# Number of rows
nrow(all.sp)
# 3957

# How many NAs are there in each column?
apply(all.sp, 2, function(x) sum(is.na(x)))
# Lots of veg class missing,
# Lots of veg class missing.
# some NPP missing
# some snow depths missing.
# little bit of spatial info missing

# How many rows are there with all data present?
apply(all.sp, 1, function(x) all(!is.na(x))) %>% table()
# 990 rows only... Would be 330 per species.

# Look at spatial info 2 only
all.sp %>%
  select(-c(slope1, elev1, asp1)) %>%
  apply(1, function(x) all(!is.na(x))) %>%
  table()
# 1083 only. Still not very much.

# Maybe if we cut out veg type? It's possibly circular.
all.sp %>%
  select(-c(slope1, elev1, asp1, veg_class)) %>%
  apply(1, function(x) all(!is.na(x))) %>%
  table()
# This... can't be true.

# NPP
all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  apply(1, function(x) all(!is.na(x))) %>%
  table()
# If we take out veg_class and mNPP, then we have
# >3K records

# Is this data frame still balanced across plots?
all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  group_by(plot) %>%
  summarise(n = n())
# Yes.

filt.all.sp = all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x))))

##### Create training and testing data.

dece.train = filt.all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015)
dece.valid = filt.all.sp %>% filter(species %in% 'DECE' & year %in% 2016:2018)

komy.train = filt.all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015)
komy.valid = filt.all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018)

gero.train = filt.all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015)
gero.valid = filt.all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018)

##### Fit models

# Modeling philosophy: model frequentist (for now),
# see how much of training data falls w/in 95% prediction interval.


### Deschampsia models

dece.null = stan_glm(cbind(n.obs, 100 - n.obs) ~ 1,
                        family = 'binomial',
                        data = dece.train,
                        seed = 1,
                        cores = 4)

dece.null.ppred = posterior_predict(dece.null, new.data = dece.valid)

# Check these for overdispersion.

# ## Null model
# dece.null = glm(cbind(n.obs, 100 - n.obs) ~ 1,
#                 family = 'binomial',
#                 data = dece.train)
# 
# summary(dece.null)
# # This model is incredibly overdispersed.
# 
# ## Null model, feat. overdispersion
# dece.null = glm(cbind(n.obs, 100 - n.obs) ~ 1,
#                 family = 'quasibinomial',
#                 data = dece.train)
# 
# summary(dece.null)
# fitted.values(dece.null)[1]
# 
# # Validate.
# dece.null.pred = predict(object = dece.null, new.data = dece.valid, interval = 'prediction',
#                          dispersion = 28.57663)
# 
# dece.null.pred
# Not working. # Fuck man