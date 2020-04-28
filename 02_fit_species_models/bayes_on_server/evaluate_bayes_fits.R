# File for evaluating model fits.

# Load ze packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstanarm)

# Read in data
all.sp = read.csv('01_process_data/output/veg_all_predictors.csv') %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  mutate(asp.e = sin(pi * asp2 / 180),
         asp.n = cos(pi * asp2 / 180),
         c.slope = slope2 - mean(slope2))

##### Create training and testing data.

dece.train = all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
dece.valid = all.sp %>% filter(species %in% 'DECE' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

komy.train = all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
komy.valid = all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

gero.train = all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
gero.valid = all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

##### Some aux functions

eval_ci = function(x) {
  with(x, table(n.obs >= yhat_q025, n.obs <= yhat_q975, 
                dnn = list('over025', 'under975')) / length(obsno))
}

rmse = function(x) with(x, mean(n.obs - yhat_mean)^2)

##### Load in model fits.

all.nulls = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_null_summary.csv')
all.aspes = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_aspe_summary.csv')
all.en = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_aspen_summary.csv')
all.ec = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_aspe_slope_summary.csv')

head(all.aspes)

##### Deschampsia models

### Null model
dece.null = merge(dece.valid, all.nulls %>% filter(sp %in% 'dece'),
                  by.x = 'obsno', by.y = 'i')

rmse(dece.null)
# 14.942
eval_ci(dece.null)
# 78.8 in confidence intervl

### Easting model
dece.aspe = merge(dece.valid, all.aspes %>% filter(sp %in% 'dece'),
                  by.x = 'obsno', by.y = 'i')

rmse(dece.aspe)
# 14.637
eval_ci(dece.aspe)
# 79.2

### Easting + Northing model
dece.enf = merge(dece.valid, all.en %>% filter(sp %in% 'dece'),
                by.x = 'obsno', by.y = 'i')

rmse(dece.enf)
# 14.523
eval_ci(dece.enf)
# 79.2

dece.ecf = merge(dece.valid, all.ec %>% filter(sp %in% 'dece'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.ecf)
# 14.810
eval_ci(dece.ecf)
# 80.3

##### Kobresia models

### Null model
komy.null = merge(komy.valid, all.nulls %>% filter(sp %in% 'komy'),
                  by.x = 'obsno', by.y = 'i')

rmse(komy.null)
# 3.72
eval_ci(komy.null)
# 93.1%

### Easting model
komy.aspe = merge(komy.valid, all.aspes %>% filter(sp %in% 'komy'),
                  by.x = 'obsno', by.y = 'i')

rmse(komy.aspe)
# 3.78
eval_ci(komy.aspe)
# 92.0%

### Easting + northng model
komy.enf = merge(komy.valid, all.en %>% filter(sp %in% 'komy'),
                by.x = 'obsno', by.y = 'i')

rmse(komy.enf)
# 3.78
eval_ci(komy.enf)
# 92.8

komy.ecf = merge(komy.valid, all.ec %>% filter(sp %in% 'komy'),
                 by.x = 'obsno', by.y = 'i')

rmse(komy.ecf)
# 3.65
eval_ci(komy.ecf)
# 92.8

##### Geum models

### Null model
gero.null = merge(gero.valid, all.nulls %>% filter(sp %in% 'gero'),
                  by.x = 'obsno', by.y = 'i')

rmse(gero.null)
# 0.007 (wow)
eval_ci(gero.null)
# 92.8%

### Easting model
gero.aspe = merge(gero.valid, all.aspes %>% filter(sp %in% 'gero'),
                  by.x = 'obsno', by.y = 'i')

rmse(gero.aspe)
# 0.011
eval_ci(gero.aspe)
# 92.8%

gero.enf = merge(gero.valid, all.en %>% filter(sp %in% 'gero'),
                 by.x = 'obsno', by.y = 'i')

rmse(gero.enf)
# 0.076
eval_ci(gero.enf)
# 93.1

gero.ecf = merge(gero.valid, all.ec %>% filter(sp %in% 'gero'),
                 by.x = 'obsno', by.y = 'i')

rmse(gero.ecf)
# 0.014
eval_ci(gero.ecf)
# 92.8

##### Variance partitioning.

load('02_fit_species_models/bayes_on_server/models/e_mods.RData')
# contains: dece.e, komy.e, gero.e

# Deschampsia easting only model

dece.e
# ------
#             Median MAD_SD
# (Intercept) -6.9    0.6  
# asp.e        2.4    0.7  
# 
# Error terms:
#   Groups Name        Std.Dev.
# obsno  (Intercept) 0.33    
# plot   (Intercept) 3.93    
# year   (Intercept) 0.34    
# Num. levels: obsno 879, plot 88, year 10  

# Get coefficients for asp.e model:
quantile(as.data.frame(dece.e)$asp.e)
#         0%        25%        50%        75%       100% 
# -0.1147437  1.9365729  2.4038198  2.9036827  5.4575117 
mean(as.data.frame(dece.e)$asp.e > 0)

### Kobresia easting only model

komy.e
# ------
#             Median MAD_SD
# (Intercept) -7.4    0.7  
# asp.e       -6.2    1.0  
# 
# Error terms:
#   Groups Name        Std.Dev.
# obsno  (Intercept) 0.31    
# plot   (Intercept) 4.56    
# year   (Intercept) 0.44    
# Num. levels: obsno 879, plot 88, year 10 

# Get coefficients for asp.e model:
quantile(as.data.frame(komy.e)$asp.e)
#         0%        25%        50%        75%       100% 
# -10.497723  -6.912314  -6.234760  -5.600576  -3.175512 
mean(as.data.frame(komy.e)$asp.e > 0)

### Geum easting only model

gero.e
# ------
#             Median MAD_SD
# (Intercept) -3.1    0.3  
# asp.e        0.1    0.4  
# 
# Error terms:
#   Groups Name        Std.Dev.
# obsno  (Intercept) 0.18    
# plot   (Intercept) 2.29    
# year   (Intercept) 0.11    
# Num. levels: obsno 879, plot 88, year 10 

quantile(as.data.frame(gero.e)$asp.e)
#           0%         25%         50%         75%        100% 
# -1.01333034 -0.15962609  0.07570233  0.32255145  1.20029411 
mean(as.data.frame(gero.e)$asp.e > 0)

### easting and northing models
load('02_fit_species_models/bayes_on_server/models/en_mods.RData')

dece.en
# asp.n not significant

komy.en
# asp.n not significant

gero.en
# asp.n not significant

### easting and slope models

### easting and slope models
load('02_fit_species_models/bayes_on_server/models/ec_mods.RData')

dece.ec
# slope is significant here

komy.ec
# slope is n.s here

gero.ec
# slope is n.s. here

####### Visualizing model fits

rbind(all.nulls, all.aspes, all.en) %>%
  filter(sp %in% 'dece') %>%
  spread(key = model, value = yhat_medn) %>%
  rename_at(vars(null, aspe, aspen), function(x) paste0(x, '_medn'))

### Deschampsia fits
dece.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = all.nulls %>% filter(sp %in% 'dece')) %>%
  arrange(desc(yhat_medn)) %>%
  mutate(j = 1:nrow(.)) %>%
  ggplot() +
  geom_segment(aes(x = j, xend = j, y = yhat_q025, yend = yhat_q975),
               size = 0.5, colour = 'orange', lineend = 'square') +
  geom_point(aes(x = j, y = yhat_medn), colour = 'orange') +
  geom_point(aes(x = j, y = n.obs), colour = 'blue', alpha = 0.25)

### Kobresia fits
komy.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = all.nulls %>% filter(sp %in% 'komy')) %>%
  arrange(desc(yhat_medn)) %>%
  mutate(j = 1:nrow(.)) %>%
  ggplot() +
  geom_segment(aes(x = j, xend = j, y = yhat_q025, yend = yhat_q975),
               size = 0.5, colour = 'orange', lineend = 'square') +
  geom_point(aes(x = j, y = yhat_medn), colour = 'orange') +
  geom_point(aes(x = j, y = n.obs), colour = 'blue', alpha = 0.25)  
# Still looks biased high.

### Geum fits
gero.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = all.nulls %>% filter(sp %in% 'gero')) %>%
  arrange(desc(yhat_medn)) %>%
  mutate(j = 1:nrow(.)) %>%
  ggplot() +
  geom_segment(aes(x = j, xend = j, y = yhat_q025, yend = yhat_q975),
               size = 0.5, colour = 'orange', lineend = 'square') +
  geom_point(aes(x = j, y = yhat_medn), colour = 'orange') +
  geom_point(aes(x = j, y = n.obs), colour = 'blue', alpha = 0.25)

ggp.gero.en = gero.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = all.en %>% filter(sp %in% 'gero')) %>%
  arrange(desc(yhat_medn)) %>%
  mutate(j = 1:nrow(.)) %>%
  ggplot() 


# Has residual bars colored by direction
ggp.gero.en +
  geom_segment(aes(x = j, xend = j, y = yhat_q025, yend = yhat_q975),
               size = 0.0625, colour = 'orange', lineend = 'square') +
  geom_point(aes(x = j, y = yhat_medn), colour = 'orange') +
  geom_point(aes(x = j, y = yhat_mean), colour = 'red', alpha = 0.25) +
  geom_point(aes(x = j, y = n.obs), colour = 'blue', alpha = 0.25) +
  geom_segment(aes(x = j, xend = j, y = yhat_mean, yend = n.obs,
                   colour = as.logical(yhat_mean > n.obs)),
                   size = 0.125) +
  scale_color_manual(values = c('blue', 'red')) +
  guides(colour = 'none')



               