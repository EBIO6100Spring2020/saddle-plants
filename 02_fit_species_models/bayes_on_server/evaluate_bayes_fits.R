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

eval_95ci = function(x) {
  with(x, table(n.obs >= yhat_q025, n.obs <= yhat_q975, 
                dnn = list('over025', 'under975')) / length(obsno))
}

eval_68ci = function(x) {
  with(x, table(n.obs >= yhat_q159, n.obs <= yhat_q841, 
                dnn = list('over025', 'under975')) / length(obsno))
}

rmse = function(x) with(x, mean(n.obs - yhat_mean)^2)

##### Load in model fits.

all.nulls = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_null_summary.csv')
all.aspes = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_aspe_summary.csv')
all.en = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_aspen_summary.csv')
all.ec = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_aspe_slope_summary.csv')
dece.temp = read.csv('02_fit_species_models/bayes_on_server/model_preds/dece_temp_summary.csv')
koge.temp = read.csv('02_fit_species_models/bayes_on_server/model_preds/komy_gero_temp_summary.csv')
all.nitrs = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_nitr_summary.csv')
all.ph = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_ph_summary.csv')
all.cl = read.csv('02_fit_species_models/bayes_on_server/model_preds/all_cl_summary.csv')

head(all.aspes)

##### Deschampsia models

### Null model
dece.null = merge(dece.valid, all.nulls %>% filter(sp %in% 'dece'),
                  by.x = 'obsno', by.y = 'i')

rmse(dece.null) # 14.889
eval_95ci(dece.null) # 79.2
eval_68ci(dece.null) # 63.6
-2 * dece.null$loglik[1] # 2088.213

# Models are overestimating

### Easting model
dece.aspe = merge(dece.valid, all.aspes %>% filter(sp %in% 'dece'),
                  by.x = 'obsno', by.y = 'i')

rmse(dece.aspe) # 14.608
eval_95ci(dece.aspe) # 79.9
eval_68ci(dece.aspe) # 64.8
-2 * dece.aspe$loglik[1] # 2089.216

# Slight decrease in RMSE, v. slight increase in deviance
# Slight improvement in CIs
# Models are still overestimating
# Keep Easting

### Easting + Northing model
dece.enf = merge(dece.valid, all.en %>% filter(sp %in% 'dece'),
                by.x = 'obsno', by.y = 'i')

rmse(dece.enf) # 14.298
eval_95ci(dece.enf) # 79.5
eval_68ci(dece.enf) # 65.2
-2 * dece.enf$loglik[1] # 2090.79

# slight decrease in RMSE
# slight decrease in 95% CI
# but increase in 68 CI, deviance

dece.ecf = merge(dece.valid, all.ec %>% filter(sp %in% 'dece'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.ecf) # 14.810
eval_95ci(dece.ecf) # 79.9
eval_68ci(dece.ecf) # 64.0
-2 * dece.ecf$loglik[1] # 2092.542

### Slope + jja3 model

dece.sj3 = merge(dece.valid, dece.temp %>% filter(model %in% 'slope.jja3'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.sj3) # 22.25 ...?
eval_95ci(dece.sj3) # 79.9%
eval_68ci(dece.sj3) # 63.3%
-2 * dece.sj3$loglik[1] # 2090.80

# This model is much worse??

### No slope + jja3 model

dece.nj3 = merge(dece.valid, dece.temp %>% filter(model %in% 'noslope.jja3'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.nj3) # 22.70
eval_95ci(dece.nj3) # 77.6%
eval_68ci(dece.nj3) # 62.8%
-2 * dece.nj3$loglik[1] # 20.89

# much worse

### Slope + jja1 model

dece.sj1 = merge(dece.valid, dece.temp %>% filter(model %in% 'slope.jja1'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.sj1) # 8.07 # WHAT
eval_95ci(dece.sj1) # 85.2%
eval_68ci(dece.sj1) # 71.5%
-2 * dece.sj1$loglik[1] # 2091.5

# WHAT

dece.nj1 = merge(dece.valid, dece.temp %>% filter(model %in% 'noslope.jja1'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.nj1) # 7.95
eval_95ci(dece.nj1) # 84.9%
eval_68ci(dece.nj1) # 72.3%
-2 * dece.nj1$loglik[1] # 2089.12

### A model with Nitrogen

dece.nitr = merge(dece.valid, all.nitrs %>% filter(sp %in% 'dece'),
                  by.x = 'obsno', by.y = 'i')

rmse(dece.nitr) # 7.79
eval_95ci(dece.nitr) # 84.8%
eval_68ci(dece.nitr) # 71.9%
-2 * dece.nitr$loglik[1] # 2090.12

### Model with pH

dece.phs = merge(dece.valid, all.ph %>% filter(sp %in% 'dece'),
                 by.x = 'obsno', by.y = 'i')

rmse(dece.phs) # 0.005
eval_95ci(dece.phs) # 92.0%
eval_68ci(dece.phs) # 71.5%
-2 * dece.cl$loglik[1] # 2090.929

### Model with Chlorine

dece.cl = merge(dece.valid, all.cl %>% filter(sp %in% 'dece'),
                by.x = 'obsno', by.y = 'i')

rmse(dece.cl) # 1.993
eval_95ci(dece.cl) # 91.6%
eval_68ci(dece.cl) # 77.6
-2 * dece.cl$loglik[1] # 2089

# No Chlorine for DECE

##### Kobresia models

### Null model
komy.null = merge(komy.valid, all.nulls %>% filter(sp %in% 'komy'),
                  by.x = 'obsno', by.y = 'i')

rmse(komy.null) # 3.79
eval_95ci(komy.null) # 93.1%
eval_68ci(komy.null) # 81.8%
-2 * komy.null$loglik[1] # 1299.86

### Easting model
komy.aspe = merge(komy.valid, all.aspes %>% filter(sp %in% 'komy'),
                  by.x = 'obsno', by.y = 'i')

rmse(komy.aspe) # 3.67
eval_ci(komy.aspe)
# 92.0%

### Easting + northng model
komy.enf = merge(komy.valid, all.en %>% filter(sp %in% 'komy'),
                by.x = 'obsno', by.y = 'i')

rmse(komy.enf) # 3.74
eval_95ci(komy.enf) # 93.1
eval_68ci(komy.enf) # 81.4%
-2 * komy.enf$loglik[1] # 1300.6

# Easting plus slope model
komy.ecf = merge(komy.valid, all.ec %>% filter(sp %in% 'komy'),
                 by.x = 'obsno', by.y = 'i')

rmse(komy.ecf) # 3.65
eval_95ci(komy.ecf) # 93.5
eval_68ci(komy.ecf) # 82.1
-2 * komy.ecf$loglik[1] # 1301.16

### Try Kobresia model with easting and jja2

komy.ej2 = merge(komy.valid, koge.temp %>% 
                   filter(sp %in% 'komy' & model %in% 'easting.jja2'),
                 by.x = 'obsno', by.y = 'i')

rmse(komy.ej2) # 7.48
eval_95ci(komy.ej2) # 93.1
eval_68ci(komy.ej2) # 80.3
-2 * komy.ej2$loglik[1] # 1301.938

### Kobresia model without easting but with jja2

komy.nj2 = merge(komy.valid, koge.temp %>% 
                   filter(sp %in% 'komy' & model %in% 'noeasting.jja2'),
                 by.x = 'obsno', by.y = 'i')

rmse(komy.nj2) # 7.26
eval_95ci(komy.ej2) # 92.4
eval_68ci(komy.ej2) # 80.3
-2 * komy.nj2$loglik[1] # 1300.6

### A model with Nitrogen

komy.nitr = merge(komy.valid, all.nitrs %>% filter(sp %in% 'komy'),
                  by.x = 'obsno', by.y = 'i')

rmse(komy.nitr) # 0.90
eval_95ci(komy.nitr) # 96.2%
eval_68ci(komy.nitr) # 90.1%
-2 * komy.nitr$loglik[1] # 2090.92

# Well, it looks like N is a good predictor of N abundance

### A model with pH (and Nitrogen)

komy.phs = merge(komy.valid, all.ph %>% filter(sp %in% 'komy'),
                 by.x = 'obsno', by.y = 'i')

rmse(komy.phs) # 0.207
eval_95ci(komy.nitr) # 96.3%
eval_68ci(komy.nitr) # 90.2%
# deviance doesn't matter lolz

komy.cl = merge(komy.valid, all.cl %>% filter(sp %in% 'komy'),
                by.x = 'obsno', by.y = 'i')

rmse(komy.cl) # 0.042 # wow...
eval_95ci(komy.cl) # 97.3%
eval_68ci(komy.cl) # 95.45 (wow.)

# okay... keep chlorine for KOMY

# apparently we should also include pH

##### Geum models

### Null model
gero.null = merge(gero.valid, all.nulls %>% filter(sp %in% 'gero'),
                  by.x = 'obsno', by.y = 'i')

rmse(gero.null) # 0.009 (wow)
eval_95ci(gero.null) # 93.1%
eval_68ci(gero.null) # 67.0% (right on)
-2 * gero.null$loglik[1] # 3604.2

### Easting model
gero.aspe = merge(gero.valid, all.aspes %>% filter(sp %in% 'gero'),
                  by.x = 'obsno', by.y = 'i')

rmse(gero.aspe) # 0.011
eval_95ci(gero.aspe) # 93.1%
eval_68ci(gero.aspe) # 67.0
-2 * gero.aspe$loglik[1] # 3604.6

### Easting + Northing
gero.enf = merge(gero.valid, all.en %>% filter(sp %in% 'gero'),
                 by.x = 'obsno', by.y = 'i')

rmse(gero.enf) # 0.011
eval_95ci(gero.enf) # 93.2
eval_68ci(gero.enf) # 66.6
-2 * gero.enf$loglik[1] # 3602.94

gero.ecf = merge(gero.valid, all.ec %>% filter(sp %in% 'gero'),
                 by.x = 'obsno', by.y = 'i')

rmse(gero.ecf) # 0.010
eval_95ci(gero.ecf) # 93.2
eval_68ci(gero.ecf) # 67.8
-2 * gero.ecf$loglik[1] # 3603.9

### Geum model with jja3

gero.ja3 = merge(gero.valid, koge.temp %>% 
                   filter(sp %in% 'gero' & model %in% 'jja3'))

rmse(gero.ja3) # 1.935
eval_95ci(gero.ja3) # holy shit 31% that is awful
eval_68ci(gero.ja3) # 18
-2 * gero.ja3$loglik[1] # 3603.6

# holy shit what a bad model

### A model with Nitrogen

gero.nitr = merge(gero.valid, all.nitrs %>% filter(sp %in% 'gero'),
                  by.x = 'obsno', by.y = 'i')

rmse(gero.nitr) # 0.039
eval_95ci(gero.nitr) # 93.1%
eval_68ci(gero.nitr) # 68.1%
-2 * gero.nitr$loglik[1] # 2090.92

# A model with pH (but no Nitrogen)

gero.phs = merge(gero.valid, all.ph %>% filter(sp %in% 'gero'),
                 by.x = 'obsno', by.y = 'i')

rmse(gero.phs) # 0.558
eval_95ci(gero.phs) # 92.1%
eval_68ci(gero.phs) # 69.3%
# no added benefit of adding ph (lmao)

### A Geum model with Chlorine

gero.cl = merge(gero.valid, all.cl %>% filter(sp %in% 'gero'),
                by.x = 'obsno', by.y = 'i')

rmse(gero.cl) # 0.011 # same as bad models above
eval_95ci(gero.cl) # 92.4
eval_68ci(gero.cl) # 69.3

# don't add chlorine

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
load('02_fit_species_models/bayes_on_server/models/ec_mods.RData')

dece.ec
# slope is significant here

komy.ec
# slope is n.s here

gero.ec
# slope is n.s. here

### Deschampsia temperature models
load('02_fit_species_models/bayes_on_server/models/dece_temp_mods.RData')
rm(dece.sj3, dece.nj3)


d.slp.ja1
# ... jja_mean1 is totally n.s...?

d.nsp.ja1
# same result here
# year-level varance is also about the same...

### Nitrogen models

load('02_fit_species_models/bayes_on_server/models/all_nitrs.RData')

dece.nitr

komy.nitr

gero.nitr

### pH models

load('02_fit_species_models/bayes_on_server/models/all_phs.RData')

summary(dece.ph)
#                                       mean   sd    10%   50%   90%
# (Intercept)                          -13.1    2.4 -16.1 -13.1 -10.1
# asp.e                                  2.4    0.7   1.5   2.4   3.3
# jja_mean1                             -0.1    0.1  -0.2  -0.1  -0.1
# pH                                     1.4    0.4   0.9   1.4   1.9

# Error terms:  
# Groups Name        Std.Dev.
# obsno  (Intercept) 0.33    
# plot   (Intercept) 3.93    
# year   (Intercept) 0.20  

summary(komy.ph)
#                                         mean   sd    10%   50%   90%
# (Intercept)                          -16.6    3.9 -21.4 -16.7 -11.9
# Total_N                               -1.0    0.7  -1.8  -1.0  -0.1
# pH                                     1.4    0.6   0.7   1.4   2.2

# Error terms:
# Groups Name        Std.Dev.
# obsno  (Intercept) 0.30    
# plot   (Intercept) 6.74    
# year   (Intercept) 0.25   

summary(gero.ph)
#                                       mean   sd   10%   50%   90%
# (Intercept)                          -1.3    1.2 -2.8  -1.3   0.1 
# pH                                   -0.3    0.2 -0.6  -0.3  -0.1

# Error terms:
# Groups Name        Std.Dev.
# obsno  (Intercept) 0.18    
# plot   (Intercept) 2.26    
# year   (Intercept) 0.10 

### Chlorine models

load('02_fit_species_models/bayes_on_server/models/all_chlorines.RData')

komy.cl
#             Median MAD_SD
# (Intercept) -14.1    4.5 
# Total_N      -0.8    0.7 
# pH            1.0    0.7 
# Cl           -6.2    6.3 
# 
# Error terms:
# Groups Name        Std.Dev.
# obsno  (Intercept) 0.30    
# plot   (Intercept) 6.74    
# year   (Intercept) 0.27  

# appears n.s...., year variance is same
# ??? spurious?

gero.cl

#             Median MAD_SD
# (Intercept) -3.1    0.3  
# Cl           2.3    1.8  
# 
# Error terms:
# Groups Name        Std.Dev.
# obsno  (Intercept) 0.18    
# plot   (Intercept) 2.27    
# year   (Intercept) 0.11 

# also not a much better model...

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

# Deschampsia with jja1

dece.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = dece.temp %>% filter(model %in% 'slope.jja1')) %>%
  arrange(desc(yhat_medn)) %>%
  mutate(j = 1:nrow(.)) %>%
  ggplot() +
  geom_segment(aes(x = j, xend = j, y = yhat_q025, yend = yhat_q975),
               size = 0.5, colour = 'orange', lineend = 'square') +
  geom_point(aes(x = j, y = yhat_medn), colour = 'orange') +
  geom_point(aes(x = j, y = n.obs), colour = 'blue', alpha = 0.25)

# Deschampsia with Nitrogen

dece.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = all.nitrs %>% filter(sp %in% 'dece')) %>%
  arrange(desc(yhat_medn)) %>%
  mutate(j = 1:nrow(.)) %>%
  ggplot() +
  geom_segment(aes(x = j, xend = j, y = yhat_q159, yend = yhat_q841),
               size = 0.5, colour = 'orange', lineend = 'square') +
  geom_point(aes(x = j, y = yhat_medn), colour = 'orange') +
  geom_point(aes(x = j, y = n.obs), colour = 'blue', alpha = 0.25)

# Results here are still biased high.

rbind(all.ec %>% filter(sp %in% 'dece'),
      dece.temp %>% filter(model %in% 'slope.jja1')) %>%
  ggplot() +
  geom_point(aes(x = i, y = yhat_mean, 
                 group = model,
                 colour = model),
             position = position_dodge())

rbind(all.ec %>% filter(sp %in% 'dece'),
      dece.temp %>% filter(model %in% 'slope.jja1')) %>%
  select(i, yhat_mean, model) %>%
  spread(key = model, value = yhat_mean) %>%
  ggplot() +
  geom_point(aes(x = aspen, y = slope.jja1)) +
  geom_segment(aes(x = 0, xend = 70, y = 0, yend = 70))
# really confused...

dece.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = rbind(
    dece.temp %>% filter(model %in% c('noslope.jja1', 'slope.jja1')),
    all.ec %>% filter(sp %in% 'dece')
    ),
    by = 'i') %>%
  mutate(nobs_jit = n.obs + ifelse(model %in% 'aspen', -0.25,
                                   ifelse(model %in% 'slope.jja1', 0, 0.25))) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  geom_point(aes(x = nobs_jit, y = yhat_mean, 
                 group = model, 
                 colour = model),
             alpha = 0.5) +
  geom_segment(aes(x = nobs_jit, xend = nobs_jit,
                   y = yhat_mean, yend = nobs_jit,
                   colour = model),
               size = 0.15)
# well, this shows the bias pretty well.
# but it illustrates the differences in models well enough.

dece.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = rbind(
    all.nitrs %>% filter(sp %in% 'dece'),
    all.ph %>% filter(sp %in% 'dece') %>% mutate(model = 'ph'),
    all.ec %>% filter(sp %in% 'dece')
  ),
  by = 'i') %>%
  mutate(nobs_jit = n.obs + ifelse(model %in% 'aspen', -0.25,
                                   ifelse(model %in% 'ph', 0, 0.25))) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  geom_point(aes(x = nobs_jit, y = yhat_mean, 
                 group = model, 
                 colour = model),
             alpha = 0.5) +
  geom_segment(aes(x = nobs_jit, xend = nobs_jit,
                   y = yhat_mean, yend = nobs_jit,
                   colour = model),
               size = 0.15)

# yep... pH definitely means we're no longer overshooting on average

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

komy.valid %>%
  select(obsno, n.obs) %>%
  rename(i = obsno) %>%
  merge(y = rbind(
    all.nitrs %>% filter(sp %in% 'komy'),
    all.ph %>% filter(sp %in% 'komy') %>% mutate(model = 'ph'),
    all.nulls %>% filter(sp %in% 'komy')
  ),
  by = 'i') %>%
  mutate(nobs_jit = n.obs + ifelse(model %in% 'null', -0.25,
                                   ifelse(model %in% 'ph', 0, 0.25))) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  geom_point(aes(x = nobs_jit, y = yhat_mean, 
                 group = model, 
                 colour = model),
             alpha = 0.5) +
  geom_segment(aes(x = nobs_jit, xend = nobs_jit,
                   y = yhat_mean, yend = nobs_jit,
                   colour = model),
               size = 0.15)
# damn wut this is actually v. good
# what the fuck changed?

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



               