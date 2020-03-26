library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

# Read in data, add easting and northing columns
all.sp = read.csv('01_process_data/output/veg_all_predictors.csv') %>%
  mutate(asp.e = sin(pi * asp2 / 180),
         asp.n = cos(pi * asp2 / 180),
         c.slope = slope2 - mean(slope2))

all.sp

head(all.sp)

dece.train = all.sp %>% filter(species %in% 'DECE') %>% mutate(obs = 1:nrow(.))
komy.train = all.sp %>% filter(species %in% 'KOMY') %>% mutate(obs = 1:nrow(.))
gero.train = all.sp %>% filter(species %in% 'GEROT') %>% mutate(obs = 1:nrow(.))

dece.model = glmer(cbind(n.obs, 100 - n.obs) ~ (1 | year) + (1 | plot),
                   family = 'binomial',
                   data = dece.train)

summary(dece.model)
# Very overdispersed.

dece.model = glmer(cbind(n.obs, 100 - n.obs) ~ (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = dece.train)

summary(dece.model)
# Most of this variation is due to plot
# very little variation is due to year.

# Add in aspect, slope

dece.mode2 = glmer(cbind(n.obs, 100 - n.obs) ~ c.slope + asp.n + asp.e +
                                               (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = dece.train)

summary(dece.mode2)
# Take out northing

dece.mode2 = glmer(cbind(n.obs, 100 - n.obs) ~ c.slope + asp.e +
                     (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = dece.train)

summary(dece.mode2)
# This gets rid of about half of the spatial variation
# But plot still accounts for >90% of variation

# Try with gero

gero.model = glmer(cbind(n.obs, 100 - n.obs) ~ (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = gero.train)

summary(gero.model)
# Much less plot-level variation hre

gero.mode2 = glmer(cbind(n.obs, 100 - n.obs) ~ c.slope + asp.e + asp.n +
                     (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = gero.train)

summary(gero.mode2)
# very little effect of aspect or slope here!

gero.mode2 = glmer(cbind(n.obs, 100 - n.obs) ~ c.slope + asp.e +
                     (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = gero.train)

summary(gero.mode2)
# yup. no influence of slope or aspect.

# Try max snowdepth. First, fit a model without snow depth
gero.mode3.base = glmer(cbind(n.obs, 100 - n.obs) ~ 1 +
                          (1 | year) + (1 | plot) + (1 | obs),
                        family = 'binomial',
                        data = gero.train %>% filter(year > 1990))

summary(gero.mode3.base)

gero.mode3 = glmer(cbind(n.obs, 100 - n.obs) ~ log(max.dep + 1) +
                     (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = gero.train %>% filter(year > 1990))

summary(gero.mode3)
# Nope... no effect of log max snow depth...

# Try max snow depth.

# Baseline - remove records w/o max snowdepth

# Now try with kobresia

komy.model = glmer(cbind(n.obs, 100 - n.obs) ~ (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = komy.train[komy.train$year > 1990,])

summary(komy.model)
# holy shit here there is so much plot-level variation

komy.mode2 = glmer(cbind(n.obs, 100 - n.obs) ~ c.slope + asp.e + asp.n +
                                              (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = komy.train[komy.train$year > 1990,])

summary(komy.mode2)
# adding in aspect has a huge influence here

komy.mode2 = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e +  (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = komy.train[komy.train$year > 1990,])

summary(komy.mode2)
# adding in aspect here 
# Moisture here would be good...

# What about adding log of max snow depth? madlad
komy.mode3 = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + log(max.dep + 1) + (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = komy.train[komy.train$year > 1990,])

summary(komy.mode3)
# nope. log of max snow depth doesn't do anything.

# Interaction?
komy.mode3 = glmer(cbind(n.obs, 100 - n.obs) ~ asp.e * log(max.dep + 1) + (1 | year) + (1 | plot) + (1 | obs),
                   family = 'binomial',
                   data = komy.train[komy.train$year > 1990,])

summary(komy.mode3)
# nope.

# mess around with some resid stuff

gg.dece.mod2 = dece.train %>%
  mutate(resid = residuals(dece.mode2)) %>%
  ggplot()

gg.dece.mod2 +
  geom_point(aes(x = UTM_E, y = UTM_N, colour = resid),
             position = position_jitter(width = 10, height = 10),
             size = 3) +
  scale_colour_gradient2(low = 'darkorange', mid = 'white', high = 'purple',
                         midpoint = 0) +
  theme(panel.background = element_rect(fill = 'black'),
        panel.grid = element_blank())

# Next: spatial autocorrelation