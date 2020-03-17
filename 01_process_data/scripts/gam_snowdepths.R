library(ggplot2)
library(dplyr)
library(tidyr)
library(mgcv)

snowdep = read.csv('00_raw_data/snowdepth_all/saddsnow.dw.data.csv')

snowdep %>%
  mutate(year = date %>% 
           as.character() %>% 
           strsplit(split = '\\-') %>%
           sapply(function(x) x[1]) %>% 
           as.numeric(),
         jd = julian.Date(date, origin = paste0(. %>% year %>% as.character(), '-01-01')))

sndp = snowdep %>%
  mutate(year = date %>% 
           as.character() %>% 
           strsplit(split = '\\-') %>%
           sapply(function(x) x[1]) %>% 
           as.numeric(),
         month = date %>% 
           as.character() %>% 
           strsplit(split = '\\-') %>%
           sapply(function(x) x[2]) %>% 
           as.numeric(),
         seas = year + as.numeric(month > 9),
         date.origin = as.Date.character(paste0(year, '-10-01'), format = '%Y-%m-%d'),
         i = 1:nrow(.)) %>%
  group_by(i) %>%
  mutate(jd = date %>% 
           as.Date('%Y-%m-%d') %>% 
           julian.Date(origin = date.origin) %>%
           (function(x) x[1]),
         jd = jd + ifelse(jd < 0, 365 + as.numeric(year %% 4), 0)) %>%
  ungroup() %>%
  select(-c(i, date.origin)) %>%
  filter(!is.nan(mean_depth) | !is.na(mean_depth))

str(sndp)
hist(sndp$jd)

ggplot(sndp, aes(x = jd, y = mean_depth)) +
  geom_point(aes(colour = month), size = 0.2)

# Now, we want a GAM with snow against date of year.
# Maybe we can rope in slope and aspect into this but that makes me incredibly nervous.

sn.gam = gam(mean_depth ~ s(jd, k = 10, bs = 'cc') +
               s(point_ID, bs = 're'),
             data = sndp, method = 'REML')

sn.gam         
gratia::draw(sn.gam)


sn.gam.y = gam(mean_depth ~ s(jd, k = 10, bs = 'cc') +
                 s(point_ID, bs = 're') +
                 s(seas, bs = 're'),
               data = sndp, method = 'REML')

sn.gam.y         
gratia::draw(sn.gam.y)

sn.gam.y20 = gam(mean_depth ~ s(jd, k = 20, bs = 'cc') +
                   s(point_ID, bs = 're') +
                   s(seas, bs = 're'),
                 data = sndp, method = 'REML')

gratia::draw(sn.gam.y20)

sn.gam.y20$fitted.values

# Some stuff to evaluate:

set.seed(1410567)

val.plots = sample(unique(sndp$point_ID), 10)

val.plots
# [1]  48  72  49   1   2  56 401  26  58 701

sndp.pp = sndp %>% mutate(pred = predict(object = sn.gam.y20)) 

sndp.pp %>%
  filter(point_ID %in% 48) %>%
  ggplot(aes(x = jd)) +
  geom_line(aes(y = mean_depth, group = seas), colour = 'purple') +
  geom_point(aes(y = mean_depth), colour = 'purple') +
  geom_line(aes(y = pred, group = seas)) +
  facet_wrap(~ seas)

# This is not good at all. Systematically high. 
# Year to year plots don't look different.

sndp.pp %>%
  filter(point_ID %in% 72) %>%
  ggplot(aes(x = jd)) +
  geom_line(aes(y = mean_depth, group = seas), colour = 'purple') +
  geom_point(aes(y = mean_depth), colour = 'purple') +
  geom_line(aes(y = pred, group = seas)) +
  facet_wrap(~ seas)

sn.gam.ygs = gam(mean_depth ~ s(jd, k = 10, bs = 'cc', m = 2) +
                   s(jd, point_ID, k = 10, bs = 'fs', m = 2) +
                   s(point_ID, bs = 're') +
                   s(seas, bs = 're'),
                 data = sndp, method = 'REML')

gratia::plot(sn.gam.ygs)
# ??? wtf is this?

sndp.pp2 = sndp %>% mutate(pred = predict(object = sn.gam.ygs)) 

sndp.pp2 %>%
  filter(point_ID %in% 48) %>%
  ggplot(aes(x = jd)) +
  geom_line(aes(y = mean_depth, group = seas), colour = 'purple') +
  geom_point(aes(y = mean_depth), colour = 'purple') +
  geom_line(aes(y = pred, group = seas)) +
  facet_wrap(~ seas)

# This also isn't coming close to working.
# We're going to need more smoothing freedom.
# Good lord all of these years look exactly the same.

sndp.pp2 %>%
  filter(point_ID %in% 48) %>%
  ggplot(aes(x = jd)) +
  geom_line(aes(y = pred, group = seas))

sn.gam.ygss = gam(mean_depth ~ s(jd, k = 10, m = 2, bs = 'cc') +
                    s(jd, by = point_ID, k = 10, m = 1, bs = 'tp') +
                    s(point_ID, bs = 're') +
                    s(seas, bs = 're'),
                  data = sndp, method = 'REML')

gratia::draw(sn.gam.ygss) # error???


sndp.gss2 = sndp %>% mutate(pred = predict(object = sn.gam.ygss)) 

sndp.gss2 %>%
  filter(point_ID %in% 48) %>%
  ggplot(aes(x = jd)) +
  geom_line(aes(y = mean_depth, group = seas), colour = 'purple') +
  geom_point(aes(y = mean_depth), colour = 'purple') +
  geom_line(aes(y = pred, group = seas)) +
  facet_wrap(~ seas)

# Yo what the fuck none of these lines are any different!!!

sn.gam.ygss = gam(mean_depth ~ s(jd, k = 10, m = 2, bs = 'cc') +
                    s(jd, by = seas, k = 10, m = 1, bs = 'tp') +
                    s(point_ID, bs = 're') +
                    s(seas, bs = 're'),
                  data = sndp, method = 'REML')

gratia::draw(sn.gam.ygss) # also an error...

sndp.gss2 = sndp %>% mutate(pred = predict(object = sn.gam.ygss)) 

sndp.gss2 %>%
  filter(point_ID %in% 48) %>%
  ggplot(aes(x = jd)) +
  geom_line(aes(y = mean_depth, group = seas), colour = 'purple') +
  geom_point(aes(y = mean_depth), colour = 'purple') +
  geom_line(aes(y = pred, group = seas)) +
  facet_wrap(~ seas)

# these also suck!
# ughhgh