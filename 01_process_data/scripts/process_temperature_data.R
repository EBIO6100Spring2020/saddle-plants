# Script to merge two temperature datasets into one
# SN April 11, 2020

library(ggplot2)
library(dplyr)
library(tidyr)

# Read in raw data
t.old = read.csv('00_raw_data/temp_old_greenland/sdltdayv.ml.data.csv')
t.new = read.csv('00_raw_data/temp_new_loggers/sdlcr23x-cr1000.daily.ml.data.csv')

head(t.old)
# Don't ned site data (it's all at the same place)

t.old = t.old %>% select(-c(LTER_site, local_site))

head(t.new)
# For now, just get the temp data

t.new = t.new %>% select(logger, date, jday,
                         airtemp_max, flag_airtemp_max,
                         airtemp_min, flag_airtemp_min,
                         airtemp_avg, flag_airtemp_avg)

# Do the merge.
# Old measurements are flagged '_old' while new measurements are flagged '_new'
t.comb = merge(x = t.old, y = t.new,
               by = 'date', suffixes = c('_old', '_new'),
               all.x = TRUE, all.y = TRUE)

head(t.comb)

t.comb.compare = t.comb %>%
  filter(!is.na(airtemp_avg_old) & !is.na(airtemp_avg_new) &
         !is.nan(airtemp_avg_old) & !is.nan(airtemp_avg_new)) %>%
  mutate(year = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[1] %>% as.numeric()))

nrow(t.comb.compare)

ggplot(t.comb.compare) +
  geom_point(aes(x = airtemp_avg_old, y = airtemp_avg_new))

# These actually correspond to each other well...

ggplot(t.comb.compare) +
  geom_point(aes(x = airtemp_avg_old, y = airtemp_avg_new,
                 colour = year), 
             alpha = 0.5) +
  scale_color_gradient(low = 'orange', high = 'blue')

ggplot(t.comb.compare) +
  geom_point(aes(x = airtemp_avg_old, y = airtemp_avg_new), 
             alpha = 0.5) +
  geom_segment(aes(x = -20, y = -20,
                   xend = 10, yend = 10),
               colour = 'gray55') +
  facet_wrap(~ year)
# airtemp_avg high is biased slightly high

ggplot(t.comb.compare) +
  geom_histogram(aes(x = airtemp_avg_new - airtemp_avg_old,
                     fill = logger),
                 binwidth = 0.25) +
  theme(legend.position = 'none') +
  facet_wrap(~ year)

# lamar = rstanarm::stan_glmer(airtemp_avg_new ~ airtemp_avg_old + (1 | year),
#                              family = 'gaussian', cores = 4,
#                              data = t.comb.compare %>% mutate(year = factor(year)),
#                              seed = 41101)
# 
# summary(lamar)
# 
# So: we have a 1.2 - 1.5 degree discrepancy between the old temperatures and the new ones.
# 
# Look at low temperatures:
# ggplot(t.comb.compare) +
#   geom_point(aes(x = airtemp_min_old, y = airtemp_min_new), 
#              alpha = 0.5) +
#   geom_segment(aes(x = -20, y = -20,
#                    xend = 10, yend = 10),
#                colour = 'gray55') +
#   facet_wrap(~ year)
# 
# lamin = rstanarm::stan_glmer(airtemp_min_new ~ airtemp_min_old + (1 | year),
#                              family = 'gaussian', cores = 4,
#                              data = t.comb.compare %>% mutate(year = factor(year)),
#                              seed = 7709210)
# 
# summary(lamin)
# 
# # we have 
# 
# lamax = rstanarm::stan_glmer(airtemp_max_new ~ airtemp_max_old + (1 | year),
#                              family = 'gaussian', cores = 4,
#                              data = t.comb.compare %>% mutate(year = factor(year)),
#                              seed = 31210110)
# summary(lamax)
# 
# # max temp is 2.5 degrees higher on the other loggers!

# Will need to run the modls "backwards" to get 'old' loggers


lamar = rstanarm::stan_glmer(airtemp_avg_old ~ airtemp_avg_new + (1 | year),
                             family = 'gaussian', seed = 41101, cores = 4,
                             data = t.comb.compare %>% mutate(year = factor(year)))
# summary(lamar)
lamin = rstanarm::stan_glmer(airtemp_min_old ~ airtemp_min_new + (1 | year),
                             family = 'gaussian', seed = 7709210, cores = 4,
                             data = t.comb.compare %>% mutate(year = factor(year)))
# summary(lamin)
lamax = rstanarm::stan_glmer(airtemp_max_old ~ airtemp_max_new + (1 | year),
                             family = 'gaussian', seed = 31210110, cores = 4,
                             data = t.comb.compare %>% mutate(year = factor(year)))

# Make a data frame of days we can impute 
t.impute = t.comb %>%
  separate(date, into = c('year', 'month', 'day'), sep = '-') %>%
  filter(is.na(airtemp_avg_old) & !(is.na(airtemp_avg_new) | is.nan(airtemp_avg_new))) %>%
  select(-contains('old')) %>%
  mutate(imp_avg_old = rstanarm::posterior_predict(lamar, newdata = ., seed = 10715) %>% 
           apply(2, mean),
         imp_min_old = rstanarm::posterior_predict(lamin, newdata = ., seed = 319480) %>% 
           apply(2, mean),
         imp_max_old = rstanarm::posterior_predict(lamax, newdata = ., seed = 125509) %>%
           apply(2, mean))

t.impute

t.comb.all = merge(x = t.comb %>% separate(date, into = c('year', 'month', 'day'), sep = '-'),
                   y = t.impute %>% select(year, month, day, contains('imp')),
                   by = c('year', 'month', 'day'), all.x = TRUE) %>%
  mutate(max_temp = ifelse(is.na(airtemp_max_old), imp_max_old, airtemp_max_old),
         avg_temp = ifelse(is.na(airtemp_avg_old), imp_avg_old, airtemp_avg_old),
         min_temp = ifelse(is.na(airtemp_min_old), imp_min_old, airtemp_min_old)) %>%
  select(year, month, day, max_temp, avg_temp, min_temp, contains('flag'))

apply(t.comb.all, 2, function(x) sum(is.na(x)))

write.csv(t.comb.all, row.names = FALSE,
           file = '01_process_data/output/daily_airtemp_all.csv')
