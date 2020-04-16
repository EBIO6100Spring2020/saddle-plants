# Script to merge two temperature datasets into one
# SN April 11, 2020

library(ggplot2)
library(dplyr)
library(tidyr)

##### Part 1: Reconcile potential differences in calibration between the two datasets,
# and impute misisng data to produce one unified daily tempreature dataset.

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

##### Now, actually generate temperature statistics for use in analysis.

### Read in temperature data, add in julian date (`jd`), water year (October -
# September, `wyear`), julian day adjusted for water year (`wy`)
daily = read.csv('01_process_data/output/daily_airtemp_all.csv') %>%
  mutate(jd = paste('1970', month, day, sep = '-') %>% as.Date() %>% julian(),
         wyear = year + as.numeric(month > 9),
         wd = jd - 273 + ifelse(wyear == year, 365, 0))

### Generate annual mean temperatures

# Data frame with mean June - August temperature for each year
# p1y, p2y, p3y columns are used for getting the time-lagged data below
# (gets previous 1 year, 2 years, 3 years of temperatures)
# jja_mean - mean daily average temperature for June, July, August
ann.means = daily %>%
  group_by(wyear) %>%
  summarise(jja_mean = mean(avg_temp[month %in% 6:8])) %>%
  mutate(p1y = wyear + 1,
         p2y = wyear + 2,
         p3y = wyear + 3)

# Use merging to connect year column to previous year data
# Make the following output columns:
#   jja_mean2 - mean Jun-Aug temperatures over last two years
#   jja_mean3 - mean Jun-Aug temperatures over last three years
#   jja_mean4 - mean Jun-Aug temperatures over last four years
rolling.means = merge(x = ann.means %>% select(-c(p1y, p2y, p3y)),
                      y = ann.means %>% select(-c(wyear, p2y, p3y)),
                      by.x = 'wyear', by.y = 'p1y',
                      suffixes = c('', '_p1')) %>%
  merge(y = ann.means %>% select(-c(wyear, p1y, p3y)),
        by.x = 'wyear', by.y = 'p2y',
        suffixes = c('', '_p2')) %>%
  merge(y = ann.means %>% select(-c(wyear, p1y, p2y)),
        by.x = 'wyear', by.y = 'p3y',
        suffixes = c('', '_p3')) %>%
  mutate(jja_mean2 = (jja_mean + jja_mean_p1)/2,
         jja_mean3 = (jja_mean + jja_mean_p1 + jja_mean_p2)/3,
         jja_mean4 = (jja_mean + jja_mean_p1 + jja_mean_p2 + jja_mean_p3)/3) %>%
  select(-c(jja_mean_p1, jja_mean_p2, jja_mean_p3)) %>%
  filter(wyear < 2019)

### Next: growing degree days by July 1
# According to Cliff B., GDDs are days with min. temp above 5

# GDD for spring of _that water year_ (april 1 - june 30)
# (earliest veg samples are june ~20, but most samples are july/august)
gdd.spring = daily %>%
  filter(month %in% 4:6) %>%
  group_by(wyear) %>%
  summarise(spring.gdd = sum(min_temp > 5))

# Moving averages of GDD, used to get GDDs of whole growing season in previous
# years.
gdd.allyrs = daily %>%
  group_by(wyear) %>%
  summarise(season.gdd = sum(min_temp > 5)) %>%
  mutate(p1y = wyear + 1,
         p2y = wyear + 2,
         p3y = wyear + 3)

# Now use merging to get previous year's GDDs Note: we don't want year of survey
# (this measurement includes post-survey GDD) so we'll clip those out in
# select(-season.gdd)
rolling.gdds = merge(x = gdd.allyrs %>% select(-c(p1y, p2y, p3y)),
                     y = gdd.allyrs %>% select(-c(wyear, p2y, p3y)),
                     by.x = 'wyear', by.y = 'p1y',
                     suffixes = c('', '_p1')) %>%
  merge(y = gdd.allyrs %>% select(-c(wyear, p1y, p3y)),
        by.x = 'wyear', by.y = 'p2y',
        suffixes = c('', '_p2')) %>%
  merge(y = gdd.allyrs %>% select(-c(wyear, p1y, p2y)),
        by.x = 'wyear', by.y = 'p3y',
        suffixes = c('', '_p3')) %>%
  select(-season.gdd) %>%
  mutate(season.gdd2 = (season.gdd_p1 + season.gdd_p2)/2,
         season.gdd3 = (season.gdd_p1 + season.gdd_p2 + season.gdd_p3)/3) %>%
  rename(season.gdd1 = season.gdd_p1) %>%
  select(-c(season.gdd_p2, season.gdd_p3)) %>%
  filter(wyear < 2019)

### Next: Julian day of start of growing season
# According to Cliff B, growing season can be thought to start when there are
# three consecutive days of min. temperatures above -3. Here, to be
# conservative, I choose four days. I also limited this to January through August,
# to avoid mistakenly picking up warm days in September/October

# diff(cumsum(min_temp > -3), lag = 3) gives the number of days out of the last
# three which have had min_temp above -3; the filter() and distinct() combo
# picks out the very first of these julian days in each year
seas.start = daily %>%
  filter(month %in% 1:8) %>%
  select(wyear, year, month, day, jd, min_temp) %>%
  arrange(wyear, jd) %>%
  group_by(wyear) %>%
  mutate(three.day.lag = c(0, 0, 0, 0, diff(cumsum(min_temp > -3), lag = 4))) %>%
  filter(three.day.lag %in% 3) %>%
  distinct(wyear, .keep_all = TRUE) %>%
  select(wyear, jd, year, month, day)

### Merge ll of these together:

merge(x = rolling.means, y = rolling.gdds, by = 'wyear') %>%
  merge(y = seas.start %>% select(wyear, jd)) %>%
  rename(seas_start_jd = jd) %>%
  write.csv('01_process_data/output/annual_temperatures.csv',
            row.names = FALSE, quote = FALSE)

