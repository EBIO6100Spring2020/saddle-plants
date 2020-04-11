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

# So: we have a 1.2 - 1.5 degree discrepancy between the old temperatures and the new ones.


# Look at low temperatures:


ggplot(t.comb.compare) +
  geom_point(aes(x = airtemp_min_old, y = airtemp_min_new), 
             alpha = 0.5) +
  geom_segment(aes(x = -20, y = -20,
                   xend = 10, yend = 10),
               colour = 'gray55') +
  facet_wrap(~ year)

lamin = rstanarm::stan_glmer(airtemp_min_new ~ airtemp_min_old + (1 | year),
                             family = 'gaussian', cores = 4,
                             data = t.comb.compare %>% mutate(year = factor(year)),
                             seed = 7709210)

summary(lamin)

# we have 

lamax = rstanarm::stan_glmer(airtemp_max_new ~ airtemp_max_old + (1 | year),
                             family = 'gaussian', cores = 4,
                             data = t.comb.compare %>% mutate(year = factor(year)),
                             seed = 31210110)
summary(lamax)

# max temp is 2.5 degrees higher on the other loggers!

# Will need to run the modls "backwards" to get 'old' loggers


# Add julian date information.


write.csv(t.comb, row.names = FALSE,
          file = '01_process_data/output/all_airtemp.csv')
