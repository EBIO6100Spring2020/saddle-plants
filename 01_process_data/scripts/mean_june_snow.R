# Script for getting mean june snow depth out of the snowdepth data.
# Inconsistencies in the data collection make it impossible to get good
# estimates for snowmelt dates in each plot in each year.
# Instead, take the mean snow levels in June.
# (not every plot-year combo was sampled in April or May)

library(ggplot2)
library(dplyr)
library(tidyr)

# Load in snow data

snowdep = read.csv('00_raw_data/snowdepth_all/saddsnow.dw.data.csv')

# Do processing on snow data.
#   - select relevant columns
#   - get julian day, water year (wyear, year starting on Oct 1.), 
#     and water day (julian day adjusted to start on October 1)
snowdep = snowdep %>%
  select(point_ID, date, mean_depth, num_meas) %>%
  separate(date, into = c('year', 'month', 'day'), sep = '-') %>%
  mutate_at(vars(year, month, day), as.numeric) %>%
  mutate(jd = paste('1970', month, day, sep = '-') %>% as.Date() %>% julian(),
         wyear = year + as.numeric(month > 9),
         wd = jd - 273 + ifelse(wyear == year, 365, 0))

# Get only the relevant years (i.e., years from vegetation sampling
veg.yrs = read.csv('01_process_data/output/veg_all_predictors.csv') %>% 
  filter(year > 1993) %>% 
  distinct(year) %>% 
  unlist()

# Does every plot-year have a measurement in June?
snowdep %>%
  filter(year %in% veg.yrs) %>%
  filter(!is.na(mean_depth)) %>%
  group_by(point_ID, year) %>% 
  summarise(any.jun = any(month %in% 6)) %>%
  apply(2, function(x) sum(!x))
# Yes!

# What if we also include... one year previous?
# Proxy for growing season length in prior year.
snowdep %>%
  filter(year %in% (veg.yrs-1)) %>%
  filter(!is.na(mean_depth)) %>%
  group_by(point_ID, year) %>% 
  summarise(any.jun = any(month %in% 6)) %>%
  apply(2, function(x) sum(!x))
# There is one missing year.

snowdep %>%
  filter(year %in% (veg.yrs-1)) %>%
  filter(!is.na(mean_depth)) %>%
  group_by(point_ID, year) %>% 
  summarise(any.jun = any(month %in% 6)) %>%
  filter(!any.jun)

# What's going on in plot 31, 1994?
snowdep %>%
  filter(wyear %in% 1994 & point_ID %in% 31)
# the Jun 2 measurement is NAN
# look at hte raw data
read.csv('00_raw_data/snowdepth_all/saddsnow.dw.data.csv') %>%
  separate(date, into = c('year', 'month', 'day'), sep = '-') %>%
  filter(year %in% 1994 & point_ID %in% 31)
# uugghaghgh WHY are there NANs, Jesus.

# Okay.
#   - Take the June measurements in relevant years
#   - Filter out NAs and NANs
#   - For each plot-year combo, get
#     - the mean measured snow depth
#     - the number of measurements
#     - the earliest and latest observed days
jun.means = snowdep %>%
  filter(year %in% c(veg.yrs, veg.yrs-1)) %>%
  filter(month %in% 6) %>%
  filter(!is.na(mean_depth)) %>%
  group_by(point_ID, year) %>% 
  summarise(mean.jun.d = mean(mean_depth),
            n.jun.meas = n(),
            max.jun.day = max(day[month %in% 6]),
            min.jun.day = min(day[month %in% 6]))

# How much sampling effort in June?
table(jun.means$n.jun.meas)
mean(jun.means$n.jun.meas)
# very close to e!

ggplot(jun.means) +
  geom_histogram(aes(x = min.jun.day), binwidth = 5)
# there are a handful (7) of plot-years sampled after the 15th
ggplot(jun.means) +
  geom_histogram(aes(x = max.jun.day), binwidth = 5)
# there are a lot of day/years not sampled after the first two weeks of Jun...
# ughghgh this is going to be noisy and arbitrary

ggplot(jun.means) +
  geom_histogram(aes(x = mean.jun.d), binwidth = 10)
# Many many many measurements at zero.
# Might make this a useless predictor.

# How many zeros?
mean(!jun.means$mean.jun.d)
# 41% of measurements are zero.
# hmmm...
# might want to do a log adjustment of sorts.

# Does the first/last day affect measurements?
ggplot(jun.means) +
  geom_point(aes(x = min.jun.day, y = mean.jun.d, size = n.jun.meas),
             alpha = 0.2)
ggplot(jun.means) +
  geom_point(aes(x = max.jun.day, y = mean.jun.d, size = n.jun.meas),
             alpha = 0.2)
# hard to pick out trends here lmao

# Oh well. Roll with it.

write.csv(jun.means, row.names = FALSE,
          file = '01_process_data/output/june_snow_measurements.csv')
