# Derive saddle summary statistics associated with snowmelt.
# sn 13 mar 2020

library(ggplot2)
library(dplyr)
library(tidyr)

snow = read.csv('00_raw_data/snowdepth_all/saddsnow.dw.data.csv')

head(snow)
#   LTER_site local_site point_ID       date depth_stake depth_n depth_e depth_s depth_w mean_depth num_meas
# 1       NWT        sdl        1 1992-10-30           8     NaN     NaN     NaN     NaN          8        1
# 2       NWT        sdl        2 1992-10-30           7     NaN     NaN     NaN     NaN          7        1
# 3       NWT        sdl        3 1992-10-30          10     NaN     NaN     NaN     NaN         10        1
# 4       NWT        sdl        4 1992-10-30           7     NaN     NaN     NaN     NaN          7        1
# 5       NWT        sdl        5 1992-10-30           5     NaN     NaN     NaN     NaN          5        1
# 6       NWT        sdl        6 1992-10-30           8     NaN     NaN     NaN     NaN          8        1

nrow(snow)
# [1] 35550

##### Part 1: Some pre-processing

snow = snow %>%
  # Add year, month, day for each entry
  mutate(year = date %>% as.character() %>% strsplit(split = '\\-') %>%
           sapply(function(x) x %>% unlist() %>% (function(y) as.numeric(y[1]))),
         month = date %>% as.character() %>% strsplit(split = '\\-') %>%
           sapply(function(x) x %>% unlist() %>% (function(y) as.numeric(y[2]))),
         dayofmonth = date %>% as.character() %>% strsplit(split = '\\-') %>%
           sapply(function(x) x %>% unlist() %>% (function(y) as.numeric(y[3])))) %>%
  # Convert date into date data type as needed
  mutate(date = as.Date(date)) %>%
  # Add water season, startng in each September
  mutate(season = year + as.numeric(month > 8))

##### Part 1: Get maximum snowdepth

# We had talked about dong this with a spline, but we can presume that during
# the period of year where snow depth would be high (?) that sampling would be
# somewhat regular enough to give us a good proxy. Whatever measurements we see
# here are likely highly correlated with the true maximum depths.

snow.max = snow %>%
  group_by(season, point_ID) %>%
  summarise(max.dep = max(mean_depth, na.rm = TRUE))

head(snow.max)
#   season point_ID max.dep
#     <dbl>    <int>   <dbl>
# 1   1993        1     381
# 2   1993        2     380
# 3   1993        3     342
# 4   1993        4     287
# 5   1993        5     277
# 6   1993        6     245

with(snow.max, table(season, point_ID)) %>% table(useNA = 'always')
# Everything is present here. No NAs.

write.csv(snow.max,
          file = '01_process_data/output/nwt_saddle_max_snowdepth.csv',
          row.names = FALSE)

##### Part 2: Get date of first snow melt.

# This will be tricky due to incomplete sampling in some plots.
# Likely will need splines of some sort to get this done.

# Look into mgcv.