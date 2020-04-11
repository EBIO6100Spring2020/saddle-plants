library(ggplot2)
library(dplyr)
library(tidyr)

weath = read.csv("~/Downloads/knb-lter-nwt.413.10/sdltdayv.ml.data.csv")

nrow(weath)

head(weath)

weath = read.csv("~/Downloads/knb-lter-nwt.413.10/sdltdayv.ml.data.csv") %>%
  mutate(year = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[1] %>% as.numeric()),
         mnth = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[2] %>% as.numeric()),
         dymn = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[3] %>% as.numeric()),
         seas = year + as.numeric(mnth > 9),
         date.origin = as.Date.character(paste0(year, '-10-01'), format = '%Y-%m-%d'),
         i = 1:nrow(.)) %>%
  group_by(i) %>%
  mutate(jd = date %>% 
           as.Date('%Y-%m-%d') %>% 
           julian.Date(origin = date.origin) %>%
           (function(x) x[1]),
         jd = jd + ifelse(jd < 0, 365 + as.numeric(year %% 4), 0)) %>%
  ungroup() %>%
  select(-c(i, date.origin))

head(weath)
# No plot substructure.

# These are daily records!
weath %>% group_by(year) %>% summarise(n.obs = n()) %>% print(n = 30)

weath %>%
  ggplot() +
  geom_line(aes(x = jd, y = airtemp_avg, group = seas), size = 0.1)

weath.c = weath %>%
  group_by(jd) %>%
  mutate(c.mean = airtemp_avg - mean(airtemp_avg, na.rm = TRUE))

weath.c %>%
  filter(year > 1993) %>%
  ggplot() +
  geom_segment(x = 0, xend = 365, y = 0, yend = 0, colour = 'blue') +
  geom_line(aes(x = jd, y = c.mean, group = seas), size = 0.4) +
  facet_wrap(~ seas)

lm(c.mean ~ year, weath.c) %>% summary() # trend of increasing over time (weak)

weath %>%
  filter(seas > 1993) %>%
  ggplot() +
  geom_line(aes(x = jd, y = airtemp_avg, group = seas), size = 0.1) +
  geom_segment(x = 0, xend = 365, y = 0, yend = 0, colour = 'blue') +
  facet_wrap(~ seas)

# THIS data could be splined very easily.

weath.thaw = weath %>%
  group_by(seas) %>%
  mutate(thaw = airtemp_avg > 0 & jd > 100,
         thaw = ifelse(is.na(thaw), FALSE, thaw),
         cuml.thaw = cumsum(thaw))

weath.thaw %>%
  filter(seas > 1993) %>%
  ggplot() +
  geom_line(aes(x = jd, y = cuml.thaw, group = seas))

weath.thaw %>%
  filter(cuml.thaw %in% 5) %>%
  distinct(seas, .keep_all = TRUE) %>%
  ggplot() +
  geom_histogram(aes(x = jd, fill = seas), binwidth = 1)

#### 

# Advice from Cliff:

# Growing season starts (?) with three consec. days of min. temp above -3C

weath %>%
  select(-c(LTER_site, local_site, flag_airtemp_max, flag_airtemp_min)) %>%
  filter(seas > 1981) %>%
  arrange(seas, jd) %>%
  group_by(seas) %>%
  mutate(a3 = airtemp_min > -3,
         c3 = cumsum(a3),
         d1 = c(0, diff(c3)),
         d2 = c(0, diff(d1)),
         d3 = c(0, diff(d2)),
         flag = d1 & d2 & d3) %>% View()

# This is hard to do with a dplyr chain.

# Growing degree days is number of days with minimum daily temperature above 5deg C

wggdn = weath %>%
  filter(seas > 1981) %>%
  arrange(seas, jd) %>%
  group_by(seas) %>%
  mutate(gddn = cumsum(airtemp_min > 5)) %>%
  ungroup()

ggplot(wggdn) +
  geom_line(aes(x = jd, y = gddn, group = seas))

# There are NAs here. There's also another data file.


##### Try to merge together varios datas.

# loggr is new data

loggr = read.csv('00_raw_data/temp_new_loggers/sdlcr23x-cr1000.daily.ml.data.csv')

head(loggr)

table(loggr$LTER_site)
table(loggr$local_site)
table(loggr$logger)

loggr = loggr %>%
  select(-c(LTER_site, local_site)) %>%
  mutate(year = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[1] %>% as.numeric()),
         mnth = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[2] %>% as.numeric()),
         dymn = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[3] %>% as.numeric()),
         seas = year + as.numeric(mnth > 9),
         jdseas = jday - (274 + as.numeric(!year %% 4)),
         jdseas = jdseas + ifelse(jdseas < 0, 365, 0))

table(loggr$jdseas)

# Plot: relative humiditiy

loggr %>%
  group_by(seas) %>%
  mutate(ann_mean_rhs = mean(rh_avg, na.rm = TRUE)) %>%
  ggplot(aes(x = jdseas)) +
  geom_segment(aes(x = 0, xend = 365,
                   y = ann_mean_rhs,
                   yend = ann_mean_rhs),
               colour = 'purple') +
  geom_line(aes(y = rh_avg), size = 0.2) +
  geom_ribbon(aes(ymin = rh_min, ymax = rh_max),
              alpha = 0.2) +
  facet_wrap(~ seas)

# Not much annual difference in relative humidity.

# Look at barometric pressure

loggr %>%
  group_by(seas) %>%
  mutate(ann_mean_bp = mean(bp_avg, na.rm = TRUE)) %>%
  ggplot(aes(x = jdseas)) +
  geom_segment(aes(x = 0, xend = 365,
                   y = ann_mean_bp,
                   yend = ann_mean_bp),
               colour = 'purple') +
  geom_line(aes(y = bp_avg), size = 0.2) +
  geom_ribbon(aes(ymin = bp_min, ymax = bp_max),
              alpha = 0.2) +
  facet_wrap(~ seas)

# Not much annual difference in BP.

# Look at solar radiation

loggr %>%
  group_by(seas) %>%
  mutate(ann_mean_rad = mean(solrad_tot, na.rm = TRUE)) %>%
  ggplot(aes(x = jdseas)) +
  geom_segment(aes(x = 0, xend = 365,
                   y = ann_mean_rad,
                   yend = ann_mean_rad),
               colour = 'purple') +
  geom_line(aes(y = solrad_tot), size = 0.2) +
  facet_wrap(~ seas)

# Detrend

loggr %>%
  group_by(jdseas) %>%
  mutate(dtrnd_mean_rad = solrad_tot - mean(solrad_tot, na.rm = TRUE)) %>%
  group_by(seas) %>%
  mutate(ann_mean_rad = mean(dtrnd_mean_rad[jdseas %in% 200:350], na.rm = TRUE)) %>%
  ggplot(aes(x = jdseas)) +
  geom_line(aes(y = dtrnd_mean_rad, group = seas), size = 0.25) +
  geom_segment(aes(x = 200, xend = 350, 
                   y = ann_mean_rad,
                   yend = ann_mean_rad),
               colour = 'purple') +
  facet_wrap(~ seas)

# No real evidence for differences in solar radiation  