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
  filter(!is.nan(mean_depth) | !is.na(mean_depth)) %>%
  mutate(point_ID = factor(point_ID),
         seas = factor(seas))

unique(sndp$point_ID)
unique(sndp$seas)

set.seed(04062020)

plot.assign = data.frame(i = 1:88,
                         plot = sample(unique(sndp$point_ID))) %>%
  mutate(poins = 1 + (i-1) %/% 8,
         lettr = letters[poins])

seas.assign = data.frame(i = 1:27,
                         seas = sample(unique(sndp$seas))) %>%
  mutate(digit = 1 + (i-1) %/% 9)

for (lettr in unique(plot.assign$lettr)) {
  for (dig in unique(seas.assign$digit)) {
    sndp %>%
      filter(point_ID %in% plot.assign$plot[plot.assign$lettr %in% lettr]) %>%
      filter(seas %in% seas.assign$seas[seas.assign$digit %in% dig]) %>%
      write.csv(paste0(
        '01_process_data/scripts/snowdepth_gams/split_data/',
        lettr, dig, '.csv'),
        row.names = FALSE)
  }
}


expand.grid(l = unique(plot.assign$lettr),
            d = unique(seas.assign$digit)) %>%
  apply(1, FUN = function(x) paste0(x['l'], x['d'], '.csv')) %>%
  write.table('01_process_data/scripts/snowdepth_gams/split_input_filenames.txt', 
              col.names = FALSE, row.names = FALSE, quote = FALSE)

