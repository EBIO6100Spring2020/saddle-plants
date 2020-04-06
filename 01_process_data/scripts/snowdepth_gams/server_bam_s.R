library(ggplot2)
library(dplyr)
library(tidyr)
library(mgcv)

setwd('~/poorcast')

snowdep = read.csv('data_inputs/saddsnow.dw.data.csv')

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

sn.gam.s = bam(mean_depth ~ t2(jd, seas, point_ID,
                               bs = c("cc", "re", "re"),
                               k = c(10, 12, 12),
                               m = 2,
                               full = TRUE),
               data = sndp, method = 'fREML', discrete = TRUE)

sndp %>%
  mutate(pred = predict(sn.gam.s)) %>%
  write.csv('ouputs/sndp_bam_s_preds.csv')