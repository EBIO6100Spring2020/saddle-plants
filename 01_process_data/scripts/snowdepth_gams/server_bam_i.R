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

sn.gam.i = bam(mean_depth ~ te(jd, seas, by = point_ID,
                               bs = c("cc", "fs"),
                               k = c(12, 8),
                               m = 2) +
                            s(point_ID, bs = 're', k = 8),
               knots = list(jd = c(0, 365)),
               data = sndp, method = 'fREML', discrete = TRUE)

p.backbone = expand.grid(jd = 1:365,
                         seas = unique(sndp$seas),
                         point_ID = unique(sndp$point_ID)) %>%
  mutate(pred = predict(sn.gam.i))


write.csv(p.backbone, row.names = FALSE, file = 'outputs/sndp_bam_i_preds.csv')
