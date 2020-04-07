library(ggplot2)
library(dplyr)
library(tidyr)

setwd('01_process_data/scripts/snowdepth_gams/')

rm(list = ls())

### Evaluating a bam

origsnd = read.csv('split_data/a1.csv')
bamfits = read.csv('bam_s_fits/snowbam_s_a1.csv')

head(bamfits, 20)

bamfits %>%
  filter(point_ID %in% 7) %>%
  ggplot() +
  geom_line(aes(x = jd, y = pred, group = seas)) +
  facet_wrap(~ seas) +
  geom_line(aes(x = jd, y = mean_depth, group = seas),
            data = origsnd %>% filter(point_ID %in% 7),
            colour = 'purple')

# ah it's the discreteness

### Evaluating a gam

origsnd = read.csv('split_data/a1.csv')
gamfits = read.csv('gam_s_fits/snowgam_s_a1.csv')

head(gamfits, 20)

gamfits %>%
  filter(point_ID %in% 7) %>%
  ggplot() +
  geom_line(aes(x = jd, y = pred, group = seas)) +
  facet_wrap(~ seas) +
  geom_line(aes(x = jd, y = mean_depth, group = seas),
            data = origsnd %>% filter(point_ID %in% 7),
            colour = 'purple')

gamfits %>%
  filter(point_ID %in% 101) %>%
  ggplot() +
  geom_line(aes(x = jd, y = pred, group = seas)) +
  facet_wrap(~ seas) +
  geom_line(aes(x = jd, y = mean_depth, group = seas),
            data = origsnd %>% filter(point_ID %in% 101),
            colour = 'purple')
# 101 is not great...

origsnd = read.csv('split_data/f2.csv')
gamfits = read.csv('gam_s_fits/snowgam_s_f2.csv')

head(gamfits, 20)

gamfits %>%
  filter(point_ID %in% 15) %>%
  ggplot() +
  geom_line(aes(x = jd, y = pred, group = seas)) +
  facet_wrap(~ seas) +
  geom_line(aes(x = jd, y = mean_depth, group = seas),
            data = origsnd %>% filter(point_ID %in% 15),
            colour = 'purple')
# that weird stuff at the end... uhhh
# add a zero?

gamfits %>%
  filter(point_ID %in% 69) %>%
  ggplot() +
  geom_line(aes(x = jd, y = pred, group = seas)) +
  facet_wrap(~ seas) +
  geom_line(aes(x = jd, y = mean_depth, group = seas),
            data = origsnd %>% filter(point_ID %in% 69),
            colour = 'purple')
# this just isn't catching the peaks.
