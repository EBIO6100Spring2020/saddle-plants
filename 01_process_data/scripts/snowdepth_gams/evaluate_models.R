library(ggplot2)
library(dplyr)
library(tidyr)

setwd('01_process_data/scripts/snowdepth_gams/')

rm(list = ls())

### Evaluating a bam

orig.snd = read.csv('snowdepth_whole.csv')
gam.fits = read.csv('all_gam_s_fits.csv') %>%
  filter(seas %in% unique(orig.snd$seas))

nrow(orig.snd)
nrow(gam.fits)
# great googly moogly that's a lot

# okay let's try some stuff.
all.fits = merge(x = orig.snd %>% mutate(mod = gsub('outputs\\/snowgam\\_s\\_', '', mod)),
                 y = gam.fits %>% mutate(mod = gsub('outputs\\/snowgam\\_s\\_', '', mod)),
                 by = c('point_ID', 'seas', 'jd', 'mod'),
                 all.x = TRUE, all.y = TRUE)

head(all.fits, 20)
table(all.fits$seas)

# Plot 1: try residuals

resid.df = all.fits %>%
  filter(!is.na(mean_depth)) %>%
  mutate(resid = mean_depth - pred)

# resid > 0: model underestimates
# resid < 0: model overestimates


resid.df %>%
  ggplot() +
  geom_line(aes(x = jd, y = resid, 
                group = interaction(point_ID, seas),
                colour = mod),
            alpha = 0.2) +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_line('gray99')) +
  facet_wrap(~ mod)
# Not great. Varies by model.

resid.df %>%
  group_by(mod) %>%
  summarise(resid.sq = sum(resid^2) / n()) %>%
  print(n = 33)

# k-models are not good.
# other models apper to be fine.

resid.df %>%
  ggplot(aes(x = mean_depth, y = resid)) +
  geom_segment(aes(x = 0, xend = 600, y = 0, yend = 0),
               colour = 'gray44') +
  geom_point(aes(colour = mod),
            size = 0.5, alpha = 0.4) +
  geom_smooth(method = 'lm') +
  labs(x = 'Snow depth', y = 'Residual') +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_line('gray99')) +
  facet_wrap(~ mod, nrow = 5)

# Underestimates when low, overestimates when hgh.

resid.df %>%
  ggplot(aes(x = mean_depth, y = resid)) +
  geom_segment(aes(x = 0, xend = 600, y = 0, yend = 0),
               colour = 'gray44') +
  geom_point(aes(colour = mod),
             alpha = 0.2,
             position = position_jitter(width = 2)) +
  geom_smooth(method = 'lm') +
  labs(x = 'Snow depth', y = 'Residual') +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_line('gray99'))

## Evaluating first melt date depths.

all.fits %>%
  arrange(point_ID, seas, jd) # %>%
  # Next: look at shit.

gg.date = ggplot(first.dates)

gg.date +
  geom_histogram(aes(x = jd),
                 binwidth = 1)

gg.date +
  geom_histogram(aes(x = jd),
                 binwidth = 1)

### Old junk.

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

misssnd = origsnd %>%
  group_by(point_ID, seas) %>%
  summarise(zero.days = sum(!mean_depth)) %>%
  filter(!zero.days)

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

all.fits %>%
  filter(seas > 1995) %>%
  filter(point_ID %in% 35) %>%
  ggplot() +
  geom_point(aes(x = jd, y = mean_depth), colour = 'purple') +
  geom_line(aes(x = jd, y = pred, group = seas)) +
  geom_line(aes(x = jd, y = mean_depth, group = seas),
            colour = 'purple') +
  facet_wrap(~ seas)
