# Exploring nitrogen/nutrient data from NADP

library(ggplot2)
library(dplyr)
library(tidyr)

nutri = read.csv('00_raw_data/nitrogen/Niwot_water_year_concentration_data.csv')

head(nutri)

# Take out year 1984 (incomplete)
nutri = nutri[nutri$yr > 1984,]

ggnut = ggplot(nutri)

ggnut +
  geom_line(aes(x = yr, y = Ca), colour = 'red') +
  geom_line(aes(x = yr, y = Total_N), colour = 'blue') +
  geom_line(aes(x = yr, y = K), colour = 'darkgreen') +
  geom_line(aes(x = yr, y = Mg), colour = 'gold')

# Try centeirng

nutri.c = nutri %>%
  mutate_at(vars(Ca, Mg, K, Na, NH4, NO3, Total_N, Cl, SO4, pH, conduc, svol, ppt),
            function(x) (x - mean(x)) / sd(x))

ggnut = ggplot(nutri.c, aes(x = yr))

ggnut +
  geom_line(aes(y = Ca), colour = 'red') +
  geom_line(aes(y = Total_N), colour = 'blue') +
  geom_line(aes(y = K), colour = 'darkgreen') +
  geom_line(aes(y = Mg), colour = 'gold') +
  geom_line(aes(y = pH), colour = 'orange') +
  geom_line(aes(y = Na), colour = 'purple') +
  theme_bw()


# Principle components analysis



# Make a long-form data frame
nutri.l = nutri %>%
  select(-c(siteID, seas, fullChemLab, daysSample, startDate, lastDate)) %>%
  gather(key = metric, value = measure, -c(yr, Criteria1, Criteria2, Criteria3))

head(nutri.l)

nutri.l = nutri.l %>%
  gather(key = crit, value = pct, -c(yr, metric, measure)) %>%
  filter(!(crit %in% c('Criteria2', 'Criteria3') & !(metric %in% c('ppt', 'svol')))) %>%
  filter(!(crit %in% 'Criteria1' & metric %in% c('ppt', 'svol')))

# why did I want to do this...?

# Look at correlation

nutri.cor = nutri %>%
  select(Ca, Mg, K, Na, NH4, NO3, Total_N, Cl, SO4, pH, conduc, ppt) %>%
  cor()

hist(nutri.cor[nutri.cor<1])

nutri.cor.long = nutri.cor %>%
  as.data.frame() %>%
  mutate(meas.a = row.names(.)) %>%
  gather(key = meas.b, value = corr, -c(meas.a)) %>%
  filter(corr < 1)

nutri.cor.long %>%
  filter(abs(corr) > 0.25)

nutri.cor.long %>%
  filter(meas.a %in% 'Total_N')

# Plot: Soil carbon, Na, Cl, pH

ggnut +
  geom_line(aes(y = Total_N), colour = 'red') +
  geom_line(aes(y = Ca), colour = 'blue')
# Seem to be tracking each other especially well 1990 - 2005

ggnut +
  geom_line(aes(y = Total_N), colour = 'red') +
  geom_line(aes(y = Cl), colour = 'gold')
# Not correlated at all really

ggnut +
  geom_line(aes(y = Total_N), colour = 'red') +
  geom_line(aes(y = pH), colour = 'purple')
# Also not correlated

nut.ph.long = nutri.c %>%
  select(-c(Criteria1, Criteria2, Criteria3, siteID, seas, 
            fullChemLab, daysSample, startDate, lastDate)) %>%
  gather(key = nutr, value = meas, -c(yr, pH))

ggplot(nut.ph.long %>% filter(!nutr %in% 'Br'), aes(x = pH)) +
  geom_line(aes(y = meas, group = nutr, colour = nutr), size = 1.2) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = 12, 'Set3')) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = 'gray44'))

ggplot(nut.ph.long %>% filter(!nutr %in% 'Br'), aes(x = pH, y  = meas)) +
  geom_smooth(method = 'lm', colour = 'gray88') +
  geom_point(aes(colour = nutr), size = 1.2) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = 12, 'Set3')) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = 'gray44'),
        legend.position = 'none') +
  facet_wrap(~ nutr)
