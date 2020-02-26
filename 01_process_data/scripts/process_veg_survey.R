# Script to process NWT saddle data to make it usable for analysis.
# february 22, 2020 - 
# scott

# Load in some good packages.
library(ggplot2)
library(dplyr)
library(tidyr)

# Load in data

veg.raw = read.csv('00_raw_data/vegetation_sampling/saddptqd.hh.data.csv')

head(veg.raw)
nrow(veg.raw)

# Remind me... what were the distributions of hits across years?
# Something was weird.

veg.raw %>%
  group_by(year, hit_type) %>%
  summarise(n.hits = n()) %>%
  ggplot() +
  geom_line(aes(x = year, y = n.hits, group = hit_type, colour = hit_type)) +
  geom_point(aes(x = year, y = n.hits, colour = hit_type))

# Many bottom hits.
# I think there were some cases of bottom hits without top?

veg.ht.by.year = veg.raw %>%
#  filter(hit_type %in% c('top', 'bottom')) %>%
  group_by(year, hit_type, point) %>%
  summarise(n.hits = n()) %>%
  ungroup() %>%
  spread(hit_type, n.hits) %>%
  mutate_if(is.integer, function(x) ifelse(is.na(x), 0, x))

head(veg.ht.by.year)

# How many top hits are missing?
sum(!veg.ht.by.year$top)

# When and where do these occur?
veg.ht.by.year %>%
  filter(!top) %>%
  group_by(year) %>%
  summarise(n = n())
# Oh? No top hit in, like, every year??

# How many missing bottom records are there...?
veg.ht.by.year %>%
  filter(is.na(bottom)) %>%
  group_by(year) %>%
  summarise(n = n())
# Just two?

# Let's get freaky and look at combos.

veg.ht.by.year %>%
  mutate(hits = paste0(top, middle1, middle2, bottom)) %>%
  group_by(year, hits) %>%
  summarise(n = n()) %>%
  spread(hits, n) %>%
  mutate_if(is.integer, function(x) ifelse(is.na(x), 0, x))
# Okay, hierarchy is:
# First bottom filled in, then top, then mid1, then mid two

veg.bottom.only.pts = veg.ht.by.year %>%
  filter(bottom > 0 & !top) %>%
  select(year, point)

# veg.tb is veg top and botom (should be top only lulz)
veg.tb = veg.raw %>%
  # get rid of 1996 (close to no sampling done this year)
  filter(!year %in% 1996) %>%
  # Isolate "top" hits OR get those point-years with only bottom hits
  # (as these should be top hits)
  filter(paste0(year, point) %in% with(veg.bottom.only.pts, paste0(year, point)) | 
         hit_type %in% 'top') %>%
  # Add a new column saying that the CORRECTED hit type is 'top'
  mutate(hit_corr = 'top') %>%
  # Rename the hit_type column to the ORIGINAL hit
  rename(hit_orig = hit_type)

nrow(veg.tb)
head(veg.tb)

with(veg.tb, table(year))

# Write a CSV with all hits
write.csv(veg.tb, row.names = FALSE,
          file = '01_process_data/output/veg_all_top_sans96.csv')

# Write a CSV with only non-lichen plants
# (USDA codes for lichens or no biotic material start with a 2)
veg.tb %>%
  filter(!grepl('^2', USDA_code)) %>%
  write.csv(row.names = FALSE,
            file = '01_process_data/output/veg_plants_top_sans96.csv')

# Write a CSV with only our focal species
veg.tb %>%
  filter(USDA_code %in% c('GEROT', 'KOMY', 'DECE')) %>%
  write.csv(row.names = FALSE,
            file = '01_process_data/output/veg_focals_top_sans96.csv')

# Which points were sampled at all? This is needed for a binomial model.

veg.tb %>%
  distinct(year, plot, point) %>%
  write.csv(row.names = FALSE,
            file = '01_process_data/output/veg_points_sampled_all.csv')

