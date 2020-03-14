# Clean up NPP observations
# sn 2020 march 13

library(ggplot2)
library(dplyr)
library(tidyr)

npp = read.csv('00_raw_data/npp/aboveground_npp.hh.data.csv')

head(npp)

# Want only points in the saddle.

npp = npp %>% filter(location %in% 'saddle')

table(npp$plotid)

# Take out plots with not-matching plots.
# It'll suffice here to only take out those which aren't integers.

npp = npp %>% 
  filter(!grepl('\\D|\\_', plotid)) %>%
  mutate(plotid = plotid %>% as.character() %>% as.numeric())

head(npp)
nrow(npp)

with(npp, table(year, plotid))
with(npp %>% filter(year > 1990), table(year, plotid))
with(npp %>% filter(year > 2000), table(year, plotid))

# Note: sampling here is inconsistent, and there are also years with two samples (?)

npp %>% filter(year %in% 2008 & plotid %in% 10)
# There are two "subsamples" here.

# Welp... save this data even if it may not be completely useful to us.

write.csv(npp, row.names = FALSE, file = '01_process_data/output/saddle_npp.csv')
