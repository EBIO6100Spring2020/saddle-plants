# Put together a master data frame with appropriate predictors.
# NOTE: script is not complete because we're still processing some variables.
# sn 2020 13 march

library(ggplot2)
library(dplyr)
library(tidyr)

##### Read in data.

# Do this with only the data on focal species.
veg.focs = read.csv('01_process_data/output/veg_focals_top_sans96.csv')

# Spatial data associated with each plot
# (note: these are assumed to not change over time)
spatial.data = read.csv('00_raw_data/avg_NDVI_stake_values2.csv')

# Maximum snow depth associated with each point, 1993 - 2019
max.snow = read.csv('01_process_data/output/nwt_saddle_max_snowdepth.csv')

##### Generate plot-level summary statistics

# Good way to model the density of species at each plot
# is a binomial model, with 100 trials (one for each point in the saddle)
# and successes as the number of points where the focal species was present.

# Note this ignores spatial structure within plots... this may underestimate
# error in our measurements. We may want to deal with that later.

# Collapsing species down into the number of observations per plot will leave
# out zeros. Need to have a list of all point-year sampling combinations for
# filling in zeros.

# Here is a list of all points sampled, ever.
pts.all = read.csv('01_process_data/output/veg_points_sampled_all.csv')

# Ensure that each plot which was sampled was sampled 100 times
pts.all %>%
  group_by(year, plot) %>%
  summarise(n = n()) %>%
  group_by(n) %>%
  summarise(n.meas = n())
# every plot in here was completely surveyed (i.e., every point has an
# observation, i.e., number of trials for each plot-year is 100)

# Take the above data frame and fill it in with zeros for each species.
# Doing this ensures that any plot without an observation of a species
# will get a zero in the count of observations in each case.
pts.all = pts.all %>%
  mutate(DECE = 0,
         KOMY = 0,
         GEROT = 0) %>%
  gather(key = species, value = n.obs, -c(year, plot, point))

# Now, for each species, in each plot, get a number of observations (out of 100)
# Do this by first summarizing number of observations of each species in each plot
# After doing this, concatenate that with the above data frame of zeros;
# Take a sum of observations across each plot-year combo;
# if a plot has a non-zero number of observations, the zero in the above data frame
# will not influence the count.

veg.n.obs = veg.focs %>%
  rename(species = USDA_code) %>%
  group_by(species, year, plot) %>%
  summarise(n.obs = n()) %>%
  as.data.frame() %>%
  rbind(pts.all %>%
          mutate(species = factor(species),
                 n.obs = as.integer(n.obs)) %>%
          select(species, year, plot, n.obs)) %>%
  group_by(species, year, plot) %>%
  summarise(n.obs = sum(n.obs))

##### Now, merge in the number of observations with the relevant plot-level predictorss.
