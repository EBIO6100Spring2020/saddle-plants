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
spatial.data1 = read.csv('00_raw_data/spatial_physical_data/Point_attributes.csv')

# Another spatial dataset
spatial.data2 = read.csv('HMSC_saddle/saddle_topo_solar.csv')
# I don't know how this was made. It's in the HMSC folder.
# Is this the file Cliff made?
# It doesn't have any NAs and has more dispersion in measurements.

# Maximum snow depth associated with each point, 1993 - 2019
max.snow = read.csv('01_process_data/output/nwt_saddle_max_snowdepth.csv')

# NDVI calculated by Sean
ndvi.data = read.csv('00_raw_data/avg_NDVI_stake_values2.csv')
# I'm not sure what is going on in here.

# NPP (and veg type) data
npp.data = read.csv('01_process_data/output/saddle_npp.csv')

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

##### Process other data frames which need to be processed.

# Not sure if this will be of use, or if this is statistically rigorous, but
# because there are multiple samples of NPP within some of these measurements
# (agh!) take the mean for each plot.

npp.clean = npp.data %>%
  select(veg_class, plotid, year, NPP) %>%
  group_by(plotid, year) %>%
  summarise(mNPP = mean(NPP),
            veg_class = veg_class[1])

head(npp.clean)

# Add suffixes to spatial data frames to distinguish between the two.

# Sean's data frame.
spatial.data1 = spatial.data1 %>%
  select(SDL_GRDP, GPS_HEIGHT, Aspect, Slope) %>%
  rename(plot = SDL_GRDP,
         slope1 = Slope,
         elev1 = GPS_HEIGHT,
         asp1 = Aspect)

# Other data frame
spatial.data2 = spatial.data2 %>%
  select(-c(LONGITUDE., LATITUDE.x)) %>%
  rename(elev2 = GPS_ELEV,
         solar = X2m.Aug.1.S,
         slope2 = Slope,
         asp2 = Aspect)

##### Now, merge in the number of observations with the relevant plot-level predictorss.

veg.n.all = veg.n.obs %>%
  merge(y = spatial.data1,
        by = 'plot',
        all.x = TRUE) %>%
  merge(y = spatial.data2,
        by = 'plot',
        all.x = TRUE) %>%
  merge(y = max.snow, 
        by.x = c("plot", "year"), by.y = c("point_ID", "season"),
        all.x = TRUE) %>%
  merge(y = npp.clean,
        by.x = c('plot', 'year'), by.y = c('plotid', 'year'),
        all.x = TRUE) %>%
  arrange(species, year, plot)

head(veg.n.all)
nrow(veg.n.all)

# Is anything missing?

with(veg.n.all, table(year, plot)) %>%
  as.data.frame() %>%
  group_by(Freq) %>%
  summarise(n = n())

# Exactly one plot is missing.

write.csv(veg.n.all, row.names = FALSE,
          file = '01_process_data/output/veg_all_predictors.csv')

