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
