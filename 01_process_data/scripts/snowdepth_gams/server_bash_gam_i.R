#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (!length(args)) { stop("At least one argument must be supplied (input file).\n", call.=FALSE)
} else if (length(args) %in% 1) { filename = args[1] }

library(ggplot2)
library(dplyr)
library(tidyr)
library(mgcv)

setwd('~/poorcast')

sndp = read.csv(paste0('data_inputs/split_data/', filename)) %>%
  mutate(seas = factor(seas),
         point_ID = factor(point_ID))

sn.gam.i = gam(mean_depth ~ te(jd, seas, by = point_ID,
                               bs = c("cc", "fs"),
                               k = c(12, 10),
                               m = 2) +
                 s(point_ID, bs = 're', k = 10),
               knots = list(jd = c(0, 365)),
               data = sndp, method = 'REML')

p.backbone = expand.grid(jd = 1:365,
                         seas = unique(sndp$seas),
                         point_ID = unique(sndp$point_ID)) %>%
  mutate(pred = predict(sn.gam.i, .))


write.csv(p.backbone, row.names = FALSE, 
          file = paste0('outputs/snowgam_i_', filename))
