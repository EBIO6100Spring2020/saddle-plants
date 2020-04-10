library(dplyr)
library(tidyr)

setwd('~/poorcast')

rm(list = ls())

# Some code for local testing.
# 
# paste0('outputs/', grep('snowgam_s', dir('gam_s_fits'), value = TRUE))
# 
# for (fin in paste0('gam_s_fits/', grep('snowgam_s', dir('gam_s_fits'), value = TRUE))) {
#   identifier = gsub('outputs\\/snowgam\\_s\\_|\\.csv', '', fin)
#   assign(paste0('fit', identifier), read.csv(fin) %>%
#            mutate(mod = fin))
# }

### Concatenate input data.

# for (fin in read.table('data_inputs/split_input_filenames.txt')$V1) {
#   assign(paste0('data_', fin), 
#          read.csv(paste0('data_inputs/split_data/', fin)) %>%
#            mutate(mod = fin))
# }
# 
# do.call(mget(grep('data_', ls(), value = TRUE)), what = rbind) %>%
#   write.csv(file = 'data_inputs/snowdepth_whole.csv', row.names = FALSE, quote = FALSE)
# 
# rm(list = ls())

### Concatenate gam_s fits

# for (fin in paste0('outputs/', grep('snowgam_s', dir('outputs'), value = TRUE))) {
#   identifier = gsub('outputs\\/snowgam\\_s\\_|\\.csv', '', fin)
#   assign(paste0('fit', identifier), read.csv(fin) %>%
#     mutate(mod = fin))
# }
# 
# ls()
# 
# do.call(mget(grep('fit', ls(), value = TRUE)), what = rbind) %>%
#   write.csv(file = 'outputs/all_gam_s_fits.csv', row.names = FALSE, quote = FALSE)

### Concatenate gam_i fits

for (fin in paste0('outputs/', grep('snowgam_i', dir('outputs'), value = TRUE))) {
  identifier = gsub('outputs\\/snowgam\\_i\\_|\\.csv', '', fin)
  assign(paste0('fit', identifier), read.csv(fin) %>%
    mutate(mod = fin))
}

ls()

do.call(mget(grep('fit', ls(), value = TRUE)), what = rbind) %>%
  write.csv(file = 'outputs/all_gam_i_fits.csv', row.names = FALSE, quote = FALSE)
