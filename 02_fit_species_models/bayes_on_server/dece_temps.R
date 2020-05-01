# Script for running rstanarm models on server.

# This script: fit Deschampsia models with temperature variables

library(rstanarm)
library(dplyr)
library(tidyr)

# Set working directory
setwd('~/poorcast/bayesian')

##### Read in data, remove NAs, add slope and aspect
all.sp = read.csv('input/veg_all_predictors.csv') %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  mutate(asp.e = sin(pi * asp2 / 180),
         asp.n = cos(pi * asp2 / 180),
         c.slope = slope2 - mean(slope2))

##### Create training and testing data.

dece.train = all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
dece.valid = all.sp %>% filter(species %in% 'DECE' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

##### Fit models

### With slope and June - August mean over last three years

d.slp.ja3 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + c.slope + jja_mean3 +
                                                   (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 49929,
                       data = dece.train)
print('deschampsia jja3 slope')

# Generate posterior predictions
ds3.pred = posterior_predict(d.slp.ja3, newdata = dece.valid,
                             re.form = ~ (1 | plot),
                             seed = 10009,
                             draws = 4000)

# Generate summary statistics for posterior draws
ds3.pred.summ = ds3.pred %>%
  t() %>%
  as.data.frame() %>%
  mutate(i = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(i)) %>%
  group_by(i) %>%
  summarise(yhat_mean = mean(pred),
            yhat_medn = median(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159)) %>%
  mutate(sp = 'dece', model = 'slope.jja3')

### Without slope but with June - August mean over last three years

d.nsp.ja3 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + jja_mean3 +
                                                   (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 89929,
                       data = dece.train)
print('deschampsia jja3 no slope')

# Generate posterior predictions
dn3.pred = posterior_predict(d.nsp.ja3, newdata = dece.valid,
                             re.form = ~ (1 | plot),
                             seed = 10009,
                             draws = 4000)

# Generate summary statistics for posterior draws
dn3.pred.summ = dn3.pred %>%
  t() %>%
  as.data.frame() %>%
  mutate(i = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(i)) %>%
  group_by(i) %>%
  summarise(yhat_mean = mean(pred),
            yhat_medn = median(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159)) %>%
  mutate(sp = 'dece', model = 'noslope.jja3')

### With slope and with June - August mean in just the last year

d.slp.ja1 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + c.slope + jja_mean1 +
                                                   (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 220156,
                       data = dece.train)
print('deschampsia jja1 slope')

# Generate posterior predictions
ds1.pred = posterior_predict(d.slp.ja1, newdata = dece.valid,
                             re.form = ~ (1 | plot),
                             seed = 80109,
                             draws = 4000)

# Generate summary statistics for posterior draws
ds1.pred.summ = ds1.pred %>%
  t() %>%
  as.data.frame() %>%
  mutate(i = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(i)) %>%
  group_by(i) %>%
  summarise(yhat_mean = mean(pred),
            yhat_medn = median(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159)) %>%
  mutate(sp = 'dece', model = 'slope.jja1')

### With no slope but with June - August mean in just the last year

d.nsp.ja1 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + jja_mean1 +
                                                   (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 651002,
                       data = dece.train)
print('deschampsia jja1 no slope')

# Generate posterior predictions
dn1.pred = posterior_predict(d.nsp.ja1, newdata = dece.valid,
                             re.form = ~ (1 | plot),
                             seed = 445501,
                             draws = 4000)

# Generate summary statistics for posterior draws
dn1.pred.summ = dn1.pred %>%
  t() %>%
  as.data.frame() %>%
  mutate(i = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(i)) %>%
  group_by(i) %>%
  summarise(yhat_mean = mean(pred),
            yhat_medn = median(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159)) %>%
  mutate(sp = 'dece', model = 'noslope.jja1')

write.csv(rbind(ds3.pred.summ %>% 
                  mutate(loglik = log_lik(d.slp.ja3) %>% apply(1, sum) %>% mean()), 
                dn3.pred.summ %>% 
                  mutate(loglik = log_lik(d.nsp.ja3) %>% apply(1, sum) %>% mean()), 
                ds1.pred.summ %>% 
                  mutate(loglik = log_lik(d.slp.ja1) %>% apply(1, sum) %>% mean()), 
                dn1.pred.summ %>% 
                  mutate(loglik = log_lik(d.nsp.ja1) %>% apply(1, sum) %>% mean())),
          row.names = FALSE,
          file = 'output/dece_temp_summary.csv')

save(d.slp.ja3, d.nsp.ja3, d.slp.ja1, d.nsp.ja1,
     file = 'dece_temp_mods.RData')
