# Script for running rstanarm models on server.

# This script: test run of a model with aspe

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

komy.train = all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
komy.valid = all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

gero.train = all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
gero.valid = all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

##### Fit models

### Deschampsia

dece.e = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + (1 | plot) + (1 | year) + (1 | obsno),
                    family = 'binomial',
                    cores = 4,
                    seed = 3009210,
                    data = dece.train)
print('deschampsia')

# Generate posterior predictions
dece.pred = posterior_predict(dece.e, newdata = dece.valid,
                              re.form = ~ (1 | plot),
                              seed = 3515,
                              draws = 4000)

# Generate summary statistics for posterior draws
dece.pred.summ = dece.pred %>%
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
  mutate(sp = 'dece', model = 'aspe')

### Kobresia

komy.e = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + (1 | plot) + (1 | year) + (1 | obsno),
                    family = 'binomial',
                    cores = 4,
                    seed = 734392,
                    data = komy.train)
print('kobresia')

# Generate posterior predictions
komy.pred = posterior_predict(komy.e, newdata = komy.valid,
                              re.form = ~ (1 | plot),
                              seed = 19899,
                              draws = 4000)

# Generate summary statistics for posterior draws
komy.pred.summ = komy.pred %>%
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
  mutate(sp = 'komy', model = 'aspe')

### Geum

gero.e = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + (1 | plot) + (1 | year) + (1 | obsno),
                    family = 'binomial',
                    cores = 4,
                    seed = 55432,
                    data = gero.train)
print('geum')

# Generate posterior predictions
gero.pred = posterior_predict(gero.e, newdata = gero.valid,
                              re.form = ~ (1 | plot),
                              seed = 99,
                              draws = 4000)

# Generate summary statistics for posterior draws
gero.pred.summ = gero.pred %>%
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
  mutate(sp = 'gero', model = 'aspe')

write.csv(rbind(dece.pred.summ %>%
                  mutate(loglik = log_lik(dece.e) %>% apply(1, sum) %>% mean()), 
                komy.pred.summ %>%
                  mutate(loglik = log_lik(komy.e) %>% apply(1, sum) %>% mean()), 
                gero.pred.summ %>%
                  mutate(loglik = log_lik(gero.e) %>% apply(1, sum) %>% mean())),
          row.names = FALSE,
          file = 'output/all_aspe_summary.csv')

save(dece.e, komy.e, gero.e, file = 'output/e_mods.RData')
