# Script for running rstanarm models on server.

# This script: runn all models with Chlorine

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

dece.lxsn = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + jja_mean1 + pH + log(1+max.dep) +
                         (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 55653,
                       data = dece.train)
print('deschampsia')

# Generate posterior predictions
dece.lxsn.pred = posterior_predict(dece.lxsn, newdata = dece.valid,
                                   re.form = ~ (1 | plot),
                                   seed = 51020,
                                   draws = 4000)

# Generate summary statistics for posterior draws
dece.pred.summ = dece.lxsn.pred %>%
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
  mutate(sp = 'dece', model = 'log.max.snow')

### Kobresia

komy.lxsn = stan_glmer(cbind(n.obs, 100 - n.obs) ~ Total_N + pH + log(1+max.dep) +
                         (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 513610,
                       data = komy.train)
print('kobresia')

# Generate posterior predictions
komy.lxsn.pred = posterior_predict(komy.lxsn, newdata = komy.valid,
                                   re.form = ~ (1 | plot),
                                   seed = 5408,
                                   draws = 4000)

# Generate summary statistics for posterior draws
komy.pred.summ = komy.lxsn.pred %>%
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
  mutate(sp = 'komy', model = 'log.max.snow')

### Geum

gero.lxsn = stan_glmer(cbind(n.obs, 100 - n.obs) ~ log(1+max.dep) + (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 51025,
                       data = gero.train)
print('geum')

# Generate posterior predictions
gero.lxsn.pred = posterior_predict(gero.lxsn, newdata = gero.valid,
                                   re.form = ~ (1 | plot),
                                   seed = 563000,
                                   draws = 4000)

# Generate summary statistics for posterior draws
gero.pred.summ = gero.lxsn.pred %>%
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
  mutate(sp = 'gero', model = 'log.max.snow')

write.csv(rbind(dece.pred.summ %>% 
                  mutate(loglik = log_lik(dece.lxsn) %>% apply(1, sum) %>% mean()), 
                komy.pred.summ %>% 
                  mutate(loglik = log_lik(komy.lxsn) %>% apply(1, sum) %>% mean()), 
                gero.pred.summ %>% 
                  mutate(loglik = log_lik(gero.lxsn) %>% apply(1, sum) %>% mean())),
          row.names = FALSE,
          file = 'output/all_log_max_snow_summary.csv')

save(dece.lxsn, komy.lxsn, gero.lxsn, file = 'output/all_log_max_snow.RData')
