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

komy.train = all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
komy.valid = all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

gero.train = all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015) %>% mutate(obsno = 1:nrow(.))
gero.valid = all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018) %>% mutate(obsno = 1:nrow(.))

##### Fit models

### Kobresia with easting and June - Aug mean in last two years

k.e.j2 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ asp.e + jja_mean2 +
                                                (1 | plot) + (1 | year) + (1 | obsno),
                    family = 'binomial',
                    cores = 4,
                    seed = 92994,
                    data = komy.train)

print('kobresia jja2 w/ easting')

# Generate posterior predictions
kej2.pred = posterior_predict(k.e.j2, newdata = komy.valid,
                              re.form = ~ (1 | plot),
                              seed = 90001,
                              draws = 4000)

# Generate summary statistics for posterior draws
kej2.pred.summ = kej2.pred %>%
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
  mutate(sp = 'komy', model = 'easting.jja2')

### Kobresia model with out easting but with June - August means over last two years

k.j2 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ jja_mean2 + (1 | plot) + (1 | year) + (1 | obsno),
                  family = 'binomial',
                  cores = 4,
                  seed = 100239,
                  data = komy.train)
print('kobresia jja2 no easting')

# Generate posterior predictions
k.j2.pred = posterior_predict(k.j2, newdata = komy.valid,
                              re.form = ~ (1 | plot),
                              seed = 44310,
                              draws = 4000)

# Generate summary statistics for posterior draws
k.j2.pred.summ = k.j2.pred %>%
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
  mutate(sp = 'komy', model = 'noeasting.jja2')

### Geum model with jja 3

g.j3 = stan_glmer(cbind(n.obs, 100 - n.obs) ~ jja_mean3 + (1 | plot) + (1 | year) + (1 | obsno),
                  family = 'binomial',
                  cores = 4,
                  seed = 789789,
                  data = gero.train)
print('geum jja3')

# Generate posterior predictions
g.j3.pred = posterior_predict(g.j3, newdata = gero.valid,
                              re.form = ~ (1 | plot),
                              seed = 80109,
                              draws = 4000)

# Generate summary statistics for posterior draws
g.j3.pred.summ = g.j3.pred %>%
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
  mutate(sp = 'gero', model = 'jja3')

##### Export

write.csv(rbind(kej2.pred.summ %>% 
                  mutate(loglik = log_lik(k.e.j2) %>% apply(1, sum) %>% mean()), 
                k.j2.pred.summ %>% 
                  mutate(loglik = log_lik(k.j2) %>% apply(1, sum) %>% mean()), 
                g.j3.pred.summ %>% 
                  mutate(loglik = log_lik(g.j3) %>% apply(1, sum) %>% mean())),
          row.names = FALSE,
          file = 'output/komy_gero_temp_summary.csv')

save(k.e.j2, k.j2, g.j3,
     file = 'komy_gero_temp_mods.RData')
