#' ---
#' title: "Final Single-Species Models"
#' author: Tom, Scott, Sean
#' output: github_document
#' ---
#' 
#+ r package-load, message = FALSE, warning = FALSE

library(ggplot2)
library(dplyr)
library(tidyr)
library(rstanarm)
library(ggridges)

# Load in overall dataset.
all.sp = read.csv('~/repos/poorcasting-real/01_process_data/output/veg_97_all_predictors.csv')

#' 
#' Here I'll be presenting the final (most predictive) models for our forecasting project.
#' 
#' Each of these models is fit with `n.obs` as the response; `n.obs` is a
#' binomial random variable out of 100 trials (for 100 points on each sample
#' grid). This means that each of these models is fit with a binomial error
#' family (logit link).
#' 
#' These models feature three random effects.
#' 
#' - `(1 | plot)` - plot-level random intercept, i.e., spatial variance
#' 
#' - `(1 | year)` - year-level random intercept, i.e., temporal variance
#' 
#' - `(1 | obs)` - an observation level random effect, meant to account for overdispersion
#' 
#' The `(1 | obs)` term captures residual variance and can be considered
#' spatio-temporal variance.
#' 
#' Each of these models was fit with the package `rstanarm`.
#' 
#' ### Deschampsia
#' 
#' The final Deschampsia model features the following terms
#' 
#' `n.obs ~ A + pH + N * veg_class + N * asp.e + (1 | plot) + (1 | year) + (1 | obs)`
#' 
#' Where
#' - `A` is a local weighted-mean of Deschampsia in the prior sample
#' - `pH` is a mean weekly pH measurement taken over the course of the prior water year (October - September)
#' - `N` is a mean weekly Nitrogen measurement taken over the previous water year
#' - `veg_class` is a categorical variable for one of several vegetation classifications in 1995
#' - `asp.e` is the Easting aspect (positive is east-facing, negative is west-facing)
#' 
#+ r load-dece-mod, warning = FALSE, message = FALSE

# DECE dataset
dece.data = all.sp %>% filter(species %in% 'DECE') %>% mutate(obsno = 1:nrow(.))

# Load in final model
load('~/repos/poorcasting-real/02_fit_species_models/sixfold_analyses/dece/dece_final.RData')
# Load in null model
load('~/repos/poorcasting-real/02_fit_species_models/sixfold_analyses/dece/dece_fnull.RData')

### Acquire fixed effect estimates for final Deschampsia model
dece.chains = as.data.frame(dece.final) %>%
  select(names(.)[!grepl('^b', names(.))]) %>%
  rename(Intercept = `(Intercept)`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500)) 

### Plot histograms of fixed effects

dece.chains %>%
  ggplot() +
  geom_histogram(aes(x = A, fill = A > 0),
                 binwidth = 0.01) + 
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none')

dece.chains %>%
  ggplot() +
  geom_histogram(aes(x = pH, fill = pH > 0),
                 binwidth = 0.05) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none')

dece.chains %>%
  select(c(contains('veg_class'), chain, draw)) %>%
  # select(c(contains('veg_class'), Intercept, Total_N, chain, draw)) %>%
  select(-contains('Sigma')) %>%
  # rename(DM = Intercept,
  #        `DM:Total_N` = Total_N) %>%
  gather(key = coef, value = estim, -c(chain, draw)) %>%
  mutate(class = gsub('veg_class|\\:Total_N', '', coef),
         interaction = grepl('\\:', coef)) %>%
  ggplot() +
  geom_histogram(aes(x = estim, fill = estim > 0),
                 binwidth = 0.1) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = 'none') +
  facet_grid(class ~ interaction)

dece.chains %>%
  select(c(contains('veg_class'), chain, draw)) %>%
  # select(c(contains('veg_class'), Intercept, Total_N, chain, draw)) %>%
  select(-contains('Sigma')) %>%
  # rename(DM = Intercept,
  #        `DM:Total_N` = Total_N) %>%
  gather(key = coef, value = estim, -c(chain, draw)) %>%
  mutate(class = gsub('veg_class|\\:Total_N', '', coef),
         interaction = ifelse(grepl('\\:', coef), 'Nitrogen Effect', 'Intercept')) %>%
  filter(!class %in% 'ST') %>%
  ggplot() +
  geom_histogram(aes(x = estim, fill = estim > 0),
                 binwidth = 0.1) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none') +
  facet_grid(class ~ interaction)
  
dece.chains %>%
  select(asp.e, `Total_N:asp.e`, chain, draw) %>%
  rename(Easting = asp.e,
         Easting_x_Nitrogen = `Total_N:asp.e`) %>%
  gather(key = coef, value = estim, -c(chain, draw)) %>%
  ggplot() +
  geom_histogram(aes(x = estim, fill = estim > 0),
                 binwidth = 0.1) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ coef)

### Predictions versus observed

dece.f.summ = posterior_predict(dece.final,
                                re.form = ~ (1 | plot),
                                seed = 2009,
                                draws = 2000) %>%
  t() %>%
  as.data.frame() %>%
  mutate(obsno = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(obsno)) %>%
  group_by(obsno) %>%
  summarise(yhat_mean = mean(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159))

dece.0.summ = posterior_predict(dece.fnull,
                                re.form = ~ (1 | plot),
                                seed = 8009,
                                draws = 2000) %>%
  t() %>%
  as.data.frame() %>%
  mutate(obsno = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(obsno)) %>%
  group_by(obsno) %>%
  summarise(yhat_mean = mean(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159))

dece.merged.preds = merge(x = dece.data, 
                          y = rbind(dece.f.summ %>% mutate(model = 'final'),
                                    dece.0.summ %>% mutate(model = 'null')), 
                          by = 'obsno')

dece.merged.preds %>%
  mutate(n.obs.jit = n.obs + ifelse(model %in% 'final', 0.25, -0.25)) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  geom_point(aes(x = n.obs.jit, y = yhat_mean, 
                 colour = model),
             alpha = 0.4) +
  geom_segment(aes(x = n.obs.jit, xend = n.obs.jit,
                   y = yhat_mean, yend = n.obs.jit,
                   colour = model),
               size = 0.15) +
  scale_colour_manual(values = c('darkblue', 'red')) +
  labs(x = 'Observed value', y = 'Predicted value') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

dece.merged.preds %>%
  mutate(in.ci = n.obs < yhat_q975 &
                 n.obs > yhat_q025) %>%
  ggplot() +
  geom_segment(aes(x = n.obs, xend = n.obs,
                   y = yhat_q025, yend = yhat_q975,
                   colour = in.ci),
               size = 0.5,
               position = position_jitter(width = 0.25)) +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  scale_color_manual(values = c('pink', 'lightblue')) +
  facet_wrap(~ model) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

dece.rfxf = dece.final %>%
  as.data.frame() %>%
  select(contains('Sigma')) %>%
  rename(Sig.obsn = `Sigma[obsno:(Intercept),(Intercept)]`,
         Sig.plot = `Sigma[plot:(Intercept),(Intercept)]`,
         Sig.year = `Sigma[year:(Intercept),(Intercept)]`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500))

dece.rfx0 = dece.fnull %>%
  as.data.frame() %>%
  select(contains('Sigma')) %>%
  rename(Sig.obsn = `Sigma[obsno:(Intercept),(Intercept)]`,
         Sig.plot = `Sigma[plot:(Intercept),(Intercept)]`,
         Sig.year = `Sigma[year:(Intercept),(Intercept)]`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500))

rbind(dece.rfxf %>% mutate(model = 'final'), 
      dece.rfx0 %>% mutate(model = 'null')) %>%
  gather(key = param, value = estim, -c(chain, draw, model)) %>%
  group_by(param, model) %>%
  summarise(q975 = quantile(estim, 0.975),
            q025 = quantile(estim, 0.025)) %>%
  ggplot() +
  geom_segment(aes(x = param, xend = param,
                   y = q025, yend = q975),
               size = 2) +
  geom_point(aes(x = param, y = q975), shape = '-', size = 10) +
  geom_point(aes(x = param, y = q025), shape = '-', size = 10) +
  facet_wrap(~ model)

# dece.merged.preds %>%
#   mutate(in.ci = n.obs < yhat_q841 &
#                  n.obs > yhat_q159) %>%
#   ggplot() +
#   geom_segment(aes(x = n.obs, xend = n.obs,
#                    y = yhat_q159, yend = yhat_q841,
#                    colour = in.ci),
#                size = 0.5,
#                position = position_jitter(width = 0.25)) +
#   geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
#   scale_color_manual(values = c('pink', 'lightblue')) +
#   facet_wrap(~ model) +
#   theme(panel.background = element_blank(),
#         panel.grid = element_blank())


### A spatial residual plot

#' ### Kobresia
#' 
#' The final Kobresia model has the following terms
#' 
#' `n.obs ~ A + pH + N * veg_class + (1 | plot) + (1 | year) + (1 | obs)`
#' Where
#' - `A` is a local weighted-mean of Deschampsia in the prior sample
#' - `pH` is a mean weekly pH measurement taken over the course of the prior water year (October - September)
#' - `N` is a mean weekly Nitrogen measurement taken over the previous water year
#' - `veg_class` is a categorical variable for one of several vegetation classifications in 1995
#' 
#' Notably, the Kobresia is unaffected by the aspect of the plot.
#' 
#+ r komy mods, warning = FALSE, message = FALSE

# DECE dataset
komy.data = all.sp %>% filter(species %in% 'KOMY') %>% mutate(obsno = 1:nrow(.))

# Load in final model
load('~/repos/poorcasting-real/02_fit_species_models/sixfold_analyses/komy/komy_final.RData')
# Load in null model
load('~/repos/poorcasting-real/02_fit_species_models/sixfold_analyses/komy/komy_fnull.RData')

### Acquire fixed effect estimates for final Deschampsia model
komy.chains = as.data.frame(komy.final) %>%
  select(names(.)[!grepl('^b', names(.))]) %>%
  rename(Intercept = `(Intercept)`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500)) 

### Plot histograms of fixed effects

komy.chains %>%
  ggplot() +
  geom_histogram(aes(x = pH, fill = pH > 0),
                 binwidth = 0.05) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none')

komy.chains %>%
  select(c(contains('veg_class'), chain, draw)) %>%
  # select(c(contains('veg_class'), Intercept, Total_N, chain, draw)) %>%
  select(-contains('Sigma')) %>%
  # rename(DM = Intercept,
  #        `DM:Total_N` = Total_N) %>%
  gather(key = coef, value = estim, -c(chain, draw)) %>%
  mutate(class = gsub('veg_class|\\:Total_N', '', coef),
         interaction = grepl('\\:', coef)) %>%
  ggplot() +
  geom_histogram(aes(x = estim, fill = estim > 0),
                 binwidth = 0.1) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none') +
  facet_grid(class ~ interaction)

komy.chains %>%
  select(c(contains('veg_class'), chain, draw)) %>%
  # select(c(contains('veg_class'), Intercept, Total_N, chain, draw)) %>%
  select(-contains('Sigma')) %>%
  # rename(DM = Intercept,
  #        `DM:Total_N` = Total_N) %>%
  gather(key = coef, value = estim, -c(chain, draw)) %>%
  mutate(class = gsub('veg_class|\\:Total_N', '', coef),
         interaction = ifelse(grepl('\\:', coef), 'Nitrogen Effect', 'Intercept')) %>%
  filter(!class %in% 'ST') %>%
  ggplot() +
  geom_histogram(aes(x = estim, fill = estim > 0),
                 binwidth = 0.1) +
  scale_fill_manual(values = c('pink', 'lightblue')) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none') +
  facet_grid(class ~ interaction)

### Predictions versus observed

komy.f.summ = posterior_predict(komy.final,
                                re.form = ~ (1 | plot),
                                seed = 2009,
                                draws = 2000) %>%
  t() %>%
  as.data.frame() %>%
  mutate(obsno = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(obsno)) %>%
  group_by(obsno) %>%
  summarise(yhat_mean = mean(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159))

komy.0.summ = posterior_predict(komy.fnull,
                                re.form = ~ (1 | plot),
                                seed = 8009,
                                draws = 2000) %>%
  t() %>%
  as.data.frame() %>%
  mutate(obsno = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(obsno)) %>%
  group_by(obsno) %>%
  summarise(yhat_mean = mean(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159))

komy.merged.preds = merge(x = komy.data, 
                          y = rbind(komy.f.summ %>% mutate(model = 'final'),
                                    komy.0.summ %>% mutate(model = 'null')), 
                          by = 'obsno')

komy.merged.preds %>%
  mutate(n.obs.jit = n.obs + ifelse(model %in% 'final', 0.25, -0.25)) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  geom_point(aes(x = n.obs.jit, y = yhat_mean, 
                 colour = model),
             alpha = 0.4) +
  geom_segment(aes(x = n.obs.jit, xend = n.obs.jit,
                   y = yhat_mean, yend = n.obs.jit,
                   colour = model),
               size = 0.15) +
  scale_colour_manual(values = c('darkblue', 'red')) +
  labs(x = 'Observed value', y = 'Predicted value') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

komy.merged.preds %>%
  mutate(in.ci = n.obs < yhat_q975 &
           n.obs > yhat_q025) %>%
  ggplot() +
  geom_segment(aes(x = n.obs, xend = n.obs,
                   y = yhat_q025, yend = yhat_q975,
                   colour = in.ci),
               size = 0.5,
               position = position_jitter(width = 0.25)) +
  geom_segment(aes(x = 0, xend = 82, y = 0, yend = 82)) +
  scale_color_manual(values = c('pink', 'lightblue')) +
  facet_wrap(~ model) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

### Kobresia random effects

komy.rfxf = komy.final %>%
  as.data.frame() %>%
  select(contains('Sigma')) %>%
  rename(Sig.obsn = `Sigma[obsno:(Intercept),(Intercept)]`,
         Sig.plot = `Sigma[plot:(Intercept),(Intercept)]`,
         Sig.year = `Sigma[year:(Intercept),(Intercept)]`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500))

komy.rfx0 = komy.fnull %>%
  as.data.frame() %>%
  select(contains('Sigma')) %>%
  rename(Sig.obsn = `Sigma[obsno:(Intercept),(Intercept)]`,
         Sig.plot = `Sigma[plot:(Intercept),(Intercept)]`,
         Sig.year = `Sigma[year:(Intercept),(Intercept)]`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500))

rbind(komy.rfxf %>% mutate(model = 'final'), 
      komy.rfx0 %>% mutate(model = 'null')) %>%
  gather(key = param, value = estim, -c(chain, draw, model)) %>%
  group_by(param, model) %>%
  summarise(q975 = quantile(estim, 0.975),
            q025 = quantile(estim, 0.025)) %>%
  ggplot() +
  geom_segment(aes(x = param, xend = param,
                   y = q025, yend = q975),
               size = 2) +
  geom_point(aes(x = param, y = q975), shape = '-', size = 10) +
  geom_point(aes(x = param, y = q025), shape = '-', size = 10) +
  facet_wrap(~ model)

### A spatial residual plot

#' ### Geum
#' 
#' The final Kobresia model has the following terms
#' 
#' `n.obs ~ A + (1 | plot) + (1 | year) + (1 | obs)`
#' Where
#' - `A` is a local weighted-mean of Deschampsia in the prior sample
#' 

#+ r gero-mods, warning = FALSE, message = FALSE

# Load in Geum data
gero.data = all.sp %>% filter(species %in% 'GERO') %>% mutate(obsno = 1:nrow(.))

# Load in model
load('~/repos/poorcasting-real/02_fit_species_models/sixfold_analyses/gero/gero_final.RData')

gero.rfx = gero.final %>%
  as.data.frame() %>%
  select(contains('Sigma')) %>%
  rename(Sig.obsn = `Sigma[obsno:(Intercept),(Intercept)]`,
         Sig.plot = `Sigma[plot:(Intercept),(Intercept)]`,
         Sig.year = `Sigma[year:(Intercept),(Intercept)]`) %>%
  mutate(chain = rep(1:4, each = 2500),
         draw = rep(1:4, times = 2500))

gero.rfx %>%
  gather(key = param, value = estim, -c(chain, draw)) %>%
  group_by(param) %>%
  summarise(q975 = quantile(estim, 0.975),
            q025 = quantile(estim, 0.025)) %>%
  ggplot() +
  geom_segment(aes(x = param, xend = param,
                   y = q025, yend = q975),
               size = 2) +
  geom_point(aes(x = param, y = q975), shape = '-', size = 10) +
  geom_point(aes(x = param, y = q025), shape = '-', size = 10)


gero.f.summ = posterior_predict(gero.final,
                                re.form = ~ (1 | plot),
                                seed = 2009,
                                draws = 2000) %>%
  t() %>%
  as.data.frame() %>%
  mutate(obsno = 1:nrow(.)) %>%
  gather(key = draw, val = pred, -c(obsno)) %>%
  group_by(obsno) %>%
  summarise(yhat_mean = mean(pred),
            yhat_q975 = quantile(pred, 0.975),
            yhat_q025 = quantile(pred, 0.025),
            yhat_q841 = quantile(pred, 0.841),
            yhat_q159 = quantile(pred, 0.159))

gero.merged = merge(x = gero.data, y = gero.f.summ)

gero.merged %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = 95, y = 0, yend = 9)) +
  geom_point(aes(x = n.obs, y = yhat_mean),
             colour = 'darkblue',
             alpha = 0.4) +
  geom_segment(aes(x = n.obs, xend = n.obs,
                   y = yhat_mean, yend = n.obs),
               size = 0.15, colour = 'darkblue') +
  labs(x = 'Observed value', y = 'Predicted value') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

gero.merged %>%
  mutate(in.ci = n.obs < yhat_q975 &
                 n.obs > yhat_q025) %>%
  ggplot() +
  geom_segment(aes(x = n.obs, xend = n.obs,
                   y = yhat_q025, yend = yhat_q975,
                   colour = in.ci),
               size = 0.5,
               position = position_jitter(width = 0.25)) +
  geom_segment(aes(x = 0, xend = 95, y = 0, yend = 95)) +
  scale_color_manual(values = c('pink', 'lightblue')) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

