# Second try at fitting models.
# sn 18 mar 2020

library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(brms)
library(rstanarm)

##### Read in data and check column completeness
all.sp = read.csv('01_process_data/output/veg_all_predictors.csv')

head(all.sp)

# Number of rows
nrow(all.sp)
# 3957

# How many NAs are there in each column?
apply(all.sp, 2, function(x) sum(is.na(x)))
# Lots of veg class missing,
# Lots of veg class missing.
# some NPP missing
# some snow depths missing.
# little bit of spatial info missing

# How many rows are there with all data present?
apply(all.sp, 1, function(x) all(!is.na(x))) %>% table()
# 990 rows only... Would be 330 per species.

# Look at spatial info 2 only
all.sp %>%
  select(-c(slope1, elev1, asp1)) %>%
  apply(1, function(x) all(!is.na(x))) %>%
  table()
# 1083 only. Still not very much.

# Maybe if we cut out veg type? It's possibly circular.
all.sp %>%
  select(-c(slope1, elev1, asp1, veg_class)) %>%
  apply(1, function(x) all(!is.na(x))) %>%
  table()
# This... can't be true.

# NPP
all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  apply(1, function(x) all(!is.na(x))) %>%
  table()
# If we take out veg_class and mNPP, then we have
# >3K records

# Is this data frame still balanced across plots?
all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  group_by(plot) %>%
  summarise(n = n())
# Yes.

filt.all.sp = all.sp %>%
  select(-c(slope1, elev1, asp1, mNPP, veg_class)) %>%
  filter(apply(., 1, function(x) all(!is.na(x)))) %>%
  mutate(asp.e = sin(pi * asp2 / 180),
         asp.n = cos(pi * asp2 / 180),
         c.slope = slope2 - mean(slope2))

##### Create training and testing data.

dece.train = filt.all.sp %>% filter(species %in% 'DECE' & year %in% 1995:2015) %>% mutate(maxs = 100)
dece.valid = filt.all.sp %>% filter(species %in% 'DECE' & year %in% 2016:2018) %>% mutate(maxs = 100)

komy.train = filt.all.sp %>% filter(species %in% 'KOMY' & year %in% 1995:2015) %>% mutate(maxs = 100)
komy.valid = filt.all.sp %>% filter(species %in% 'KOMY' & year %in% 2016:2018) %>% mutate(maxs = 100)

gero.train = filt.all.sp %>% filter(species %in% 'GEROT' & year %in% 1995:2015) %>% mutate(maxs = 100)
gero.valid = filt.all.sp %>% filter(species %in% 'GEROT' & year %in% 2016:2018) %>% mutate(maxs = 100)

######## Fit models in rstanarm

dece.null = stan_glmer(cbind(n.obs, 100 - n.obs) ~ (1 | plot) + (1 | year) + (1 | obsno),
                       family = 'binomial',
                       cores = 4,
                       seed = 2009210,
                       data = dece.train)

summary(dece.null)
# tiny tiny effective sample size for the intercept. large uncertainty

dece.null.pred = posterior_predict(dece.null, newdata = dece.valid,
                                   re.form = ~ (1 | plot),
                                   draws = 1000)
# I HOPE (1 | plot) is taking only the plot-level random effect...
# but does this include residual error? fuck i'll have to play around with this
dece.null.eval = dece.null.pred %>%
  t() %>%
  as.data.frame() %>%
  mutate(i = 1:nrow(.),
         y = dece.valid$n.obs) %>%
  gather(key = draw, val = pred, -c(i,y))
  
ggplot(dece.null.eval) +
  geom_point(aes(x = i, y = pred),
             position = position_jitter(0.25),
             alpha = 0.125,
             colour = 'orange') +
  geom_point(aes(x = i, y = y),
             colour = 'blue',
             size = 2)
# yep... not good

# jesus christ all of this is slow

# ######## Fit models in brms (old data - won't be used because of... issues)
# 
# ### Aux code for a brms beta binomial
# 
# beta_binomial2 = custom_family(
#   "beta_binomial2", dpars = c("mu", "phi"),
#   links = c("logit", "log"), lb = c(NA, 0),
#   type = "int", vars = "prevs[n]"
# )
# 
# stan_funs = "
#   real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
# return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
# }
# int beta_binomial2_rng(real mu, real phi, int T) {
# return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
# }
# "
# 
# stanvars = stanvar(scode = stan_funs, block = "functions") + stanvar(as.integer(dece.train$maxs), name = "prevs")
# 
# log_lik_beta_binomial2 = function(i, draws) {
#   mu <- draws$dpars$mu[, i]
#   phi <- draws$dpars$phi
#   N <- draws$data$prevs[i]
#   y <- draws$data$Y[i]
#   beta_binomial2_lpmf(y, mu, phi, N)
# }
# 
# predict_beta_binomial2 = function(i, draws, ...) {
#   mu <- draws$dpars$mu[, i]
#   phi <- draws$dpars$phi
#   N <- draws$data$trials[i]
#   beta_binomial2_rng(mu, phi, N)
# }
# 
# ### Deschampsia models
# 
# dece.null = brm(n.obs | trials(maxs) ~ (1 | year) + (1 | plot),
#                 family = beta_binomial2, stanvars = stanvars,
#                 data = dece.train,
#                 seed = 55949, cores = 4)
# 
# # Warning messages:
# #   1: The largest R-hat is 1.06, indicating chains have not mixed.
# # Running the chains for more iterations may help. See
# # http://mc-stan.org/misc/warnings.html#r-hat 
# # 2: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# # Running the chains for more iterations may help. See
# # http://mc-stan.org/misc/warnings.html#bulk-ess 
# # 3: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# # Running the chains for more iterations may help. See
# # http://mc-stan.org/misc/warnings.html#tail-ess 
# 
# # dece.null = stan_glm(cbind(n.obs, 100 - n.obs) ~ 1,
# #                         family = 'binomial',
# #                         data = dece.train,
# #                         seed = 1,
# #                         cores = 4)
# 
# expose_functions(dece.null, vectorize = TRUE)
# pp_check(dece.null)
# # Yep. This is nowhere close.
# 
# dece.null.ppred = posterior_predict(dece.null, new.data = dece.valid)
# 
# # Check these for overdispersion.
# 
# # ## Null model
# # dece.null = glm(cbind(n.obs, 100 - n.obs) ~ 1,
# #                 family = 'binomial',
# #                 data = dece.train)
# # 
# # summary(dece.null)
# # # This model is incredibly overdispersed.
# # 
# # ## Null model, feat. overdispersion
# # dece.null = glm(cbind(n.obs, 100 - n.obs) ~ 1,
# #                 family = 'quasibinomial',
# #                 data = dece.train)
# # 
# # summary(dece.null)
# # fitted.values(dece.null)[1]
# # 
# # # Validate.
# # dece.null.pred = predict(object = dece.null, new.data = dece.valid, interval = 'prediction',
# #                          dispersion = 28.57663)
# # 
# # dece.null.pred
# # Not working. # Fuck man

### Attempting a multinomial model

