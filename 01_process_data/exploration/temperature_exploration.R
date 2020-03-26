weath = read.csv("~/Downloads/knb-lter-nwt.413.10/sdltdayv.ml.data.csv")

nrow(weath)

head(weath)

weath = read.csv("~/Downloads/knb-lter-nwt.413.10/sdltdayv.ml.data.csv") %>%
  mutate(year = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[1] %>% as.numeric()),
         mnth = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[2] %>% as.numeric()),
         dymn = date %>% as.character() %>% strsplit(split = '\\-') %>% 
           sapply(function(x) unlist(x)[3] %>% as.numeric()),
         seas = year + as.numeric(mnth > 9),
         date.origin = as.Date.character(paste0(year, '-10-01'), format = '%Y-%m-%d'),
         i = 1:nrow(.)) %>%
  group_by(i) %>%
  mutate(jd = date %>% 
           as.Date('%Y-%m-%d') %>% 
           julian.Date(origin = date.origin) %>%
           (function(x) x[1]),
         jd = jd + ifelse(jd < 0, 365 + as.numeric(year %% 4), 0)) %>%
  ungroup() %>%
  select(-c(i, date.origin))

head(weath)
# No plot substructure.

# These are daily records!
weath %>% group_by(year) %>% summarise(n.obs = n()) %>% print(n = 30)

weath %>%
  ggplot() +
  geom_line(aes(x = jd, y = airtemp_avg, group = seas), size = 0.1)

weath.c = weath %>%
  group_by(jd) %>%
  mutate(c.mean = airtemp_avg - mean(airtemp_avg, na.rm = TRUE))

weath.c %>%
  filter(year > 1993) %>%
  ggplot() +
  geom_segment(x = 0, xend = 365, y = 0, yend = 0, colour = 'blue') +
  geom_line(aes(x = jd, y = c.mean, group = seas), size = 0.4) +
  facet_wrap(~ seas)

lm(c.mean ~ year, weath.c) %>% summary() # trend of increasing over time (weak)

weath %>%
  filter(seas > 1993) %>%
  ggplot() +
  geom_line(aes(x = jd, y = airtemp_avg, group = seas), size = 0.1) +
  geom_segment(x = 0, xend = 365, y = 0, yend = 0, colour = 'blue') +
  facet_wrap(~ seas)

# THIS data could be splined very easily.

weath.thaw = weath %>%
  group_by(seas) %>%
  mutate(thaw = airtemp_avg > 0 & jd > 100,
         thaw = ifelse(is.na(thaw), FALSE, thaw),
         cuml.thaw = cumsum(thaw))

weath.thaw %>%
  filter(seas > 1993) %>%
  ggplot() +
  geom_line(aes(x = jd, y = cuml.thaw, group = seas))

weath.thaw %>%
  filter(cuml.thaw %in% 5) %>%
  distinct(seas, .keep_all = TRUE) %>%
  ggplot() +
  geom_histogram(aes(x = jd, fill = seas), binwidth = 1)

weath.thaw %>%
  filter(cuml.thaw %in% 5) %>%
  distinct(seas, .keep_all = TRUE) %>%
  select(seas, )
  print(n = 30)
