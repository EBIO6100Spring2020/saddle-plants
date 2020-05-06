#' ---
#' title: "Exploration"
#' author: All
#' output: github_document
#' ---
#+ r load.shit, message = FALSE, warnings = FALSE, echo = FALSE
library(ggplot2)
library(dplyr)
library(tidyr)

#' We decided to use the the NIWOT saddle grid data to answer our questions.
#' This grid was sampled intermittently since 1989 and annually or biannually 
#' since 2006. 

#' How often is each year represented?
#' We first wanted to know how sampling differed across the years in the data set.
#' Based on this information we excluded 1996 from our analysis
#' 
table(veg$year)
#'we also noticed sampling design seemed to vary. In some years middle hits were
#'taken and in others they were not. In some years top hits were recorded for every
#'plot but not in others.
#'
#'Ultimately we decide to isolate only the top hits and covert bottom only hits to top 
#'hits in the early years

veg = read.csv('00_raw_data/vegetation_sampling/saddptqd.hh.data.csv', header = T)
head(veg)
class(veg$year)
veg.proc = veg %>% filter(!year %in% 1996)
veg.proc = veg.proc %>% filter(!grepl('^2', USDA_code))
veg %>%
  filter(USDA_code %in% c('KOMY', 'GEROT', 'DECE')) %>%
  filter(!year %in% 1996) %>%
  filter()
  group_by(year, hit_type, USDA_code) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_line(aes(x = year, y = n, group = hit_type, colour = hit_type)) +
  geom_point(aes(x = year, y = n, colour = hit_type)) +
  theme(legend.position = 'bottom') +
  facet_wrap(~ USDA_code)

#'Although intially we were primarily interested in the Deschapsia-Geum interaction 
#'we noticed that kobresia myoseroides was the second most common hit at decided to 
#'include it in our analysis. 
#'
#'We next wanted to visualise how each species was changing throughout time. Was 
#'Deschampsia becoming much more abundant across the whole site?
#+ r species trends, echo = FALSE
veg.clean<-read.csv('01_process_data/output/veg_all_predictors.csv')
  veg.clean%>%
    ggplot( aes(x = year, y = n.obs, color = species))+
    geom_point(aes(x =year, y = n.obs, color = species))+
    geom_smooth(method = 'lm')
#'Within certain habitat type it seems like relative abundance of certain species changes
  #+ r veg class trends, echo = FALSE
  veg.clean<-read.csv('01_process_data/output/veg_all_predictors.csv')
  veg.clean%>%
    group_by(plot, species)%>%
    mutate(vegclass = ifelse(!is.na(veg_class[which(year == 1995)]), veg_class[which(year == 1995)] , 'B'))
  
  veg.clean%>%
    drop_na(veg_class)%>%
    ggplot( aes(x = year, y = n.obs, color = veg_class))+
    geom_point(aes(x =year, y = n.obs, color = veg_class))+
    geom_smooth(method = 'lm')+
    facet_wrap(vars(species))
  
  #'
#' Elevation 1 with veg classes
#+ r elev1, echo = FALSE
read.csv('01_process_data/output/veg_all_predictors.csv') %>%
  distinct(plot, year, .keep_all = TRUE) %>%
  filter(!is.na(veg_class)) %>%
  ggplot() +
  geom_point(aes(x = UTM_E, y = UTM_N, shape = veg_class,
                 colour = elev1),
             position = position_jitter(height = 5, width = 5),
             size = 6) +
  theme(panel.background = element_rect(fill = 'black'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'black'),
        axis.text = element_text(colour = 'white'),
        axis.title = element_text(colour = 'white'),
        legend.background = element_rect(fill = 'black'),
        legend.text = element_text(colour = 'white'),
        legend.title = element_text(colour = 'white'),
        legend.key = element_rect(fill = 'gray55')) +
  scale_color_gradient(low = 'gray22', high = 'white') +
  scale_shape_manual(values = 1:7)

#'Snow depth: early models showed that that there was a lot of variation in space,
#'with somewhat less in time.The only explanatory variable we had that varied in 
#'both space and time was snow depth. Snow depth was measured at stakes associated 
#'with each plot starting in March on a biweekly (mostly) schedule, producing a 
#'mountain of data.
#'
#'We felt the most biologically relavant data to be max depth and snow melt date.
#'Due to gaps in the data it was hard to estimate snow melt date reliably. Scott 
#'tried valiantly to fit GAMs to the snow data to allow us to estimate snow melts. 
#'But after several weeks and many discussions the GAMs grew too frustrating and we 
#'opted to use mean snow depth in June - simpler metric that still included some of
#'that information.
#'
#'We were able use temperature data at the site level from the saddle data logger.
#'This provided us with a wealth of data at a fine temporal scale.
#+ r daily temp, echo = FALSE
daily = daily %>%
mutate(jd = paste('1970', month, day, sep = '-') %>% as.Date() %>% julian(),
       wyear = year + as.numeric(month > 9),
       wd = jd - 273 + ifelse(wyear == year, 365, 0))

head(daily)

daily %>%
  ggplot() +
  geom_line(aes(x = wd, y = avg_temp)) +
  facet_wrap(~ wyear)
#'
#'However, we decided to use the summer mean daily maximum from June - August as our #'predictor
#'
#'We also included a spatial autocorrelation term to incorporate the effect of the
#'abuncance of each speces in the previous year weighted by distance to the plot. 
#+ r spatial autocorrelation, echo = FALSE
veg.2010<-veg%>%filter(year %in% 2010)%>%filter(species %in% 'DECE')
coords<- cbind(veg.2010$UTM_E, veg.2010$UTM_N)
matrix <- as.matrix(veg.2010$UTM_E, veg.2010$UTM_N, veg.2010$n.obs)
colnames(coords) <- c("X", "Y")
rownames(coords)<-veg.2010$plot
distmat <- as.matrix(dist(coords))
idw<-1/distmat
idw[!is.finite(idw)]<-NA
idw

