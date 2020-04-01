library(tidyr)
library(dplyr)
library(ggplot2)

veg<-read.csv('01_process_data/output/veg_all_predictors.csv')
head(veg)

##are there trends within veg types

veg%>%
  drop_na(veg_class)%>%
  ggplot( aes(x = year, y = n.obs, color = veg_class))+
  geom_point(aes(x =year, y = n.obs, color = veg_class))+
  geom_smooth(method = 'lm')+
    facet_wrap(vars(species))
##Komy looks like it is increasing in Dry meadow
komy.dm<-veg%>%
  filter(species %in% 'KOMY')%>%
  filter(veg_class %in% 'DM')
 summary(lm(data = komy.dm, n.obs~year))
 
 ##dece is increasing in mm -p.value = 0.09
dece.mm<-veg%>%
   filter(species %in% 'DECE')%>%
   filter(veg_class %in% 'MM')
 summary(lm(data = dece.mm, n.obs~year))

 
 #are the porpotion of veg classes changing over time
 veg%>%
   drop_na(veg_class)%>%
   group_by(veg_class, year)%>%
   summarise(class.plots = n())%>%
   ggplot(aes(x= year, y = class.plots, color = veg_class))+
   geom_point(aes(x= year, y = class.plots))+
   geom_smooth(method = 'lm')
 