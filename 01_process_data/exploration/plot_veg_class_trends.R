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
 
 ##dece is maybe increasing in mm -p.value = 0.09
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
 
 #plots
 #are there plots that change
 #look within moist meadow plots (plots that were designated as moist meadow in 2010)
mmplots<- veg$plot[which(veg$year==2010 & veg$veg_class == 'MM' & veg$species == 'DECE')]

veg%>%
  filter(plot %in% mmplots)%>%
  ggplot(aes(x=year, y= n.obs, color = species))+
  geom_point(aes(x=year, y= n.obs, color = species))+
  geom_smooth(method = 'lm')+
  ggtitle('species observations in moist meadow plots')
  
dece.mm.all<-veg%>%
  filter(plot %in% mmplots)%>%
  filter(species %in% 'DECE')
summary(lm(data = dece.mm.all, n.obs~year))#very sig. r2 only 0.03 though

#plot plots by range of dece values
veg%>%
  filter(species %in% 'DECE')%>%
  group_by(plot)%>%
  summarise(
    range = max(n.obs)-min(n.obs)
  )%>%
  ggplot(aes(plot, range))+
  geom_point(aes(plot, range))
  #some plots vary a lot in number of dece hits
plot.range<-veg%>%
  filter(species %in% 'DECE')%>%
  group_by(plot)%>%
  summarise(
    range = max(n.obs)-min(n.obs)
  )
high.range.plots<-veg$plot[which(plot.range>20)]
veg%>%
  filter(plot %in% high.range.plots)%>%
  ggplot(aes(x=year, y= n.obs, color = species))+
  geom_point(aes(x=year, y= n.obs, color = species))+
  geom_smooth(method = 'lm')+
  ggtitle('species observations in plots where dece changes a lot')
 