library(tidyr)
library(dplyr)
library(ggplot2)
library(spdep)
library(lme4)

veg<-read.csv('01_process_data/output/veg_all_predictors.csv')
head(veg)


###spatial dependence
veg.2010<-veg%>%filter(year %in% 2010)%>%filter(species %in% 'DECE')
coords<- cbind(veg.2010$UTM_E, veg.2010$UTM_N)
matrix <- as.matrix(veg.2010$UTM_E, veg.2010$UTM_N, veg.2010$n.obs)
colnames(coords) <- c("X", "Y")
rownames(coords)<-veg.2010$plot
distmat <- as.matrix(dist(coords))
max(distmat)
maxdist = 2/3 *max(distmat)

neigh <- dnearneigh(x = coords, d1 = 0, d2 = 100, longlat = F)

plot(neigh, coordinates(coords))

#variogram

library(geoR)
library(gstat)
geo.veg <- as.geodata(matrix)
emp.geoR <- variog(geo.veg, max.dist = maxdist)
plot(emp.geoR)

##one way to encorporate spatial autocorrelation would be to create a weighted matrix based on distance. like a dissimilarity matrix. Then for every point use the row summing each the point weight by the abundance for the previous year. 

#creat Inverse distance weighted matrix
idw<-1/distmat
idw[!is.finite(idw)]<-NA
#I believe this give the estimate fo each plot based on the weights for all other plots * the amount fo dece in those plots. (in 2010)
A<-colSums(veg.2010$n.obs*idw, na.rm = T)


#which year does not have 88 plots
veg%>%
  filter(species %in% 'DECE')%>%
  filter(!year %in% 1995)%>%
  group_by(year)%>%
  summarise(nplots = n())



dece.A.all.years<-veg%>%
  filter(species %in% 'DECE')%>%
  filter(!year %in% 1995)%>%#1995 does not appear to have all 88 plots 
  group_by(year)%>%
  mutate( A =colSums(n.obs*idw, na.rm = T) )
#very simple model
summary(lm(data = dece.A.all.years, n.obs~A))

##ok to do this we first need to subset to consecutive years - 2010-2018
dece.sp.a.yr<-dece.A.all.years%>%
  filter(year > 2008)%>%
  group_by(plot, year, A)%>%
  summarise(
    Ayr = year + 1
  )
dece.sp.a.yr$year<-NULL
colnames(dece.sp.a.yr)[2:3]<-c('A1', 'year')
  
dece.consec.yrs<-dece.A.all.years%>%
  filter(year > 2008)
dece.A.consec.yrs<-merge(dece.sp.a.yr, dece.consec.yrs, by = c('year', 'plot'))

#try mixed effects model
dece.sp.a.null = glm(cbind(n.obs, 100 - n.obs) ~ 1, 
                family = 'binomial',
                data =dece.A.consec.yrs)

dece.sp.a.1 = glmer(cbind(n.obs, 100 - n.obs) ~ A1 +(1|year), 
                     family = 'binomial',
                     data =dece.A.consec.yrs)
summary(dece.sp.a.1)

dece.sp.a.2 = glmer(cbind(n.obs, 100 - n.obs) ~ A1 +(1|year)+(1|plot), 
                    family = 'binomial',
                    data =dece.A.consec.yrs)
summary(dece.sp.a.2)
dece.sp.a.3 = glmer(cbind(n.obs, 100 - n.obs) ~ (1|year)+(1|plot), 
                    family = 'binomial',
                    data =dece.A.consec.yrs)
summary(dece.sp.a.3)


##geum
gero.A.all.years<-veg%>%
  filter(species %in% 'GEROT')%>%
  filter(!year %in% 1995)%>%#1995 does not appear to have all 88 plots 
  group_by(year)%>%
  mutate( A =colSums(n.obs*idw, na.rm = T) )
#very simple model
summary(lm(data = gero.A.all.years, n.obs~A))

##next we will use the previous years spatial autocorrelation to predict the next years abundance
##ok to do this we first need to subset to consecutive years - 2010-2018
gero.sp.a.yr<-gero.A.all.years%>%
  filter(year > 2008)%>%
  group_by(plot, year, A)%>%
  summarise(
    Ayr = year + 1
  )
gero.sp.a.yr$year<-NULL
colnames(gero.sp.a.yr)[2:3]<-c('A1', 'year')

gero.consec.yrs<-gero.A.all.years%>%
  filter(year > 2008)
gero.A.consec.yrs<-merge(gero.sp.a.yr, gero.consec.yrs, by = c('year', 'plot'))

#try mixed effects model
gero.sp.a.null = glm(cbind(n.obs, 100 - n.obs) ~ 1, 
                     family = 'binomial',
                     data =gero.A.consec.yrs)

gero.sp.a.1 = glmer(cbind(n.obs, 100 - n.obs) ~ A1 +(1|year), 
                    family = 'binomial',
                    data =gero.A.consec.yrs)
summary(gero.sp.a.1)

gero.sp.a.2 = glmer(cbind(n.obs, 100 - n.obs) ~ A1 + (1|plot)+(1|year), 
                    family = 'binomial',
                    data =gero.A.consec.yrs)
summary(gero.sp.a.2)

gero.sp.a.3 = glmer(cbind(n.obs, 100 - n.obs) ~(1|plot)+(1|year), 
                    family = 'binomial',
                    data =gero.A.consec.yrs)
summary(gero.sp.a.3)

##KOMY

komy.A.all.years<-veg%>%
  filter(species %in% 'KOMY')%>%
  filter(!year %in% 1995)%>%#1995 does not appear to have all 88 plots 
  group_by(year)%>%
  mutate( A =colSums(n.obs*idw, na.rm = T) )
#very simple model
summary(lm(data = komy.A.all.years, n.obs~A))

##next we will use the previous years spatial autocorrelation to predict the next years abundance
##ok to do this we first need to subset to consecutive years - 2010-2018
komy.sp.a.yr<-komy.A.all.years%>%
  filter(year > 2008)%>%
  group_by(plot, year, A)%>%
  summarise(
    Ayr = year + 1
  )
komy.sp.a.yr$year<-NULL
colnames(komy.sp.a.yr)[2:3]<-c('A1', 'year')

komy.consec.yrs<-komy.A.all.years%>%
  filter(year > 2008)
komy.A.consec.yrs<-merge(komy.sp.a.yr, komy.consec.yrs, by = c('year', 'plot'))

#try mixed effects model
komy.sp.a.null = glm(cbind(n.obs, 100 - n.obs) ~ 1, 
                     family = 'binomial',
                     data =komy.A.consec.yrs)


komy.sp.a.1 = glmer(cbind(n.obs, 100 - n.obs) ~ A1 +(1|year), 
                    family = 'binomial',
                    data =komy.A.consec.yrs)
summary(komy.sp.a.1)#not significant

komy.sp.a.2 = glmer(cbind(n.obs, 100 - n.obs) ~ A1+(1|plot)+(1|year), 
                    family = 'binomial',
                    data =komy.A.consec.yrs)
summary(komy.sp.a.2)#relationship minorly significant with plot level random effect

komy.sp.a.3 = glmer(cbind(n.obs, 100 - n.obs) ~ (1|plot)+(1|year), 
                    family = 'binomial',
                    data =komy.A.consec.yrs)
summary(komy.sp.a.3)

##make csv with autocorrelation variable and year
#DECE
sample.years<-unique(dece.A.all.years$year)
dece.sp.a.yr<-dece.A.all.years%>%
  group_by(plot, year, A)%>%
  mutate(
    Ayr = sample.years[which(sample.years == year)+1]
  )
dece.A<-data.frame(dece.sp.a.yr$A, dece.sp.a.yr$Ayr, dece.sp.a.yr$plot, dece.sp.a.yr$species )

colnames(dece.A)<-c('A', 'year', 'plot', 'species')

#GEUM
sample.years<-unique(gero.A.all.years$year)
gero.sp.a.yr<-gero.A.all.years%>%
  group_by(plot, year, A)%>%
  mutate(
    Ayr = sample.years[which(sample.years == year)+1]
  )
gero.A<-data.frame(gero.sp.a.yr$A, gero.sp.a.yr$Ayr, gero.sp.a.yr$plot , gero.sp.a.yr$species)

colnames(gero.A)<-c('A', 'year', 'plot', 'species')


#KOMY
sample.years<-unique(komy.A.all.years$year)
komy.sp.a.yr<-komy.A.all.years%>%
  group_by(plot, year, A)%>%
 mutate(
    Ayr = sample.years[which(sample.years == year)+1]
  )
komy.A<-data.frame(komy.sp.a.yr$A, komy.sp.a.yr$Ayr, komy.sp.a.yr$plot, komy.sp.a.yr$species)

colnames(komy.A)<-c('A', 'year', 'plot', 'species')

#Combine all species into one
all.species<-rbind(dece.A, gero.A, komy.A)

write.csv(all.species, row.names = F, '01_process_data/output/autocor_value.csv')
