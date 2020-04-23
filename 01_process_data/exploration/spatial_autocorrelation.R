library(tidyr)
library(dplyr)
library(ggplot2)
library(spdep)

veg<-read.csv('01_process_data/output/veg_all_predictors.csv')
head(veg)
unique(veg$year)

###spatial dependence
read.csv('00_raw_data/spatial_physical_data/Point_attributes.csv')
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

A.all.yrs<-veg%>%
  filter(species %in% 'DECE')%>%
  filter(!year %in% 1995)%>%#1995 does not appear to have all 88 plots 
  group_by(year)%>%
  mutate( A =colSums(n.obs*idw, na.rm = T) )

#which year does not have 88 plots
veg%>%
  filter(species %in% 'DECE')%>%
  filter(!year %in% 1995)%>%
  group_by(year)%>%
  summarise(nplots = n())

#very simple model
summary(lm(data = A.all.yrs, n.obs~A))


##ok to do this we first need to subset to consecutive years - 2010-2018
prev.yr.sp.a<-A.all.yrs%>%
  filter(year > 2008)%>%
  group_by(plot, year, A)%>%
  summarise(
    Ayr = year + 1
  )
prev.yr.sp.a$year<-NULL
colnames(prev.yr.sp.a)[2:3]<-c('A1', 'year')
  
consec.yrs<-A.all.yrs%>%
  filter(year > 2008)
A.consec.yrs<-merge(prev.yr.sp.a, consec.yrs, by = c('year', 'plot'))
