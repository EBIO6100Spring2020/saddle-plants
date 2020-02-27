#pull in and plot niwot saddle data for plant species composition and NPP
#Calculate relative abundance and spp diversity across plots 
#Courtney G. Collins
#1/29/2020

library(dplyr)
library(tidyr)
library(RColorBrewer)
library(viridisLite)
library(ggplot2)
library(grid)
library(gridGraphics)
library(gridExtra)

####### IMPORT DATA
# Package ID: knb-lter-nwt.93.3  
# Data set title: Plant species composition data for Saddle grid, 1989 - ongoing
# Data set creator:  Walker, Marilyn, Humphries, Hope  
# https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-nwt.93.3 --go to Data Entities for inURL

inUrl1  <- "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.93.3&entityid=03590a13459ffb31dc411ef6634ffaf2" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

plantcomp <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "local_site",     
                 "year",     
                 "plot",     
                 "point",     
                 "x",     
                 "y",
                 "hit_type",
                 "USDA_code",     
                 "USDA_name"), check.names=TRUE)


# Package ID: knb-lter-nwt.16.4  
# Data set title: Aboveground net primary productivity data for Saddle grid, 1992 - ongoing
# Data set creators:  Walker, Marilyn, Humphries, Hope, Smith, Jane G.
# https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-nwt.16.4 --go to Data Entities for inURL

inUrl1  <- "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.16.4&entityid=a655025cba30153a20badad6e96c6736" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

npp<-read.csv(infile1,header=F 
                     ,skip=1
                     ,sep=","  
                     ,quot='"' 
                     , col.names=c(
                       "LTER_site" ,
                       "local site",
                       "year",
                       "collection_date",
                       "plot",
                       "subsample",
                       "veg_class",
                       "NPP",
                       "notes"), check.names=TRUE)

#take mean of two NPP subsamples within a plot/year
npp<- group_by(npp, plot, year)%>%
  mutate(NPPmean=mean(NPP))%>%
    unite(plotyear, plot, year, remove=F)

#remove unidentified stuff
plantcomp<-filter(plantcomp, !grepl("2", USDA_code))

#sum hits of each spp within a plot/year = spp hits, sum total (veg) hits in that plot year=veg hits
#rel abund =spp hits/veg hits
spp_abund<-plantcomp %>%group_by(year, plot, USDA_code, USDA_name) %>% 
  count() %>% 
  summarise(spp_hits=sum(n))%>%
  group_by(., year, plot, .drop = FALSE) %>%
  mutate(veg_hits=sum(spp_hits))%>%
  mutate(rel_abund=spp_hits/veg_hits)%>%
  unite(., plotyear, plot, year)


#calculate shannon div
spp_diversity<-spp_abund %>%group_by(plotyear) %>% 
  mutate(richness=n_distinct(USDA_code))%>% 
  mutate(shannon= sum(rel_abund*log(rel_abund))*-1) 

spp_diversity<-merge(spp_diversity, npp, by="plotyear")  
spp_diversity<-group_by(spp_diversity, plotyear)%>% 
  dplyr::select(. , NPP, richness, shannon, plot, year, plotyear)%>% 
  distinct(.)

#put all together
spp_abund_div<-left_join(spp_abund, spp_diversity)%>%
  separate(., plotyear, c("plot", "year"))

#plot spp abundance over time for spp with 20% or more plot cover
ggplot(subset(spp_abund_div, year<2006 & rel_abund>0.2 & !year==1996)) + 
  geom_boxplot(aes(x=as.factor(year), y=log(rel_abund), fill=USDA_code)) +
  theme_classic()

ggplot(subset(spp_abund_div, year>2006 & year<2013 & rel_abund>0.2)) + 
  geom_boxplot(aes(x=as.factor(year), y=log(rel_abund), fill=USDA_code)) +
  theme_classic()

ggplot(subset(spp_abund_div, year>2013 & rel_abund>0.2)) + 
  geom_boxplot(aes(x=as.factor(year), y=log(rel_abund), fill=USDA_code)) +
  theme_classic()


