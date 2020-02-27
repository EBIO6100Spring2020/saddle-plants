##code from Courtney collins modified by TM 2/26

#try to subset dominant spp and look at interactions across years...
#use predict function to look at extreme snow years 
#use previous years abundance as enviro predictor (waterbirds example)
#combine with Jared's SEM analysis? 
#need to differentiate from Bowman

setwd("C:/Users/court/Google Drive/CU Postdoc/Saddle grid")
library(dplyr)
library(tidyr)
library(HMSC)
library(Rcpp)
library(coda)
library(RcppArmadillo)
library(beanplot)
library(circlize)
library(corrplot)
library(devtools)
library(MASS)
library(vegan)
library(RColorBrewer)
library(viridisLite)
library(ggplot2)
library(grid)
library(gridGraphics)
library(gridExtra)
library(pez)
set.seed(1)

#HMSC Model separate years----  
#Y species abundances----
#read in plant composition dataset 
plantcomp<-read.csv("saddptqd.hh.data.csv")
#subset only top hits (Spasojevic et al) +bottom hits (TM)+ remove years before 2008
plantcomp<-plantcomp%>%
  filter( hit_type %in% c("top","bottom"))%>%
  filter(year > 2007)
#remove unidentified stuff
plantcomp<-plantcomp%>%
  filter(!substring(USDA_code, 1,1)== "2")
                
#Calculate relative abund of each spp x plot x year 
#sum hits of each spp within a plot/year = spp hits, sum total veg hits in that plot year=veg hits
#rel abund = spp hits/veg hits

spp_abund<-plantcomp%>%
  mutate(plant.sp = ifelse(USDA_code %in% c('KOMY', 'GEROT', 'DECE'),
                           as.character(USDA_code), "other"))%>%# subset to major players + other category
  group_by(year, plot, plant.sp) %>% 
  count() %>% 
  summarise(spp_hits=sum(n))%>%
  group_by(., year, plot, .drop = FALSE) %>%
  mutate(veg_hits=sum(spp_hits))%>%
  mutate(rel_abund=spp_hits/veg_hits)

#spp_domminance<-spp_abund%>%group_by(USDA_code)%>%
  #summarise(tot_hits=sum(spp_hits))%>%
  #subset(., tot_hits>0)%>%
  #subset(., tot_hits>99)%>%#53
  #subset(., tot_hits>999) #21 spp


#spp_subdom<-spp_abund%>%group_by(USDA_code)%>%
#  summarise(tot_hits=sum(spp_hits))%>%
#  subset(., tot_hits>0)%>%
#  subset(., tot_hits>99)%>% #42 spp

#create Y matrix for HMSC with spp names as columns and relative abund in each plot 
#subset by year 
yr = 2018 #set year of interest
unique(plantcomp$year) 
spp_abund_yr<-subset(spp_abund, year==yr)
Y<-pivot_wider(spp_abund_yr, names_from = plant.sp, values_from =rel_abund, id_cols = plot)%>%
  mutate_all(~replace(., is.na(.), 0)) %>% #put zeroes for NAs  
  as.data.frame() 
rownames(Y)<-Y$plot 
Y$plot<-NULL
plots<-rownames(Y)



#enviro vars X----
#read in snow dataset 
snow<-read.csv("saddsnow.dw.data.csv")
unique(snow$year)#1992-2019
snow_yr<-subset(snow, year==yr)

#take max snow depth that year 
snow_yr<- group_by(snow_yr, plot)%>% 
  summarise(depth=max(na.omit(mean_depth)))

plot(mean_depth~year, subset(snow, year>2007))#which years had highest npp?

#plot(mean_depth~year, subset(snow, year>2007))#which years had most snow?
ggplot(subset(snow, year>2007), aes(x=as.factor(year), y=log(mean_depth+1))) + 
  geom_boxplot()


#read in npp and community type 
npp<-read.csv("saddgrid_npp.hh.data.csv")
#unique(npp$year)
npp_yr<-subset(npp, year==yr)
#average two npp subsamples 
npp_yr<-group_by(npp_yr,plot, year)%>%
  summarise(NPP=mean(NPP))

plot(log(NPP)~year, subset(npp, year>2007))#which years had highest npp?

X<-merge(npp_yr, snow_yr, by="plot")

#read in slope, aspect, solar radiation for plots
topo<-read.csv("saddle_topo_solar.csv")
topo$easting<-sin(3.14*(topo$Aspect/180))
topo$northing<-cos(3.14*(topo$Aspect/180))


#solar/topo info on plots 
topox<-topo[ , c(3:4, 7:10)]
topox$Aspect<-NULL
X<-merge(topox, X, by="plot")  

#read in soil moisture/nutrient data
soil<-read.csv("saddsoil.dw.data.csv")
soil<-soil[, c(1,3,6, 11)]

soil[55,4]<-4.667 #fill in NA~ Oxidizable org C *1.3 (see metadata) 
X<-merge(soil, X, by="plot")  

#subset X and Pi to only plots that are in Y
#X<-filter(X, plot %in% (row.names(Y))) #matching-don't think need..should be 81 
row.names(X)<-X$plot
X$plot<-NULL
X$year<-NULL

#subset Y by dominant spp (>999 hits)#
#spp_domminance$USDA_code<-factor(spp_domminance$USDA_code)
domspp<-as.character(unique(spp_abund$plant.sp))
Y_dom<-dplyr::select(Y, one_of(domspp))

#spp_subdom$USDA_code<-factor(spp_subdom$USDA_code)
#subdomspp<-as.character(spp_subdom$USDA_code)
#Y_subdom<-dplyr::select(Y, one_of(subdomspp))

#traits-IF USING----
#traits_all<-read.csv("C:/Users/court/Google Drive/CU Postdoc/Saddle grid/plant_traits/plant_trait.ms.data.csv") 
#calculate and replace missing SLA and LDMC if raw data available 
#traits_all$sla<-traits_all$LeafArea/traits_all$DryWeight
#traits_all$ldmc<-traits_all$DryWeight/traits_all$WetWeight
#traits_all<-mutate(traits_all, SLA=ifelse(is.na(SLA), sla, SLA)) 
#traits_all<-mutate(traits_all, LDMC=ifelse(is.na(LDMC), sla, LDMC)) 

#traits<-group_by(traits_all, USDA.Code, Latin.name)%>%
 # summarise_at(.vars = vars(OHeight, SLA, LDMC),
  #             .funs = mean, na.rm=TRUE)
#remove duplicate spp
#traits<-subset(traits, Latin.name!="Gentianoides algida"& USDA.Code!="NaN")
#traits = as.data.frame(traits)
#rownames(traits) = traits$USDA.Code

#replace with old USDA code format
#row.names(traits)[37]<-"CARUD"
#row.names(traits)[38]<-"CASCS2"
#row.names(traits)[67]<-"GEROT"
#row.names(traits)[130]<-"TRPAP"
#traits$USDA.Code = rownames(traits) 

#traits_dom<-filter(traits,row.names(traits)%in% domspp, .preserve = TRUE) 

#rownames(traits_dom) = traits_dom$USDA.Code

#traits_dom$USDA.Code<-NULL
#traits_dom$Latin.name<-NULL
#colnames(traits_dom)<-c("height", "sla", "ldmc") #check for no NAs

#phylogeny-use with traits
#C_dom<-read.csv("plant_traits/phylo_vcv_dom_999.csv") 
#rownames(C_dom)<-C_dom$X
#C_dom$X<-NULL


#Study design Pi----
Pi<-factor(row.names(X))
Pi<-data.frame(Pi)
colnames(Pi)<-"plot"

#spatially explicit 

#temporally explicit

#OLD version code
#make HMSC object----
#ymat<-as.matrix(Y_traits)
ymat<-as.matrix(Y_dom)
#ymat<-as.matrix(Y_subdom)
#ymat<-as.matrix(Y)
xmat<-as.matrix(X)
#traits<-t(traits_dom) #spp as columns-need
#trmat<-as.matrix(traits)
#cmat<-as.matrix(C_dom)

#run model----
formdata <- as.HMSCdata(Y = ymat, X = xmat, Random = Pi, scaleX = T, scaleTr = T)
#formdata <- as.HMSCdata(Y = ymat, X = xmat, Random = Pi)
model<- hmsc(data = formdata, family = "gaussian", param = NULL, priors=NULL, niter = 10000, nburn = 1000, thin = 10, verbose=T)
save(model, file = "HMSC_Saddlegrid_2010_dom_traits_soil.RData") 

load(file = "HMSC_Saddlegrid_2010_dom_traits_soil.RData")

#RESULTS----
#traceplots
mixing<- as.mcmc(model, parameters = "paramX")  
pdf("HMSC_Saddlegrid_2014_dom_traits_soil_B.pdf")
plot(mixing) #only one chain in the trace plots-old version
dev.off()
str(mixing)

mixingtr<-as.mcmc(model, parameters = "paramTr")
pdf("HMSC_Saddlegrid_",yr,"_dom_traits_soil_Tr.pdf")
plot(mixingtr) #only one chain in the trace plots-old version
dev.off()



#Spp correlations 
corMat <- corRandomEff(model, cor = TRUE )
averageCor <- apply(corMat[, , , 1], 1:2, mean)
corrplot(averageCor, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.45, diag = FALSE, 
  title = "200", mar = c(2, 2, 2, 2))  
write.csv(averageCor, "HMSC_model_output/spp_cor_2012_dom_traits_soil.csv")

model$data$X
#variance partitioning 
variationPart <- variPart(model,groupX =c("Int", "Moisture","pH", "total C", "Elev", "Solar rad", "Slope", "Aspect",  "NPP", "Snow depth"))
vp<-as.data.frame(variationPart)
par(mfrow=c(1, 1), mar=c(5, 5, 4, 6))
barplot(t(vp),  col = viridis(11, option = "B"),las=2, cex.names = 0.8, ylab = "Proportion of variance", main=yr, legend.text=colnames(vp), args.legend=list(x= "topright",inset=c(-0.3, -0.05),bty = "n"))

variationPart

#Avg % explained 
varpart<-as.data.frame(variationPart$variPart)
mean(varpart$plot)
mean(varpart$Elev)#topo
mean(varpart$Slope)#topo
mean(varpart$Aspect)#topo
mean(varpart$`Soil moisture`)#soil
mean(varpart$pH)#soil
mean(varpart$`Organic C`)#soil
mean(varpart$`Solar rad`)#climate/topo
mean(varpart$NPP)#total resource availability?
mean(varpart$`Snow depth`)
write.csv(varpart, "HMSC_model_output/variance_part_2014_dom_soil.csv")
variationPart$traitR2 #traits 

#significance of Beta params 
model$results$estimation$paramX
meanX <- apply(model$results$estimation$paramX,1:2,mean)
sdX<- apply(model$results$estimation$paramX,1:2,sd)
p.val <- pnorm(0,meanX, sdX) #all pvalues of the fixed effects by species 

#significance of trait params 
meanTr <- apply(model$results$estimation$paramTr,1:2,mean)
sdTr<- apply(model$results$estimation$paramTr,1:2,sd)
p.val.tr <- pnorm(0,meanTr, sdTr) #all pvalues of the traits by species 

#plot p rho-phylo signal for response to enviro
#boxplot(model$results$estimation$paramPhylo) #very strong signal 

#model fit
Ymean <- apply(model$data$Y,2,mean)
R2 <- Rsquared(model, averageSp=FALSE)
plot(Ymean,R2)
sort(R2)
R2<-as.data.frame(R2)
p.val<-merge(p.val, R2, by="row.names")
write.csv(p.val, "HMSC_model_output/Beta_significance_2014_dom.csv")

#grid.arrange(P ncol=2, widths=c(2.5,2), heights=c(2,2))



#Calculate niche breadth 
mixingdf<-as.data.frame(mixing)
mixingdf<-t(mixingdf)

niche<-matrix(NA,160,2)

for(i in 1:160){
niche[i,1]<-min(mixingdf[i,])
niche[i,2]<-max(mixingdf[i,])
}

niche<-as.data.frame(niche)
niche<-rename(niche, min=V1, max=V2)
row.names(niche)<-row.names(mixingdf)
niche$width<-niche$max-niche$min
niche<-niche[c(17:160),]#take out intercepts 

niche$spp<-rownames(niche)
niche <- separate(niche, spp, c("spp", "enviro"), extra="merge")
niche$year<-2008

#spp interactions predicted by niche breadth? 
cor<-read.csv("HMSC_model_output/spp_cor_2018_dom_traits_soil.csv")
row.names(cor)<-cor$X
cor$X<-NULL
cor[is.na(cor)] <- 1

avgcor<-matrix(NA,16,1)
for(i in 1:16){
  avgcor[i]<-colMeans(cor[i])
}
row.names(avgcor)<-row.names(cor)
avgcor<-as.data.frame(avgcor)
avgcor$spp<-row.names(avgcor)
avgcor<-rename(avgcor, avgcor=V1)

niche<-left_join(niche, avgcor)

nichesub<-subset(niche,enviro=="max_depth"|enviro=="easting"|enviro=="NPP"|enviro=="moisture")

ggplot(nichesub, aes(y=avgcor, x=width, colour=spp, shape=enviro))+
         geom_point(size=1.5)+ ggtitle("2018")

    