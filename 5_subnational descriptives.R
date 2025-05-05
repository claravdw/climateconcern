# This code runs analyses in which our climate concern model is the outcome variable # 
# Preamble #### 
rm(list=ls())
library(reshape2)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(broom)
library(ggforce)
library(lfe)
library(modelsummary)
library(sensemakr)
library(xtable)
library(estimatr)
library(texreg)
library(tidyverse)
library(data.table)

# source("globalmrp_functions.R")


setwd("~/Documents/GitHub/globalmrp/globalmrp_hpc")
figfolder<-"~/Documents/GitHub/globalmrp/figures/"
#paperfigfolder<-"~/Dropbox/Apps/Overleaf/globalmrp/figures/"

# # load data #### 
 modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
# timecollapse<-"2yr"
# qrestrict<-"concernhuman"
# datafilter<-paste(timecollapse,qrestrict,sep="_")

est.reg <- read.csv(file="analyzed_outputs/estimates_regional_2010-22_country_walk_region_walk_fxdstart.csv")
est.nat <- read.csv(file="analyzed_outputs/estimates_2010-22_country_walk_region_walk_fxdstart.csv")

#load("outputs_stan/stan_clean_country_walk_region_walk_fxdstart.Rdata")
load("outputs_stan/stan_clean_country_walk_region_walk_fxdstart_2yr_concernhuman.Rdata")


# load region key
load("individual polls/regioncodes/region_key_disaggregated.Rda")

# load population data
load("basedata/populations_extract_worldpop/populations.Rda")
load("basedata/populations_extract_worldpop/populations_national.Rda")

populations.combine <- merge(populations, populations.national, by="iso_3166")

# if(exists("datafilter")){
#   load(paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
# }else{
#   load(paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
# }


#merge in sample sizes at region and national level
samples.reg <- d%>%group_by(mergekey)%>%summarise(n=sum(size))
samples.nat <- d%>%group_by(iso_3166)%>%summarise(n=sum(size))

  
est.reg <- merge(est.reg, samples.reg, by="mergekey", all.y=FALSE)
est.nat <- merge(est.nat, samples.nat, by="iso_3166", all.y=FALSE)

est.2022<-est.nat%>%filter(year=="2022-23")

est.reg.2022<-est.reg%>%filter(year=="2022-23")


est.reg.2022 <- data.table(est.reg.2022)

#remove regions with sample less than 1000 
est.reg.2022 <- est.reg.2022[est.reg.2022$n>999,]
est.reg.2022 <- est.reg.2022[est.reg.2022$mergekey!="CN-HK",]


#calculate national coefficient of variation by region
nat.descriptives <- est.reg.2022[, .(nat.CV = sd(mean.scl)/mean(mean.scl), nat.regionmean = mean(mean.scl), nat.regionsd = sd(mean.scl)), by = .(iso_3166)]

#calculate regional location quotient by country
est.nat.sub <- est.2022[,c("iso_3166", "mean.scl", "Continent_Name", "NAME_0_gadm")]
names(est.nat.sub)[2] <- "nat.mean.scl"
nat.descriptives <- merge(nat.descriptives, est.nat.sub, by="iso_3166")
est.reg.2022 <- merge(est.reg.2022, nat.descriptives, by="iso_3166")

#est.reg.2020.pop <- merge(est.reg.2020, populations.combine, by="mergekey")

est.reg.2022$reg.ratio <- (est.reg.2022$mean.scl / est.reg.2022$nat.mean.scl) *100
#nat.descriptives <- merge(nat.descriptives, populations.national, by="iso_3166")

#figures of regions that are highly dissimilar from their country
library(sf)
regions <- st_read("basedata/simplified geometries/g1.agg_wmissingcountries_simplifedgeometry.gpkg")

regions.sub <- regions[,c("mergekey", "regionname_ISOenglish")]
est.reg.2022.merge <- merge(est.reg.2022, regions.sub, by="mergekey")
est.reg.2022.merge$regionname_countryname <- paste0(est.reg.2022.merge$regionname_ISOenglish, ", ", est.reg.2022.merge$NAME_0_gadm.x)

## regional location quotient by continent
top5<-data.frame(est.reg.2022.merge)%>%
  select(regionname_countryname,Continent_Name,reg.ratio)%>%
  distinct()%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(reg.ratio))%>%
  group_by(Continent_Name)%>%
  arrange(reg.ratio)%>%
  slice_tail(n=5)%>%
  ungroup()

bottom5<-data.frame(est.reg.2022.merge)%>%
  select(regionname_countryname,Continent_Name,reg.ratio)%>%
  distinct()%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(reg.ratio))%>%
  group_by(Continent_Name)%>%
  arrange(reg.ratio)%>%
  slice_head(n=5)%>%
  ungroup()


top.bottom5fig.regions<-rbind(top5,bottom5)%>%
  ggplot(aes(x=reg.ratio,y=reorder(regionname_countryname, reg.ratio)))+
  geom_point(aes(y=reorder(regionname_countryname, reg.ratio)))+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y="Country",x="Subnational location quotient (2022-23 climate concern)",color="Continent")+
  theme_bw()+
  guides(color="none")+
  facet_wrap(~Continent_Name,scales="free_y")
quartz(width=10,height=8)
plot(top.bottom5fig.regions)
ggsave(file=paste0(figfolder,"Subnational_lq_top5bottom5_bycontinent_",modelname,".pdf"),width=10,height=8,top.bottom5fig.regions)



## regional location quotient for selected countries
top5_bycountry<-data.frame(est.reg.2022.merge[est.reg.2022.merge$iso_3166%in% c("US", "CN", "IN")])%>%
  select(regionname_ISOenglish,iso_3166,reg.ratio,NAME_0_gadm.x)%>%
  distinct()%>%
  filter(!is.na(reg.ratio))%>%
  group_by(iso_3166)%>%
  arrange(reg.ratio)%>%
  slice_tail(n=5)%>%
  ungroup()

bottom5_bycountry<-data.frame(est.reg.2022.merge[est.reg.2022.merge$iso_3166%in% c("US", "CN", "IN")])%>%
  select(regionname_ISOenglish,iso_3166,reg.ratio,NAME_0_gadm.x)%>%
  distinct()%>%
  filter(!is.na(reg.ratio))%>%
  group_by(iso_3166)%>%
  arrange(reg.ratio)%>%
  slice_head(n=5)%>%
  ungroup()


top.bottom5fig.regions.bycountry<-rbind(top5_bycountry,bottom5_bycountry)%>%
  ggplot(aes(x=reg.ratio,y=reorder(regionname_ISOenglish, reg.ratio)))+
  geom_point(aes(y=reorder(regionname_ISOenglish, reg.ratio)))+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y=element_blank(),x="Subnational location quotient (2022-23 climate concern)")+
  theme_bw()+
  guides(color="none")+
  facet_wrap(~NAME_0_gadm.x,scales="free_y")
quartz(width=10,height=6)
plot(top.bottom5fig.regions.bycountry)
ggsave(file=paste0(figfolder,"Subnational_lq_top5bottom5_bytop5countries_",modelname,".pdf"),width=10,height=6,top.bottom5fig.regions.bycountry)








#figures of national within-country variation by global region
#p <- ggplot(nat.descriptives, aes(x=nat.CV, y=nat.mean.scl)) + geom_point() + facet_grid(~Continent_Name)
library(RColorBrewer)


summtop<-nat.descriptives%>%
  arrange(desc(nat.CV))%>%
  slice_head(n=30)
summbottom<-nat.descriptives%>%
  arrange(nat.CV)%>%
  slice_head(n=30)

hilo <-rbind(summtop,summbottom)%>%
  mutate(NAME_0_gadm=factor(NAME_0_gadm),
         NAME_0_gadm=fct_reorder(NAME_0_gadm,nat.CV))%>%
  arrange(NAME_0_gadm)
hilofig<-hilo%>%
  ggplot()+
  geom_point(aes(x=nat.CV,y=NAME_0_gadm))+
  theme_bw()+
  labs(x="Subnational coefficient of variation (2022-23)",y="")+

quartz(width=5,height=10)
hilofig
ggsave(paste0(figfolder,"Subnational_CV_highestlowest_",modelname,".pdf"),width=4,height=8,hilofig)


cvfig <- nat.descriptives%>%
  ggplot()+
  geom_point(aes(x=nat.mean.scl,y=nat.CV, color=Continent_Name))+# facet_grid(Continent_Name~.) +
  theme_bw()+
  labs(y="Subnational coefficient of variation (2022-23)",x="Climate concern (2022-23)",color="Continent")

quartz(width=8,height=8)
cvfig
ggsave(paste0(figfolder,"Subnational_CV_scatter_",modelname,".pdf"),width=8,height=8,cvfig)


## regional CV by country by continent
top5<-data.frame(nat.descriptives)%>%
  select(NAME_0_gadm,Continent_Name,nat.CV)%>%
  distinct()%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(nat.CV))%>%
  group_by(Continent_Name)%>%
  arrange(nat.CV)%>%
  slice_tail(n=5)%>%
  ungroup()

bottom5<-data.frame(nat.descriptives)%>%
  select(NAME_0_gadm,Continent_Name,nat.CV)%>%
  distinct()%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(nat.CV))%>%
  group_by(Continent_Name)%>%
  arrange(nat.CV)%>%
  slice_head(n=5)%>%
  ungroup()


top.bottom5fig<-rbind(top5,bottom5)%>%
  ggplot(aes(x=nat.CV,y=reorder(NAME_0_gadm, nat.CV)))+
  geom_point(aes(y=reorder(NAME_0_gadm, nat.CV)))+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y="Country",x="Subnational coefficient of variation (2022-23 climate concern)",color="Continent")+
  theme_bw()+
  guides(color="none")+
  facet_wrap(~Continent_Name,scales="free_y")
quartz(width=8,height=8)
plot(top.bottom5fig)
ggsave(file=paste0(figfolder,"Subnational_CV_top5bottom5_bycontinent_",modelname,".pdf"),width=6.5,height=6.5,top.bottom5fig)

## regional CV for 15 largest sample size countries
top15 <- nat.descriptives[nat.descriptives$iso_3166 %in% c("US", "DE", "GB", "FR", "IN", "CN", "ES", "IT", "PL", "NL", "FI", "SE","MX", "HU", "CZ"),]
 
top15fig <- top15 %>%
  ggplot(aes(x=nat.CV,y=reorder(NAME_0_gadm, nat.CV)))+
  geom_point(aes(y=reorder(NAME_0_gadm, nat.CV)))+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y=element_blank(),x=element_blank(),color="Continent")+
  theme_bw()+
  guides(color="none")
quartz(width=6,height=6)
plot(top15fig)
ggsave(file=paste0(figfolder,"Subnational_CV_top15data_",modelname,".pdf"),width=6.5,height=6.5,top15fig)


