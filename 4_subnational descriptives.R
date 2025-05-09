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

## set your working directory here 
setwd("~/Documents/GitHub/climateconcern")
figfolder<-"~/Documents/GitHub/climateconcern/figures/"

# # load data #### 
 modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
est.reg <- read.csv(file="analyzed_outputs/estimates_regional_2010-22_country_walk_region_walk_fxdstart.csv")
est.nat <- read.csv(file="analyzed_outputs/estimates_2010-22_country_walk_region_walk_fxdstart.csv")

load("outputs_stan/stan_clean_country_walk_region_walk_fxdstart_2yr_concernhuman.Rdata")


# load region key
load("individual polls/regioncodes/region_key_disaggregated.Rda")

# load population data
load("basedata/populations_extract_worldpop/populations.Rda")
load("basedata/populations_extract_worldpop/populations_national.Rda")

populations.combine <- merge(populations, populations.national, by="iso_3166")

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
nat.descriptives <- merge(nat.descriptives, est.nat.sub, by=intersect(names(nat.descriptives),names(est.nat.sub)))
est.reg.2022 <- merge(est.reg.2022, nat.descriptives, by=intersect(names(est.reg.2022),names(nat.descriptives)))

#est.reg.2020.pop <- merge(est.reg.2020, populations.combine, by="mergekey")

est.reg.2022$reg.ratio <- (est.reg.2022$mean.scl / est.reg.2022$nat.mean.scl) *100
#nat.descriptives <- merge(nat.descriptives, populations.national, by="iso_3166")

library(sf)
regions <- st_read("basedata/simplified geometries/g1.agg_wmissingcountries_simplifedgeometry.gpkg")

regions.sub <- regions[,c("mergekey", "regionname_ISOenglish")]
est.reg.2022.merge <- merge(est.reg.2022, regions.sub, by="mergekey")
est.reg.2022.merge$regionname_countryname <- paste0(est.reg.2022.merge$regionname_ISOenglish, ", ", est.reg.2022.merge$NAME_0_gadm.x)

## Fig 5b: regional location quotient for selected countries
top5_bycountry<-data.frame(est.reg.2022.merge[est.reg.2022.merge$iso_3166%in% c("US", "CN", "IN")])%>%
  select(regionname_ISOenglish,iso_3166,reg.ratio,NAME_0_gadm)%>%
  distinct()%>%
  filter(!is.na(reg.ratio))%>%
  group_by(iso_3166)%>%
  arrange(reg.ratio)%>%
  slice_tail(n=5)%>%
  ungroup()

bottom5_bycountry<-data.frame(est.reg.2022.merge[est.reg.2022.merge$iso_3166%in% c("US", "CN", "IN")])%>%
  select(regionname_ISOenglish,iso_3166,reg.ratio,NAME_0_gadm)%>%
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
  facet_wrap(~NAME_0_gadm,scales="free_y")
quartz(width=10,height=6)
plot(top.bottom5fig.regions.bycountry)
ggsave(file=paste0(figfolder,"Subnational_lq_top5bottom5_bytop5countries_",modelname,".pdf"),width=10,height=6,top.bottom5fig.regions.bycountry)


# Fig. 5a: regional CV for 15 largest sample size countries
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

# figure S19: regional CV for all countries with at least 25,000 respondents in sample 
sample25k<-nat.descriptives%>%
  left_join(samples.nat)%>%
  filter(n>25000,!is.na(nat.CV))%>%arrange(desc(nat.CV))
fig25k<-sample25k%>%
  ggplot(aes(x=nat.CV,y=reorder(NAME_0_gadm, nat.CV)))+
  geom_point(aes(y=reorder(NAME_0_gadm, nat.CV)))+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y=element_blank(),x=element_blank(),color="Continent")+
  theme_bw()+
  theme(axis.text.y=element_text(size=5))+
  guides(color="none")
fig25k
ggsave(file=paste0(figfolder,"Subnational_CV_all25kplus_",modelname,".pdf"),width=6.5,height=8.5,fig25k)
