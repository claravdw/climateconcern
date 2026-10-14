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
# setwd("~/Documents/GitHub/climateconcern") ## uncomment once data are publicly shareable
setwd("~/Library/CloudStorage/Dropbox/global_mrp") ## comment this out once data are publicly shareable
repofolder<-"~/Documents/GitHub/climateconcern/"
figfolder<-"~/Documents/GitHub/climateconcern/figures/"

# # load data #### 
 modelname<-"climate_concern_v2_revision" 
timecollapse<-"2yr"
qrestrict<-"concernhuman"
datafilter<-paste(timecollapse,qrestrict,sep="_")
load(file=paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))

est.reg%<>%
  filter(year=="2010-11"|year=="2024-25")
est.nat%<>%
  filter(year=="2010-11"|year=="2024-25")

# load population data
load(paste0(repofolder,"basedata/populations_extract_worldpop/populations.Rda"))
load(paste0(repofolder,"basedata/populations_extract_worldpop/populations_national.Rda"))

populations.combine <- merge(populations, populations.national, by="iso_3166")

#merge in sample sizes at region and national level
samples.reg <- d%>%group_by(mergekey)%>%summarise(n=sum(size))
samples.nat <- d%>%group_by(iso_3166)%>%summarise(n=sum(size))

  
est.reg <- merge(est.reg, samples.reg, by="mergekey", all.y=FALSE)
est.nat <- merge(est.nat, samples.nat, by="iso_3166", all.y=FALSE)

est.2024<-est.nat%>%filter(year=="2024-25")

est.reg.2024<-est.reg%>%filter(year=="2024-25")


est.reg.2024 <- data.table(est.reg.2024)

#remove regions with sample less than 1000 
est.reg.2024 <- est.reg.2024[est.reg.2024$n>999,]
est.reg.2024 <- est.reg.2024[est.reg.2024$mergekey!="CN-HK",]

# within-country regional dispersion, on the raw latent scale
nat.descriptives <- est.reg.2024[, .(
  nat.regionsd  = sd(mean),
  nat.regioniqr = IQR(mean),
  nat.regionmean = mean(mean)
), by = .(iso_3166)]

# merge in the national (population-weighted) estimate and its posterior SD
est.nat.sub <- est.2024[,c("iso_3166", "mean", "sd", "Continent_Name", "NAME_0_gadm")]
names(est.nat.sub)[2:3] <- c("nat.mean","nat.sd")
nat.descriptives <- merge(nat.descriptives, est.nat.sub, by="iso_3166")
est.reg.2024 <- merge(est.reg.2024, nat.descriptives, by=intersect(names(est.reg.2024),names(nat.descriptives)))

# regional departure from the national mean, as a difference
est.reg.2024$reg.diff <- est.reg.2024$mean - est.reg.2024$nat.mean

# approximate posterior uncertainty on that difference, using the per-unit posterior SDs
# already in the data. NOTE: this treats the region and national posteriors as independent,
# which they aren't (the national estimate is built from the regions), so this is a
# conservative (likely too-wide) approximation, not the exact joint posterior. If you have
# access to the raw posterior draws (not just this summary object) I can compute the exact
# draw-wise difference instead, which would be tighter and fully correct.
est.reg.2024$reg.diff.se <- sqrt(est.reg.2024$sd^2 + est.reg.2024$nat.sd^2)
est.reg.2024$reg.diff.lo <- est.reg.2024$reg.diff - 1.645*est.reg.2024$reg.diff.se  # ~90%, to match your existing q5/q95 convention
est.reg.2024$reg.diff.hi <- est.reg.2024$reg.diff + 1.645*est.reg.2024$reg.diff.se

library(sf)
regions <- st_read(paste0(repofolder,"basedata/simplified geometries/g1.agg_wmissingcountries_simplifedgeometry.gpkg"))

regions.sub <- regions[,c("mergekey", "regionname_ISOenglish")]
est.reg.2024.merge <- merge(est.reg.2024, regions.sub, by="mergekey")
est.reg.2024.merge$regionname_countryname <- paste0(est.reg.2024.merge$regionname_ISOenglish, ", ", est.reg.2024.merge$NAME_0_gadm.x)

# And updating the two figures (replace reg.ratio → reg.diff, nat.CV → nat.regionsd):
## Fig 5b: regional departure from national mean, with approximate uncertainty
top5_bycountry<-data.frame(est.reg.2024.merge[est.reg.2024.merge$iso_3166%in% c("US", "CN", "IN")])%>%
  select(regionname_ISOenglish,iso_3166,reg.diff,reg.diff.lo,reg.diff.hi,NAME_0_gadm)%>%
  distinct()%>%
  filter(!is.na(reg.diff))%>%
  group_by(iso_3166)%>%
  arrange(reg.diff)%>%
  slice_tail(n=5)%>%
  ungroup()

bottom5_bycountry<-data.frame(est.reg.2024.merge[est.reg.2024.merge$iso_3166%in% c("US", "CN", "IN")])%>%
    select(regionname_ISOenglish,iso_3166,reg.diff,reg.diff.lo,reg.diff.hi,NAME_0_gadm)%>%
    distinct()%>%
    filter(!is.na(reg.diff))%>%
    group_by(iso_3166)%>%
    arrange(reg.diff)%>%
    slice_head(n=5)%>%
    ungroup()

top.bottom5fig.regions.bycountry<-rbind(top5_bycountry,bottom5_bycountry)%>%
  ggplot(aes(x=reg.diff,y=reorder(regionname_ISOenglish, reg.diff)))+
  geom_vline(xintercept=0,lty="dashed",color="gray50")+
  geom_point()+
  geom_linerange(aes(xmin=reg.diff.lo,xmax=reg.diff.hi))+
  labs(y=element_blank(),x="Regional departure from national mean\n(climate concern, latent scale)")+
  theme_bw()+
  guides(color="none")+
  facet_wrap(~NAME_0_gadm,scales="free_y")

quartz(width=10,height=6)
plot(top.bottom5fig.regions.bycountry)
ggsave(file=paste0(figfolder,"Subnational_deviance_top5bottom5_bytop5countries_",modelname,".pdf"),width=10,height=6,top.bottom5fig.regions.bycountry)

## Fig 5a: within-country IQR across regions, 15 largest countries
top15 <- nat.descriptives[nat.descriptives$iso_3166 %in% c("US", "DE", "GB", "FR", "IN", "CN", "ES", "IT", "PL", "NL", "FI", "SE","MX", "HU", "CZ"),]
top15fig <- top15 %>%
  ggplot(aes(x=nat.regioniqr,y=reorder(NAME_0_gadm, nat.regioniqr)))+
  geom_point()+
  labs(y=element_blank(),x="Within-country regional inter-quartile range (climate concern, latent scale)")+
  theme_bw()+
  guides(color="none")
top15fig
ggsave(file=paste0(figfolder,"Subnational_iqr_top15data_climate_concern_v2_revision.pdf"),top15fig)


# figure S19: IQR for all countries with at least 25,000 respondents in sample 
sample25k<-nat.descriptives%>%
  left_join(samples.nat)%>%
  filter(n>25000,!is.na(nat.regioniqr))%>%arrange(desc(nat.regioniqr))
fig25k<-sample25k%>%
  ggplot(aes(x=nat.regioniqr,y=reorder(NAME_0_gadm, nat.regioniqr)))+
  geom_point(aes(y=reorder(NAME_0_gadm, nat.regioniqr)))+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y=element_blank(),x=element_blank(),color="Continent")+
  theme_bw()+
  theme(axis.text.y=element_text(size=5))+
  guides(color="none")
fig25k
ggsave(file=paste0(figfolder,"Subnational_IQR_all25kplus_",modelname,".pdf"),width=6.5,height=8.5,fig25k)
