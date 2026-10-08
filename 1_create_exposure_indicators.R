## read in indicators from ND-GAIN and create index that we use in our regressions and figures

rm(list=ls())
library(magrittr)
library(readxl)
library(tidyverse)

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}
if(onUser("pberg")){
  # setwd("~/Documents/GitHub/climateconcern")
  setwd("~/Documents/GitHub/climateconcern")
}

# read in ND-GAIN data ####
ndgain<-list.files("predictors/ND-GAIN2026/indicators/")
dir1<-"predictors/ND-GAIN2026/indicators"
ndgain<-ndgain[grep(paste0(c("01","02"),collapse="|"),ndgain)] ## subset to 1st 2 indicators in each category
## remove governance indicators, and business climate, and hydropower risk 
## because it is intended be weighted in a way that we don't have the data to do 
ndgain<-ndgain[-grep(paste0(c("soci","gove","econ","id_infr_01"),collapse="|"),ndgain)] 
d<-list()
for(i in 1:length(ndgain)){
  d[[i]]<-read_csv(paste(dir1,ndgain[i],"score.csv",sep="/"))%>%
    select(ISO3,Name,`2024`)%>%
    mutate(var=substr(ndgain[i],4,nchar(ndgain[i])))%>%
    rename(value=`2024`)
  ## identify each variable but remove "id_" prefix
}
d<-do.call(rbind,d)
unique(d$var)
sort(unique(d$ISO3))
## clean up data--eliminate duplicates
dups<-d%>%select(ISO3,Name)%>%distinct()
print(dups[which(duplicated(dups$ISO3)==TRUE),],n=Inf) ## different countries have different names but need to remove the Sao Paolo entry for Brazil which is duplicative

d<-d%>%
  group_by(ISO3)%>%
  summarise(exposure=mean(value,na.rm=TRUE))

## load 3-letter iso codes, names, and 2-letter iso codes, so that we can merge with concern data
countrycodes<-read_csv("basedata/Covariates/region_covariates.csv")
countrycodes<-countrycodes%>%
  select(iso_3166,GID_0_gadm,NAME_0_gadm)%>%
  filter(!is.na(GID_0_gadm))%>%
  distinct()

d<-left_join(countrycodes,d,by=c("GID_0_gadm"="ISO3"))

write_csv(d,file="predictors/ND-GAIN2026_exposure.csv")

# create emissions indicators ####
emissions<-read_excel("predictors/EDGAR_2025_GHGs_CO2eq_AR5_NUTS2_by_country_sector_1990-2024_b.xlsx",
                      sheet=2)
emissions%<>%
  filter(Sector=="Energy")%>%
  select(ISO,Country,`Subnational code *`,Y_2024)%>%
  filter(`Subnational code *`!="N/A")
unique(emissions$Country)
crosswalk<-read_excel("predictors/EU_crosswalk.xlsx",sheet=1)
emissions<-left_join(emissions,crosswalk,by=c("Subnational code *"="eucodes")) ## match nuts codes to codes used in our data
emissions<-emissions%>%
  group_by(Country,ourcodes)%>%
  summarise(emissions.cumul=sum(Y_2024,na.rm=TRUE), ## aggregate across EU regions with same code in our dataset
            emissions.cumul=emissions.cumul*1000) ## convert to tons rather than kilotons
pop<-read_csv("basedata/Covariates/region_covariates.csv")%>%
  select(mergekey,region_pop)
emissions<-left_join(emissions,pop,by=c("ourcodes"="mergekey"))%>%
  mutate(emissions.percap=emissions.cumul/region_pop)
emissions$emissions.percap.bin<-cut(emissions$emissions.percap,breaks=c(-1,1,2,3,4,5,10,20,100),
                                    labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","20+"))

## this dataset only includes EU countries because of the NUTS-ourcodes crosswalk. for now, eliminate any rows without a 
## value in the "ourcodes" column and that will eliminate NA values
emissions%<>%
  filter(!is.na(ourcodes))

write_csv(emissions,"predictors/emissions_clean.csv")
