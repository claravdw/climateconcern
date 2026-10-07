## read in indicators from ND-GAIN and create index that we use in our regressions and figures

rm(list=ls())
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
ndgain<-ndgain[grep(paste0(c("01","02"),collapse="|"),ndgain)] ## subset to 1st 2 indicators
ndgain<-ndgain[-grep(paste0(c("soci","gove","econ","id_infr_01"),collapse="|"),ndgain)] ## remove governance indicators, and business climate, and hydropower risk because it should be weighted in a way that we don't have the data for. 
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

