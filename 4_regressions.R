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
library(magrittr)
library(tidyverse)
# source("globalmrp_functions.R")


onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}
if(onUser("pberg")){
  setwd("~/Documents/GitHub/globalmrp/globalmrp_hpc")
  figfolder<-"~/Documents/GitHub/globalmrp/figures/"
  paperfigfolder<-"~/Dropbox/Apps/Overleaf/globalmrp/figures/"
  # dropbox<-"~/Dropbox (Personal)/global_mrp/"
}
## Clara set up your folders here
if(onUser("clara")){
  setwd("~/Documents/globalmrp/globalmrp_hpc")
  figfolder<-"~/Documents/globalmrp/figures/"
  paperfigfolder<-figfolder
}


# load data #### 
modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
timecollapse<-"2yr"
qrestrict<-"concernhuman"
datafilter<-paste(timecollapse,qrestrict,sep="_")
# load region key
load("individual polls/regioncodes/region_key_disaggregated.Rda")


if(exists("datafilter")){
  load(paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
}else{
  load(paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
}
est.2020<-est.nat%>%filter(year=="2020-21")
est.2018<-est.nat%>%filter(year=="2018-19") ## for green voting data since the last year of that data is 2019

# est.2020<-est.2020%>%
#   select(NAME_0_gadm,Continent_Name,year2,iso_3166,mean)
# read in ND-GAIN data #### 
# ndgain<-list.files("ND-GAIN2022/indicators/")
# dir1<-"ND-GAIN2022/indicators"
# ndgain<-ndgain[grep(paste0(c("01","02"),collapse="|"),ndgain)] ## subset to 1st 2 indicators
# ndgain<-ndgain[-grep(paste0(c("soci","gove"),collapse="|"),ndgain)] ## remove governance indicators
# 
# d<-list()
# for(i in 1:length(ndgain)){
#   d[[i]]<-read_csv(paste(dir1,ndgain[i],"score.csv",sep="/"))%>%
#     select(ISO3,Name,`2020`)%>%
#     mutate(var=substr(ndgain[i],4,nchar(ndgain[i])))%>%
#     rename(value=`2020`)
#   ## identify each variable but remove "id_" prefix
# }
# d<-do.call(rbind,d)
# unique(d$var)
# sort(unique(d$ISO3))
# ## clean up data--eliminate duplicates 
# dups<-d%>%select(ISO3,Name)%>%distinct()
# print(dups[which(duplicated(dups$ISO3)==TRUE),],n=Inf) ## different countries have different names but need to remove the Sao Paolo entry for Brazil which is duplicative 
# 
# d<-d%>%filter(Name!="Brazil S<U+00E3>o Paulo")
# 
# d<-d%>%
#   group_by(ISO3)%>%
#   summarise(exposure=mean(value,na.rm=TRUE))
# 
# ## load 3-letter iso codes, names, and 2-letter iso codes, so that we can merge with concern data
# countrycodes<-read_csv("basedata/Covariates/region_covariates.csv")
# countrycodes<-countrycodes%>%
#   select(iso_3166,GID_0_gadm,NAME_0_gadm)%>%
#   filter(!is.na(GID_0_gadm))%>%
#   distinct()
# 
# d<-left_join(countrycodes,d,by=c("GID_0_gadm"="ISO3"))
# 
# write_csv(d,file="ND-GAIN2022/exposure.csv")

# green voting data #### 
load("predictors/hoffman_greenvoting/vote_climate.RData")

# crosswalk<-readxl::read_excel("predictors/EU_crosswalk.xlsx",sheet=1) ## spreadsheet that Emma created 
# ## ourcodes is the mergekey for the region, in our dataset, and eucodes corresponds to the level 2 nuts code as used in the climate_voting dataset
# crosswalk%<>%rename(mergekey=ourcodes)
# load new crosswalk (created from lines below) and merge in 
crosswalk<-read_csv("predictors/nuts2_crosswalk_240730.csv")



vote_climate%<>%
  filter(year==2019)%>%
  select(unit,nuts0,green_vote)%>%
  left_join(crosswalk,by=c("unit"="nuts2"))  



# # # find regions for which we're still missing geographic codes.
# est.reg.europe.2018<-est.reg%>%
#   filter(Continent_Name=="Europe",year=="2018-19")%>%
#   left_join(vote_climate%>%select(unit,green_vote),by=c("mergekey"="unit"))
# 
# ## filter to rows where we're missing green voting data, and save for emma 
# est.reg.europe.2018%<>%
#   filter(is.na(green_vote),
#          grepl(".nat",mergekey)==FALSE)

# ## filter to rows in estimates dataset for which we are missing nuts id data, and save for Emma to find
# navoting<-est.reg.europe.2018%>%
#   select(mergekey,unit,NAME_0_gadm,iso_3166)%>%
#   filter(is.na(unit))
# write_csv(est.reg.europe.2018,file="code troubleshooting/voting_missing_nuts_codes_240730.csv") 
## sent this to emma to ask her to find nuts codes for the places that don't have them

est.reg.europe.2018<-est.reg%>%
  filter(Continent_Name=="Europe",year=="2018-19")
# View(vote_climate%>%filter(mergekey%in%(intersect(est.reg.europe.2018$mergekey,vote_climate$mergekey))))
est.reg.europe.2018%<>%
  left_join(vote_climate,by="mergekey")%>%
  select(mergekey,iso_3166,nuts0,mean.std,green_vote)
table(est.reg.europe.2018$iso_3166[!is.na(est.reg.europe.2018$green_vote)])
# est.reg.europe.2018%<>%filter(!is.na(green_vote)) ## only 78 observations here :/ somehow this landed us with fewer, rather than more, observations
summary(lm_robust(green_vote~mean.std,data=est.reg.europe.2018,fixed_effects = iso_3166))
## negative, and statistically insignificant. plotting this shows a flat relationship in most places 
## with strong negative slopes in SE (Sweden) and NL (Netherlands)

ggplot(est.reg.europe.2018,aes(x=mean.std,y=green_vote,label=iso_3166,color=iso_3166,group=iso_3166))+
  geom_text(size=2)+
  geom_smooth(method="lm")+
  theme_bw()

# GDP data #### 
gdp<-read_csv("predictors/GDP_Wang_etal_2005.csv")



# merge exposure data with 2020 climate opinion estimates #### 
d<-read_csv("predictors/ND-GAIN2022/exposure.csv")


est.2020<-left_join(est.2020,d,by=c("iso_3166","NAME_0_gadm"))

est.2020$mean.std<-c(scale(est.2020$mean))
est.2020$exposure.std<-c(scale(est.2020$exposure))

reg1<-lm_robust(mean.std~exposure.std,data=est.2020)
summary(reg1)
reg1$coefficients

texreg(reg1,custom.coef.names=c("(Intercept)","Exposure (st.dev.)"),
       custom.model.names="Climate concern",
       include.ci=FALSE,label="tab:regressionsimple",
       caption="{ \\bf Effect of climate risk exposure on climate concern:} The table shows the results from an OLS regression of climate concern (standardized) on climate risk exposure (standardized) in 2020. The model was estimated using OLS regression with heteroskedasticity-robust standard errors.",
       file=paste0(paperfigfolder,"regression_results_simple.tex"),float.pos="!htpb")


# # analysis with economic data #### 
# gdp<-read_csv("basedata/P_Data_Extract_From_World_Development_Indicators/7b73f413-fe15-4dc2-9db5-f1ee3975c2c2_Data.csv")
# gdp<-gdp%>%
#   select(`Country Code`,`2020 [YR2020]`)%>%
#   rename("gdp"="2020 [YR2020]")
# pop<-read_csv("basedata/Covariates/region_covariates.csv")
# pop<-pop%>%
#   select(GID_0_gadm,region_pop)%>%
#   group_by(GID_0_gadm)%>%
#   summarise(pop=sum(region_pop,na.rm=TRUE))%>%
#   ungroup()
# gdp<-left_join(pop,gdp,by=c("GID_0_gadm"="Country Code"))
# gdp<-gdp%>%
#   mutate(gdp=as.numeric(gdp),
#          gdp.pc=gdp/pop)
# est.2020<-left_join(est.2020,gdp,by="GID_0_gadm")
# plot(density(est.2020$gdp.pc,na.rm=TRUE))
# est.2020$log.gdp<-log(est.2020$gdp.pc)
# reg2<-lm_robust(mean.std~exposure.std+log(gdp.pc),data=est.2020)
# summary(reg2)
# texreg(reg2,custom.coef.names=c("(Intercept)","Exposure (st.dev.)","Log(GDP per capita)"),
#        custom.model.names="Climate concern",
#        include.ci=FALSE,label="tab:regression",
#        caption="{ \\bf Effect of climate risk exposure and GDP on climate concern:} The table shows the results from an OLS regression of climate concern (standardized) on logged, per capita GDP and climate risk exposure (standardized) in 2020. The model was estimated using OLS regression with heteroskedasticity-robust standard errors.",
#        file=paste0(paperfigfolder,"regression_results.tex"),float.pos="!htpb")

# countrylabs<-est.2020%>%
#   filter((mean.scl>quantile(est.2020$mean.scl,na.rm=TRUE,probs=0.75)&exposure.scl<quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.25))|## high concern, low exposure
#   (mean.scl<quantile(est.2020$mean.scl,na.rm=TRUE,probs=.25)&exposure.scl>quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.75))| # low concern, high exposure
#     (mean.scl<quantile(est.2020$mean.scl,na.rm=TRUE,probs=.25)&exposure.scl<quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.25))| # low concern, exposure concern
#     (mean.scl>quantile(est.2020$mean.scl,na.rm=TRUE,probs=.75)&exposure.scl>quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.75))| # high concern, high exposure
#   NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany"))%>%
#   mutate(type=ifelse(NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany"),"emitter","outlier"))
# countrylabs%<>%mutate(NAME_0_gadm=ifelse(iso_3166=="CD","Congo",NAME_0_gadm))
# 
# lmplot1<-ggplot(est.2020,aes(x=exposure.scl,y=mean.scl,label=iso_3166))+
#   geom_text(color="lightgray")+
#   geom_smooth(method="lm",se=FALSE,lty="dashed")+
#   geom_text_repel(data=countrylabs,aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm))+
#   annotate(geom="text",x=2,y=-3,label="High exposure,\nlow concern",color="blue")+
#   annotate(geom="text",x=2,y=3,label="High exposure,\nhigh concern",color="blue")+
#   annotate(geom="text",x=-2,y=-3,label="Low exposure,\nlow concern",color="blue")+
#   annotate(geom="text",x=-2,y=3,label="Low exposure,\nhigh concern",color="blue")+
#   # geom_text_repel(data=countrylabs,aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm))+
#   # geom_text_repel(data=subset(est.2020,(
#   #   mean.scl>quantile(est.2020$mean.scl,na.rm=TRUE,probs=0.75)&exposure.scl<quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.25))|
#   #     (mean.scl<quantile(est.2020$mean.scl,na.rm=TRUE,probs=.25)&exposure.scl>quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.75))),
#   #   aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm))+
#   # geom_text_repel(data=subset(est.2020,
#   #                             NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany")),
#   #                 aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm),fontface="bold")+
#   annotate("text",x=3,y=.75,label=paste("beta == ",round(reg1$coefficients[2],2)),color="blue",parse=TRUE)+
#   annotate("text",x=3,y=.2,label=paste("p == ",round(reg1$p.value[2],2)),color="blue",parse=TRUE)+
#   theme_classic()+
#   theme(axis.line=element_blank())+
#   geom_vline(aes(xintercept=0))+
#   geom_hline(aes(yintercept=0))+
#   # guides(color="none")+
#   labs(x="Climate exposure (st.dev)",y="Climate concern (st.dev)")
# lmplot1

# ggsave(file=paste0(figfolder,"quadrants_exposure_concern.pdf"),
#        width=6.5,height=6.5)
# ggsave(file=paste0(paperfigfolder,"quadrants_exposure_concern.pdf"),
#        width=6.5,height=6.5)

# lmplot1<-ggplot(est.2020,aes(x=exposure.scl,y=mean.scl,label=iso_3166))+
#   geom_text(color="lightgray")+
#   geom_smooth(method="lm",se=FALSE,lty="dashed")+
#   geom_text_repel(data=countrylabs,aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm))+
#   annotate(geom="text",x=2,y=-3,label="High exposure,\nlow concern",color="blue")+
#   annotate(geom="text",x=2,y=3,label="High exposure,\nhigh concern",color="blue")+
#   annotate(geom="text",x=-2,y=-3,label="Low exposure,\nlow concern",color="blue")+
#   annotate(geom="text",x=-2,y=3,label="Low exposure,\nhigh concern",color="blue")+
#   # geom_text_repel(data=countrylabs,aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm))+
#   # geom_text_repel(data=subset(est.2020,(
#   #   mean.scl>quantile(est.2020$mean.scl,na.rm=TRUE,probs=0.75)&exposure.scl<quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.25))|
#   #     (mean.scl<quantile(est.2020$mean.scl,na.rm=TRUE,probs=.25)&exposure.scl>quantile(est.2020$exposure.scl,na.rm=TRUE,prob=.75))),
#   #   aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm))+
#   # geom_text_repel(data=subset(est.2020,
#   #                             NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany")),
#   #                 aes(x=exposure.scl,y=mean.scl,label=NAME_0_gadm),fontface="bold")+
#   # annotate("text",x=3,y=.15,label=paste("beta == ",round(reg1$coefficients[2],2)),color="blue",parse=TRUE)+
#   # annotate("text",x=3,y=-.15,label=paste("p == ",round(reg1$p.value[2],2)),color="blue",parse=TRUE)+
#   theme_classic()+
#   theme(axis.line=element_blank())+
#   geom_vline(aes(xintercept=0))+
#   geom_hline(aes(yintercept=0))+
#   # guides(color="none")+
#   labs(x="Climate exposure (st.dev)",y="Climate concern (st.dev)")
# lmplot1
# 
# ggsave(file=paste0(figfolder,"scatter_exposure_concern.pdf"),
#        width=6.5,height=6.5)
# ggsave(file=paste0(paperfigfolder,"scatter_exposure_concern.pdf"),
#        width=6.5,height=6.5)

# national analysis with error correction #### 
## load posteriors
load(paste0("analyzed_outputs/",modelname,".Rda"))
## convert to long-form to sample 
post.nat.20<-pivot_longer(post.nat,cols=-c(geog_int,year_int,param,iso_3166,year2),names_to="iter",values_to="mean.post")%>%
  filter(year2=="2020-21")%>%
  select(iso_3166,iter,mean.post)
lm1.moc.all<-NA
for(p in 1:500){
  print(p)
  iter.sample<-sample(unique(post.nat.20$iter),1)
  print(iter.sample)
  post.nat2<-filter(post.nat.20,iter==iter.sample)
  est.2020.2<-left_join(est.2020,post.nat2,by="iso_3166")
  est.2020.2$mean.post.std<-c(scale(est.2020.2$mean.post))
  lm1.moc<-tidy(lm_robust(mean.post.std~exposure.std,data=est.2020.2))#%>%
    # filter(term!="(Intercept)")
  lm1.moc<-lm1.moc%>%
    mutate(boot=p,
           estimate2=rnorm(n=dim(lm1.moc)[1]),mean=estimate,sd=std.error)
  lm1.moc.all<-rbind(lm1.moc.all,lm1.moc)
}
lm1.moc.all<-lm1.moc.all%>%
  filter(!is.na(term))
lm1.moc.all<-lm1.moc.all%>%
  group_by(term)%>%
  summarise(estimate=mean(estimate2),
            std.error=sd(estimate2),
            statistic=estimate/std.error)%>%
  ungroup()

# combine with uncorrected results for a CI plot
lm1.moc.all$mod<-"Error corrected"
lm1<-tidy(reg1)%>%mutate(mod="Uncorrected")%>%
  select(term,estimate,std.error,statistic,mod)
mod1plot<-rbind(lm1.moc.all,lm1)%>%
  mutate(ci.high=estimate+1.96*std.error,
         ci.low=estimate-1.96*std.error)%>%
  filter(term!="(Intercept)")%>%
  mutate(term=case_when(term=="log(gdp.pc)"~"GDP per capita\n(logged)",
                        term=="exposure.std"~"Climate exposure\n(std.dev)"))%>%
  ggplot(aes(x=estimate,y=term,shape=mod))+
  geom_pointrange(aes(xmin=ci.low,xmax=ci.high),position=position_dodge(width=.5))+
  scale_shape(guide=guide_legend(reverse=TRUE))+
  geom_vline(xintercept=0,lty="dashed")+
  theme_bw()+
  # theme(legend.position="bottom")+
  labs(x="Effect on climate concern (std.dev)",y="",shape="Model")


ggsave(file=paste0(paperfigfolder,"ciplot.pdf"),width=6.5,height=3.5,mod1plot)
ggsave(file=paste0(figfolder,"ciplot.pdf"),width=6.5,height=3.5,mod1plot)
# subnational regressions #### 
continents<-country.poststrat%>%
  select(iso_3166,Continent_Name)%>%
  distinct()
# countries<-d%>%
#   select(iso_3166,NAME_0_gadm)%>%
#   distinct()
# countries<-left_join(countries,continents,by="iso_3166")
regions.2020<-region.alphas%>%
  select(mean,year2,iso_3166,mergekey,NAME_0_gadm)%>%  
  filter(year2=="2020-21")%>%
  left_join(continents,by="iso_3166")
coaldata<-read_csv("predictors/coalmines_coaljobs_annex_3.csv")
nutscodes<-read_csv("basedata/NUTS2_to_ISO.csv")
nutscodes2<-nutscodes%>%
  pivot_longer(cols=regioncode:regioncode8,names_to="col.orig",values_to="mergekey")%>%
  filter(!is.na(mergekey))%>%
  select(mergekey,nuts2,nuts_name,country,iso_3166)
coaldata<-coaldata%>%
  left_join(nutscodes2,by=c("NUTS 2"="nuts2","NUTS Name"="nuts_name","Country"="country"))
regions.2020<-regions.2020%>%
  left_join(coaldata,by=c("mergekey","iso_3166"))
sort(unique(coaldata$Country))

EUcountries<-regions.2020%>%
  filter(Continent_Name=="Europe"|Continent_Name=="Eurasia")%>%
  filter(NAME_0_gadm%in%c("Albania","Andorra","Armenia","Azerbaijan","Belarus","Bosnia and Herzegovina",
                      "Georgia","Iceland","Kazakhstan","Moldova","Montenegro","North Macedonia","Norway","Russia",
                      "Serbia","Switzerland","Turkey","Ukraine")==FALSE)%>% ## check that none of these entered after our coal data were collected
## and check that the coal data doesn't include any non-EU countries
  select(NAME_0_gadm)%>%distinct()


regions.2020<-regions.2020%>%
  filter(NAME_0_gadm%in%c(EUcountries$NAME_0_gadm))%>%
  mutate(`Production (Mt)`=ifelse(is.na(`Production (Mt)`),0,`Production (Mt)`),
         # production.ln=log(`Production (Mt)`+.00001),
         production.bin=ifelse(`Production (Mt)`==0,0,1),
         `No. mines`=ifelse(!is.na(`No. mines`),`No. mines`,0),
         coalprod.bin=ifelse(`No. mines`>0,1,0))
regions.2020$mean.std=c(scale(regions.2020$mean))

coalreg<-lm_robust(mean.std~coalprod.bin,data=regions.2020) ## simple difference in means test


coalreg2<-lm_robust(mean.std~coalprod.bin,data=regions.2020,clusters=NAME_0_gadm) ## with clustered standard errors by country

texreg(coalreg,custom.coef.names=c("(Intercept)","Coal production (dichotomous)"),
       custom.model.names="Climate concern",
       include.ci=FALSE,label="tab:regression_subnat",
       caption="{ \\bf Effect of coal dependency on climate concern:} The table shows the results from an OLS regression of climate concern (standardized) on 
       coal production (dichotomous) in sub-national regions within the EU in 2020. The model was estimated using OLS regression with standard errors clustered by country. Regions without any coal production were assigned a value of 0.00001 on the coal-production variable prior to applying the log transformation.",
       file=paste0(paperfigfolder,"regression_results_subnational.tex"),float.pos="!htpb")


# coalplot<-ggplot(regions.2020,aes(x=coalprod.bin,y=mean.std))+
#   annotate("text",x=0.5,y=-1.5,label=paste("beta == ",round(coalreg$coefficients[2],3)),color="blue",parse=TRUE)+
#   annotate("text",x=0.5,y=-1.8,label=paste("p == ",round(coalreg$p.value[2],2)),color="blue",parse=TRUE)+
#   geom_jitter(width=0.05)+
#   geom_smooth(method="lm",se=FALSE,lty="dashed")+
#   theme_bw()+
#   scale_x_continuous(breaks=c(0,1))+
#   labs(x="Coal producing region (1=yes)",
#        y="Climate concern (st.dev)")
# 
# coalplot
# ggsave(file=paste(paperfigfolder,"scatter_coal_concern.pdf"),width=6.5,height=3.5,coalplot)
# ggsave(file=paste(figfolder,"scatter_coal_concern.pdf"),width=6.5,height=3.5,coalplot)

## average opinion, for in-text interpretation 
mean(regions.2020$mean.std[regions.2020$coalprod.bin==0]) ## 0.016
mean(regions.2020$mean.std[regions.2020$coalprod.bin==1]) ## -0.099
summary(lm(mean.std~coalprod.bin,regions.2020)) # t=-.710; beta=-.12

# subnational analysis with EDGAR data #### 

# load posterior distrubitions
load(paste0("analyzed_outputs/",modelname,".Rda"))
## convert to long-form to sample 
post.reg.20<-pivot_longer(post.reg,cols=-c(geog_int,year_int,param,mergekey,year2),names_to="iter",values_to="mean.post")%>%
  filter(year2=="2020-21")%>%
  select(mergekey,iter,mean.post)

# load EDGAR emissions data 
emissions<-readxl::read_excel("predictors/EDGAR_GHG_NUTS2_by_sector_1990_2021.xlsx",sheet=1)
emissions%<>%
  filter(sector=="ENE",
         grepl("FRY",NUTS_ID)==FALSE)%>% # filter to energy production sector 
  # select(-`2021`)%>%
  # pivot_longer(`1990`:`2020`,values_to="emissions",names_to="year")%>%
  select(NUTS_ID,CNTR_CODE,NAME_LATN,sector,`2020`)%>%
  group_by(NUTS_ID,CNTR_CODE,NAME_LATN)%>%
  summarise(emissions.cumul=sum(`2020`,na.rm=TRUE),
            emissions.cumul=emissions.cumul*1000) ## convert to tons rather than kilotons
crosswalk<-readxl::read_excel("predictors/EU_crosswalk.xlsx",sheet=1)
emissions<-left_join(emissions,crosswalk,by=c("NUTS_ID"="eucodes")) ## aggregate across EU regions with same code in our dataset
emissions<-emissions%>%
  group_by(CNTR_CODE,ourcodes)%>%
  summarise(emissions.cumul=sum(emissions.cumul))
pop<-read_csv("basedata/Covariates/region_covariates.csv")%>%
  select(mergekey,region_pop)
emissions<-left_join(emissions,pop,by=c("ourcodes"="mergekey"))%>%
  mutate(emissions.percap=emissions.cumul/region_pop)
# emissions$emissions.percap.bin<-cut(emissions$emissions.percap,breaks=c(-1,1,2,3,4,5,10,20,100),
#                                     labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","20+"))

# ggsave(file=paste(paperfigfolder,"scatter_europe_regs.pdf"),width=6.5,height=3.5,
#        ggarrange(coalplot,emissionsplot))
# ggsave(file=paste(figfolder,"scatter_europe_regs.pdf"),width=6.5,height=3.5,
#        ggarrange(coalplot,emissionsplot))
lm2.moc.all<-NA
for(p in 1:500){
  print(p)
  iter.sample<-sample(unique(post.reg.20$iter),1)
  print(iter.sample)
  post.reg2<-filter(post.reg.20,iter==iter.sample)
  emissions2<-left_join(emissions,post.reg2,by=c("ourcodes"="mergekey"))
  emissions2$mean.post.scl<-c(scale(emissions2$mean.post))
  lm2.moc<-tidy(lm_robust(mean.post.scl~log(emissions.percap),data=emissions2))#%>%
  # filter(term!="(Intercept)")
  lm2.moc<-lm2.moc%>%
    mutate(boot=p,
           estimate2=rnorm(n=dim(lm2.moc)[1]),mean=estimate,sd=std.error)
  lm2.moc.all<-rbind(lm2.moc.all,lm2.moc)
}
lm2.moc.all<-lm2.moc.all%>%
  filter(!is.na(term))
lm2.moc.all<-lm2.moc.all%>%
  group_by(term)%>%
  summarise(estimate=mean(estimate2),
            std.error=sd(estimate2),
            statistic=estimate/std.error)%>%
  ungroup()
## b=-.0648, t=-.0655


