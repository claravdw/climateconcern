# Preamble #### 
rm(list=ls())
library(reshape2)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(broom)
library(ggforce)
library(ggpubr)
library(corrplot)
# library(ggsflabel)
library(rmapshaper)
library(sp)
# devtools::install_github("ianmoran11/mmtable2")
# library(mmtable2)
library(xtable)
library(magrittr)
library(gt)
library(RColorBrewer)
library(colorspace)
library(nFactors)
library(estimatr)
library(readxl)
library(sf)
library(fixest)
library(grid) ## for function "unit" in maps
library(ggspatial) ## for north arrow function in maps
library(ggpattern) ## for hatching on deltas map
library(tidyverse)
# source("globalmrp_functions.R")

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}
if(onUser("pberg")){
  # setwd("~/Documents/GitHub/climateconcern")
  setwd("~/Library/CloudStorage/Dropbox/global_mrp") ## comment this out once data are publicly shareable
  repofolder<-"~/Documents/GitHub/climateconcern/"
  figfolder<-"~/Documents/GitHub/climateconcern/figures/"
}
## Clara set up your folders here
if(onUser("clara")){
  setwd("~/Documents/climateconcern")
  figfolder<-"~/Documents/climateconcern/figures/"
}

#load cleaned model outputs
modelname<-"climate_concern_v2_revision" ## new model 261002
timecollapse<-"2yr"
qrestrict<-"concernhuman"
datafilter<-paste(timecollapse,qrestrict,sep="_")


if(exists("datafilter")){
  load(paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
}else{
load(paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
}

## aggregated data objects from data that are not publicly shareable
load(paste0(repofolder,"inputs/surveys_nonpublic.Rda"))

## number of questions in our final model: 
length(unique(d$question)) #121
length(unique(d.nat$question)) # 107
## total sample
sum(d$yes) ## 4.5 million
sum(d.nat$yes) ## 1.1 million

qs<-data.frame(question=c(unique(d$question),unique(d.nat$question)))%>%distinct() 
nrow(qs) ##138 

## how many *respondents* are in the dataset. 
respondents<-survey.data%>%select(all_of(qs$question))%>%
  filter_all(any_vars(!is.na(.))) ## This will be an under-count in replication code because of non-shareable data; but add to nrow(constructed to get full count)
nrow(nonpublic.constructed) + nrow(respondents) ## 3.89 m
## number of countries and regions in input: 
length(unique(d$iso_3166)) ## 159
length(unique(d.nat$iso_3166)) ## 142
surveycountries<-d%>%select(iso_3166)%>%distinct()%>%full_join(d.nat%>%select(iso_3166)%>%distinct()) ## 169 rows
length(unique(d$mergekey[grepl(".nat",d$mergekey)==FALSE])) ## 2240


## number of countries where concern increased 
# increase in global climate concern 
est.nat.allyears<-est.nat ## preserve all years before narrowing est.nat below, for analyses elsewhere that need other periods (e.g. the 2012-13/2022-23 validation figure)
est.nat%<>%filter(year%in%c("2010-11","2024-25"))
est.reg%<>%filter(year%in%c("2010-11","2024-25"))

## load thinned posteriors for countries in 2010 and 2024
load(paste0(repofolder,"outputs_stan/draws_country_2010-11_full4000.Rda"))
draws_2010<-data.frame(draws_country)%>%
  mutate(draw=1:nrow(draws_country))
load(paste0(repofolder,"outputs_stan/draws_country_2024-25_full4000.Rda"))
draws_2024<-data.frame(draws_country)%>%
  mutate(draw=1:nrow(draws_country))

stopifnot(identical(draws_2010$draw, draws_2024$draw)) ## confirms the two files are paired by posterior sample
est.nat$q5.scl<-scales::rescale(est.nat$q5,to=c(0,1),from=global.range)
est.nat$q95.scl<-scales::rescale(est.nat$q95,to=c(0,1),from=global.range)
nat_isos<-unique(est.nat$iso_3166)
M2010<-as.matrix(draws_2010%>%select(draw,any_of(nat_isos))%>%select(-draw))
M2024<-as.matrix(draws_2024%>%select(draw,any_of(nat_isos))%>%select(-draw))

range_width<-diff(global.range)
Mdelta<-(M2024-M2010)/range_width

delta_ci<-tibble(
  iso_3166=colnames(Mdelta),
  delta.ci.low=apply(Mdelta,2,quantile,probs=.025),
  delta.ci.high=apply(Mdelta,2,quantile,probs=.975)
)%>%
  mutate(delta.sig=(delta.ci.low>0)|(delta.ci.high<0)) ## 95% CI excludes zero

## "robust" change: the ENTIRE 95% CI must fall outside a band of negligible
## change (-delta.threshold, +delta.threshold), not just exclude zero. This is
## stricter than simple zero-exclusion: a country can technically exclude zero
## while its whole CI still sits inside the negligible-change band (Slovenia
## here: delta=0.011, CI=[0.0002,0.022] -- "significant" only because its
## estimate happens to be very precise, not because the change is meaningful).
## delta.threshold is anchored to Cohen's conventional "small effect" benchmark
## (0.2 SD) applied to the cross-country variability in delta itself --
## an externally-recognized convention, not derived from estimation precision.
summ<-est.nat%>%
  select(iso_3166,NAME_0_gadm,mean.scl,q5.scl,q95.scl,year)%>%
  pivot_wider(names_from=year,values_from=c(mean.scl,q5.scl,q95.scl))%>%
  mutate(delta=`mean.scl_2024-25`-`mean.scl_2010-11`,
         increase=factor(ifelse(delta>0,1,0)))%>%
  left_join(delta_ci,by="iso_3166")

delta.threshold<-0.1*sd(summ$delta,na.rm=TRUE)

summ<-summ%>%
  mutate(real.change=case_when(
    delta.ci.low > delta.threshold ~ "increase (robust)",
    delta.ci.high < -delta.threshold ~ "decrease (robust)",
    TRUE ~ "not established"))

summ%>%group_by(real.change)%>%count() ## 24 decrease (robust), 46 increase (robust), 99 not established

summ_sig<-summ%>%filter(delta.sig)

# fig of top increasers and decreasers #### 
summtop<-summ_sig%>%
  arrange(desc(delta))%>%
  slice_head(n=10)
summbottom<-summ_sig%>%
  arrange(delta)%>%
  slice_head(n=10)
mypal<-brewer.pal(10,"RdBu")[c(3,8)]
change.topbottom<-rbind(summtop,summbottom)%>%  
  mutate(NAME_0_gadm=factor(NAME_0_gadm),
         NAME_0_gadm=fct_reorder(NAME_0_gadm,delta))%>%
  arrange(NAME_0_gadm)
changefig<-change.topbottom%>%
  ggplot()+
  geom_segment(aes(x=`q5.scl_2010-11`,xend=`q95.scl_2010-11`,y=NAME_0_gadm,yend=NAME_0_gadm),
               color="gray40",lwd=1.5,alpha=.5)+
  geom_segment(aes(x=`q5.scl_2024-25`,xend=`q95.scl_2024-25`,y=NAME_0_gadm,yend=NAME_0_gadm),
               color="gray40",lwd=1.5,alpha=0.5)+
  geom_segment(aes(x=`mean.scl_2010-11`,y=NAME_0_gadm,xend=`mean.scl_2024-25`,yend=NAME_0_gadm,color=increase),
               arrow=arrow(length=unit(0.2,"cm")))+
  geom_point(aes(x=`mean.scl_2010-11`,y=NAME_0_gadm,color=increase))+
  scale_color_manual(values=rev(mypal))+
  theme_bw()+
  labs(x="Climate concern",y="")+
  theme(legend.position="none")
changefig
ggsave(paste0(figfolder,"countrychanges_",modelname,".pdf"),width=3.5,height=6.5,changefig)

# maps #### 
## load country-level polygons PICK UP HERE, LOAD NEW REGION BOUNDARIES 
regions <- st_read(paste0(repofolder,"basedata/simplified geometries/model_regions_IR7_simplified.gpkg"))
# setdiff(est.reg$mergekey,regions$Group.1)
regions <- ms_simplify(regions)


# countries<-st_read("basedata/gadm_410-levels.gpkg",layer="ADM_0")
countries<-st_read(paste0(repofolder,"basedata/simplified geometries/gadm_410-level0_simplifiedgeometry.gpkg"))

setdiff(est.nat$NAME_0_gadm,countries$COUNTRY) ## rename these in countries shapefile so that names match those in est.2020
countries%<>%
  mutate(COUNTRY=case_when(GID_0=="MEX"~"Mexico",
                           GID_0=="COG"~"Republic of Congo",
                           GID_0=="CIV"~"Cote d'Ivoire",
                           GID_0=="CPV"~"Cape Verde",
                           GID_0=="CZE"~"Czech Republic",
                           GID_0=="PSE"~"Palestina",
                           GID_0=="STP"~"Sao Tome and Principe",
                           .default=COUNTRY))
est.nat%<>%mutate(NAME_0_gadm=ifelse(iso_3166=="ST","Sao Tome and Principe",NAME_0_gadm)) ## take out special characters in both shapefiles and estimates for this country 
setdiff(est.nat$NAME_0_gadm,countries$COUNTRY) 
countries<-ms_simplify(countries)
intersect(names(countries),names(est.nat)) ## check that column names are the same
countries<-left_join(countries,est.nat,by=c("COUNTRY"="NAME_0_gadm"))
intersect(names(regions),names(est.reg)) ## check that column names are the same
regions<-regions%>%left_join(est.reg,by=c("mergekey","iso_3166"))
countrychange<-left_join(countries,summ,by=c("COUNTRY"="NAME_0_gadm"))

## save outputs for website #### 
regionkey<-read_csv(paste0(repofolder,"individual polls/regioncodes/model_region_key.csv"))
intersect(names(regionkey),names(countries))
regionkey%<>%left_join(countries%>%select(year,mean.scl,iso_3166),
                       by="iso_3166")%>%
  rename(mean.scl.national=mean.scl)#%>%
# select(-geom)
regionkey%<>%left_join(regions%>%select(year,mean.scl,mergekey),by=c("mergekey","year"))%>%
  rename(mean.scl.regional=mean.scl)#%>%
# select(-geom)
regionkey%<>%
  # filter(!is.na(year))%>% ## filter out na values --but need to keep these so that the mappers have all the geographic units
  pivot_wider(names_from=year,values_from=c("mean.scl.regional","mean.scl.national"))%>%
  select(-mean.scl.national_NA,-mean.scl.regional_NA)

regionkey%<>%select(-geom.x,-geom.y)%>%distinct()

save(regionkey,file=paste0(repofolder,"analyzed_outputs/webdata/",modelname,"_formapping.Rda"))
write_csv(regionkey,file=paste0(repofolder,"analyzed_outputs/webdata/",modelname,".csv"))
rm(regionkey)

## colorblind-safe sequential palette for climate concern choropleths (mean.scl.bin) ####
## single-hue blue ramp, light=low concern -> dark=high concern
climate_concern_palette <- c(
  "0-0.1"   = "#cde2fb",
  "0.1-0.2" = "#9ec5f4",
  "0.2-0.3" = "#6da7ec",
  "0.3-0.4" = "#5598e7",
  "0.4-0.5" = "#3987e5",
  "0.5-0.6" = "#2a78d6",
  "0.6-0.7" = "#256abf",
  "0.7-0.8" = "#1c5cab",
  "0.8-0.9" = "#184f95",
  "0.9-1"   = "#0d366b"
)

## Fig. 1: world map ####
## world map ####
countryest<-countries%>%filter(!is.na(mean.std))
## save this file 
countrynoest1<-countries%>%filter(is.na(mean.std))%>%
  mutate(year="2010-11")
countrynoest2<-countries%>%filter(is.na(mean.std))%>%
  mutate(year="2024-25")
countrynoest<-rbind(countrynoest1,countrynoest2)
rm(list=c("countrynoest1","countrynoest2"))
# countryest<-as_Spatial(countryest)
# countries<-as_Spatial(countries)
#quartz(12,12)

## World map in equal area projection
countryest_proj <- st_transform(countryest, "+proj=moll")
countrynoest_proj <- st_transform(countrynoest, "+proj=moll")
countryest_proj%<>%
  left_join(summ%>%select(NAME_0_gadm,real.change),by=c("COUNTRY"="NAME_0_gadm"))

## drop countries whose raw WGS84 geometry spans the antimeridian as a single unsplit polygon
## (e.g. Russia, USA via the Aleutians, New Zealand, Antarctica) -- reprojecting these into any
## off-Pacific-centered CRS draws a spurious straight line across the map connecting their
## far-apart vertices. This layer is only a thin country-border accent, and none of these
## countries are relevant to the regional close-up maps that use it.
countries$lon_span<-sapply(seq_len(nrow(countries)), function(i){
  b<-st_bbox(countries[i,]); b["xmax"]-b["xmin"]
})
countries_clean<-countries%>%filter(lon_span<=180|is.na(lon_span))

countries_proj1<-st_transform(countries_clean, "+proj=moll")

## deltas 2010-2020
countrychange%<>%filter(!is.na(delta))
countrydelta_proj<-st_transform(countrychange,"+proj=moll")

worldplot<-countrynoest_proj%>%
  ggplot()+
  theme_bw()+
  geom_sf(aes(geometry=geom),fill="lightgray",lwd=.1,color="darkgray")+
  geom_sf(data=countryest_proj,aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  geom_sf_pattern(data=countryest_proj%>%filter(real.change%in%c("increase (robust)","decrease (robust)"),year=="2024-25"),
                  aes(geometry=geom),
                  pattern="stripe",pattern_fill="gray20",pattern_colour=NA,
                  pattern_density=0.2,pattern_spacing=0.01,pattern_angle=45,
                  fill=NA,color=NA)+
  scale_fill_viridis_d(option="viridis",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  labs(fill="Climate\nconcern")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf()+
  facet_wrap(~year,nrow=2,ncol=1)
worldplot
ggsave(file=paste0(figfolder,"map_projected_paired_",modelname,"_",datafilter,".png"),height=6.5,width=6.5,
       worldplot,device=ragg::agg_png,dpi=600)



## fig. S9: Deltas
worldplot_delta<-countrynoest_proj%>%
  ggplot()+
  theme_bw()+
  geom_sf(aes(geometry=geom),fill="lightgray",lwd=.1,color="darkgray")+
  geom_sf(data=countrydelta_proj,aes(geometry=geom,fill=delta),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  geom_sf_pattern(data=countrydelta_proj%>%filter(real.change%in%c("increase (robust)","decrease (robust)")),
                  aes(geometry=geom),
                  pattern="stripe",pattern_fill="gray20",pattern_colour=NA,
                  pattern_density=0.1,pattern_spacing=0.01,pattern_angle=45,
                  fill=NA,color=NA)+
  scale_fill_distiller(palette="RdBu")+
  labs(fill="Change in \nclimate\nconcern")+
  theme_classic()+
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),
        panel.grid=element_blank(),axis.line=element_blank())+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),style=north_arrow_orienteering(text_size=4))+
  coord_sf()
worldplot_delta
ggsave(file=paste0(figfolder,"map_delta_",modelname,"_",datafilter,".png"),height=6.5,width=6.5,
       worldplot_delta,device=ragg::agg_png,dpi=600)

## subnational maps of Europe #### 
# regions[is.na(regions$year)==TRUE,]$year <- "2024-25" ## nothing here now 
europe <- st_transform(regions, "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs ")
# countries_proj <- st_transform(countrynoest, "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs ")
countries_proj<-st_transform(countries_proj1, "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs ")
europe2024 <- europe[europe$year=="2024-25",]
#quartz(12,12)

europlot<-europe2024%>%
  ggplot()+
  theme_bw()+
 # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),
                    na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  # geom_sf(data=europecountries,aes(geometry=geom),lwd=.2,color="darkgray",fill=NA)+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf()+
  coord_sf(xlim = c(-2000000, 2000000), ylim = c(0, 5000000))
#  facet_wrap(~year)
europlot


## subnational maps of east asia #### 
eastasia <- st_transform(regions, "+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
eastasia <- eastasia[eastasia$year=="2024-25",]
#quartz(12,12)

eastasiaplot<-eastasia%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  labs(fill="Climate\nconcern")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf(xlim = c(-2500000, 3500000), ylim = c(-3000000, 4000000))
#  facet_wrap(~year)
eastasiaplot
ggsave(file=paste0(figfolder,"map_concern_eastasia_",modelname,".pdf"),width=6.5,height=6.5,eastasiaplot)



## Fig. S12: subnational maps of south asia and middle east #### 
southasia <- st_transform(regions, "+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
southasia <- southasia[southasia$year=="2024-25",]
#quartz(12,12)

southasiaplot<-southasia%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf(xlim = c(-2500000, 4000000), ylim = c(-0000000, 5000000))
#  facet_wrap(~year)
southasiaplot
ggsave(file=paste0(figfolder,"map_concern_southasia_",modelname,".pdf"),width=6.5,height=6.5,southasiaplot)


## Figure 3: combined figure with south asia and europe/ eurasia #### 
## south asia currently has width=6,500,000, height=5,000,000 (ratio 1.3)
## to hit europe's ratio (0.8), needed height = 6,500,000/0.8 = 8,125,000
## so extend ylim by (8,125,000-5,000,000)/2 = 1,562,500 on each side
southasiaplot <- southasiaplot +
  coord_sf(xlim = c(-2500000, 4000000), ylim = c(-1562500, 6562500))
europlot <- europlot +
  coord_sf(xlim = c(-3250000, 3250000), ylim = c(0, 5000000))
ggsave(file=paste0(figfolder,"map_concern_europe_southasia_",modelname,".pdf"),width=6.5,height=9,
       ggarrange(europlot,southasiaplot,nrow=2,ncol=1,common.legend=TRUE,legend="right"))
  

## Fig. S13: subnational maps of Africa #### 
# regions[is.na(regions$Continent_Name)==TRUE,]$Continent_Name <- "Missing"
africa <- st_transform(regions, "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
africa<-africa%>%filter(Continent_Name %in% c("Africa","Europe","Asia","Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
africa<-africa%>%filter(year=="2024-25")
#quartz(12,12)

africaplot<-africa%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+ ## this line is now creating lines across the map. why? 
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-4500000, 3500000), ylim = c(-4000000, 4000000))
#  facet_wrap(~year)
africaplot
ggsave(file=paste0(figfolder,"map_concern_africa_",modelname,".pdf"),width=6.5,height=6.5,africaplot)

## Fig. S11: subnational maps of South America #### 
southamerica <- st_transform(regions, "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs ")
southamerica<-southamerica%>%filter(Continent_Name %in% c("South America","North America","Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs ")
southamerica <- southamerica[southamerica$year=="2024-25",]
#quartz(12,12)

southamericaplot<-southamerica%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-3000000, 3000000), ylim = c(-2500000, 5000000))
#  facet_wrap(~year)
southamericaplot
ggsave(file=paste0(figfolder,"map_concern_southamerica_",modelname,".pdf"),width=6.5,height=6.5,southamericaplot)

## subnational maps of North America & Caribbean #### 
northamerica <- st_transform(regions, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
northamerica<-northamerica%>%filter(Continent_Name %in% c("South America","North America","Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
northamerica <- northamerica[northamerica$year=="2024-25",]
#quartz(12,12)

northamericaplot<-northamerica%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-3500000, 3000000), ylim = c(-3500000, 3500000))
#  facet_wrap(~year)
northamericaplot
ggsave(file=paste0(figfolder,"map_concern_northamerica_",modelname,".pdf"),width=6.5,height=6.5,northamericaplot)

## subnational maps of Oceania #### 
oceania <- st_transform(regions, "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs  ")
oceania<-oceania%>%filter(Continent_Name %in% c("Asia","Oceania","Eurasia", "Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
oceania <- oceania[oceania$year=="2024-25",]
#quartz(12,12)

oceaniaplot<-oceania%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_viridis_d(direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-2500000, 5000000), ylim = c(-5500000, 500000))
#  facet_wrap(~year)
oceaniaplot
ggsave(file=paste0(figfolder,"map_concern_oceania_",modelname,".pdf"),width=4.5,height=6.5,oceaniaplot)


# Figure S16: countries with biggest increases and decreases #### 

# tables of regional estimates #### 
top20<-data.frame(regions)%>%
  filter(year=="2024-25")%>%
  select(region_name,mergekey,NAME_0_gadm,Continent_Name,mean,q95,q5)%>%
  distinct()%>%
  filter(!is.na(mean))%>%
  arrange(mean)%>%
  slice_tail(n=20)%>%
  arrange(region_name)

bottom20<-data.frame(regions)%>%
  filter(year=="2024-25")%>%
  select(region_name,mergekey,NAME_0_gadm,Continent_Name,mean,q95,q5)%>%
  distinct()%>%
  filter(!is.na(mean))%>%
  arrange(mean)%>%
  slice_head(n=20)%>%
  arrange(region_name)
ciplot.world<-rbind(top20,bottom20)%>%
  mutate(fullname=paste(region_name,NAME_0_gadm,sep=", "),
         fullname=fct_reorder(fullname,mean))%>%
  ggplot(aes(x=mean,y=fullname,color=Continent_Name))+
  geom_point()+
  geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y="Region",x="Climate concern",color="Continent")+
  theme_bw()
ciplot.world  
ggsave(file=paste0(figfolder,"ciplot_worldregions_",modelname,".png"),width=6.5,height=6.5,ciplot.world) ## have to save these as png files or come up with a way for the pdf output to correctly save non-English characters

# Fig. S17: highest and lowest climate concerned regions by continent #### 
top5<-data.frame(regions)%>%
  filter(year=="2024-25")%>%
  select(region_name,mergekey,NAME_0_gadm,Continent_Name,mean,q95,q5)%>%
  distinct()%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(mean))%>%
  group_by(Continent_Name)%>%
  arrange(mean)%>%
  slice_tail(n=5)%>%
  ungroup()%>%
  select(-mergekey)
bottom5<-data.frame(regions)%>%
  filter(year=="2024-25")%>%
  select(region_name,mergekey,NAME_0_gadm,Continent_Name,mean,q95,q5)%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(mean))%>%
  group_by(Continent_Name)%>%
  arrange(mean)%>%
  slice_head(n=5)%>%
  ungroup()%>%
  select(-mergekey)
top.bottom5fig<-rbind(top5,bottom5)%>%
  mutate(fullname=paste(region_name,NAME_0_gadm,sep=",\n"),
         fullname=fct_reorder(fullname,mean))%>%
  ggplot(aes(x=mean,y=fullname))+
  geom_pointrange(aes(y=fullname,xmin=q5,xmax=q95),size=.1)+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y="Region",x="Climate concern",color="Continent")+
  theme_bw()+
  guides(color="none")+
  facet_wrap(~Continent_Name,scales="free_y")
top.bottom5fig
ggsave(file=paste0(figfolder,"ciplot_continentregions_",modelname,".png"),width=6.5,height=6.5,top.bottom5fig)


# Fig. S15: ci plot with 2024-25 values ####
## rearrange by order of climate concern
est.2024<-est.nat%>%filter(year=="2024-25")
est.2024$NAME_0_gadm<-reorder(est.2024$NAME_0_gadm,est.2024$mean)
est.2024<-est.2024%>%
  arrange(NAME_0_gadm)
est.2024$group<-c(rep(1,round(nrow(est.2024)/2,0)),
                  rep(2,nrow(est.2024)-round(nrow(est.2024)/2,0)))
head(levels(est.2024$NAME_0_gadm))

## plot
#quartz(12,12)
estplot<-ggplot(est.2024,aes(x=mean,y=NAME_0_gadm,color=Continent_Name))+
  geom_point()+
  geom_linerange(aes(y=NAME_0_gadm,xmin=q5,xmax=q95))+
  facet_wrap(~group,scales="free_y",strip.position="top")+
  labs(y="",x="Climate concern index",color="Continent")+
  theme_bw()+
  theme(strip.background=element_blank(),strip.text=element_blank(),
        text=element_text(size=8))
estplot

if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"estimates_",modelname,"_",datafilter,".pdf"),
         width=6.5,height=9,units="in",estplot)
}else{
  ggsave(file=paste0(figfolder,"estimates_",modelname,"_",".pdf"),
       width=6.5,height=9,units="in",estplot)
}
# Fig. S8:  barplot with discrimination parameters ####
## separate out category
discrimination2<-discrimination%>%
  mutate(question=ifelse(question=="attribution2_eurobarometer","attribution_eurobarometer2",question), ## this should be fixed in next merge 
         question=ifelse(question=="consensus_attribution_activity_bcb","attribution_consensus_bcb",question), ## should also be fixed in next merge
         question2=question)%>%
  separate(question,c("Item category","question_short"),extra="merge",sep="_")%>%
  mutate(question2=fct_reorder(question2,mean))
## quantiles: 
quantile(discrimination2$mean)

discplot<-ggplot(discrimination2,aes(x=mean,y=question2))+
  geom_bar(stat="identity",aes(fill=`Item category`))+
  # geom_pointrange(aes(y=question2,x=mean,xmin=q5,xmax=q95,color=`Item category`))+
  labs(x="Discrimination parameter",y="Survey item")+
  theme_classic()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(text=element_text(size=8))
discplot
if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"discrimination_all_",datafilter,"_",modelname,".pdf"),width=6.5,height=9)
}else{
ggsave(file=paste0(figfolder,"discrimination_all.pdf"),width=6.5,height=9)
}

# Figure S1: time coverage across countries by continent #### 
dsum<-d%>%
  select(-c(regioncode_int,size,yes))%>%
  distinct()%>%
  group_by(year2,iso_3166,Continent_Name)%>%tally()
# summarise(qs=count(question_int))
head(dsum) 
range(dsum$n)

# #quartz(18,18)

questions.plot<-ggplot(dsum,aes(x=year2,y=iso_3166,alpha=n,color=Continent_Name))+
  geom_point()+
  facet_wrap(~Continent_Name,nrow=7,ncol=1,scales="free_y")+
  theme_classic()+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none")+
  labs(y="",x="")

questions.plot
if(exists("datafilter")){
  ggsave(filename=paste0(figfolder,"questions_plot_",datafilter,".pdf"),width=8,height=10,plot=questions.plot)
}else{
ggsave(filename=paste0(figfolder,"questions_plot.pdf"),width=8,height=10,plot=questions.plot)
}

# Fig. S19: country and region-level trends with CIs ####
country.est<-country.poststrat%>%select(iso_3166,year2,mean,q5,q95)## year2? 220813
countrynames<-country.poststrat%>%select(iso_3166,NAME_0_gadm)%>%distinct()%>%
  mutate(NAME_0_gadm=ifelse(iso_3166=="ST","Sao Tome and Principe",NAME_0_gadm))
country.est<-left_join(country.est,countrynames,by="iso_3166")
##country alphas; one graph per country #### 
country.alphas.fig<-
  ggplot(country.est, aes(year2, mean, group = 1)) +
  geom_point() +
  geom_line() +
  geom_ribbon(data=country.est, aes(ymin=q5,ymax=q95, alpha=0.3)) +  ## this is 90% CIs
  facet_wrap(~ NAME_0_gadm) +
  theme(legend.position = "none")+
  scale_x_discrete(limits=sort(unique(country.est$year2)),breaks=c("2000-01","2005-06","2010-11","2015-16","2020-21","2022-23","2024-25"))+
  theme(axis.text.x=element_text(angle=45))
pdf(paste0(figfolder,"countrytrends-",modelname,"_",".pdf"), 15, 20)
country.alphas.fig
dev.off()


# Fig. 4: Climate concern and vulnerability #### 
## merge in exposure data, to analyze change in quadrants
d<-read_csv(paste0(repofolder,"predictors/ND-GAIN2026_exposure.csv"))
est.nat<-left_join(est.nat,d%>%select(-NAME_0_gadm,-GID_0_gadm),by=c("iso_3166"))
est.nat$exp.std<-scale(est.nat$exposure,center=TRUE,scale=TRUE)

reg1<-lm_robust(mean.std~exp.std,data=subset(est.nat,year=="2024-25"))
## capture fit statistics directly from reg1 (lm_robust doesn't populate residuals()/sigma(),
## so use summary()$r.squared/$adj.r.squared and res_var directly instead)
reg1_stats_uncorrected<-c(
  N=nobs(reg1),
  `R-squared`=summary(reg1)$r.squared,
  `Adj. R-squared`=summary(reg1)$adj.r.squared,
  RMSE=sqrt(reg1$res_var)
)
## error-corrected. 
load(paste0(repofolder,"outputs_stan/draws_country_2024-25.Rda"))
est.nat24<-est.nat%>%filter(year=="2024-25")%>%select(iso_3166,exp.std,mean)
nat_isos<-unique(est.nat24$iso_3166)

draws_country_nat<-draws_country%>%
  select(draw, any_of(nat_isos))

## quick check worth running once: any national iso_3166 codes missing from draws_country?
## setdiff(nat_isos, names(draws_country))

draw_ids<-draws_country_nat$draw
M<-as.matrix(draws_country_nat%>%select(-draw))
rownames(M)<-draw_ids

moc_draws_exposure<-map_dfr(seq_len(nrow(M)), function(i){
  est.nat24$mean.post<-M[i, est.nat24$iso_3166]
  est.nat24$mean.std<-as.numeric(scale(est.nat24$mean.post))
  
  mod<-lm_robust(mean.std~exp.std,data=est.nat24)
  tidy(mod)%>%
    mutate(draw=draw_ids[i],
           composed=rnorm(n(),mean=estimate,sd=std.error),
           r2=summary(mod)$r.squared,
           ar2=summary(mod)$adj.r.squared,
           rmse=sqrt(mod$res_var))
})

reg1.moc<-moc_draws_exposure%>%
  group_by(term)%>%
  summarise(estimate=mean(composed),
            std.error=sd(composed),
            ci.low=quantile(composed,.025),
            ci.high=quantile(composed,.975),
            statistic=estimate/std.error,
            p_below_zero=mean(composed<0),
            p_empirical=2*min(mean(composed<0),mean(composed>0)),
            mean_r2=mean(r2),
            mean_ar2=mean(ar2),
            mean_rmse=mean(rmse))%>%
  ungroup()


## combine uncorrected + error-corrected exposure results, append fit statistics, export as a table ####
## function: reshape a long coefficient table + a long fit-stats table into a
## standard side-by-side regression table (one 4-column block per model:
## Estimate, SE, t, p), with fit statistics as rows below the coefficients.
make_regression_table<-function(coef_long, stats_long, model_levels=c("Uncorrected","Error corrected")){
  metric_levels<-c("estimate","std.error","statistic","p.value")
  all_cols<-paste(rep(model_levels,each=4),metric_levels,sep="__")
  
  coef_wide<-coef_long%>%
    mutate(mod=factor(mod,levels=model_levels))%>%
    pivot_wider(id_cols=term,names_from=mod,
                values_from=c(estimate,std.error,statistic,p.value),
                names_glue="{mod}__{.value}")%>%
    select(term,all_of(all_cols))
  
  stats_wide<-stats_long%>%
    mutate(mod=factor(mod,levels=model_levels))%>%
    pivot_wider(id_cols=term,names_from=mod,values_from=estimate,
                names_glue="{mod}__estimate")
  missing_cols<-setdiff(all_cols,names(stats_wide))
  stats_wide[missing_cols]<-NA_real_
  stats_wide<-stats_wide%>%select(term,all_of(all_cols))
  
  list(table=bind_rows(coef_wide,stats_wide), n_coef=nrow(coef_wide))
}

## export: builds the two-level LaTeX header and writes the file
export_regression_table<-function(tbl_list, label, caption, file){
  tex_names<-c("Term","Estimate","SE","t","p","Estimate","SE","t","p")
  addtorow<-list()
  addtorow$pos<-list(0,0)
  addtorow$command<-c(
    "\\hline\n & \\multicolumn{4}{c}{Uncorrected} & \\multicolumn{4}{c}{Error corrected} \\\\\n",
    paste0(paste(tex_names,collapse=" & ")," \\\\\n\\hline\n")
  )
  xt<-xtable(tbl_list$table,digits=3,label=label,caption=caption)
  print.xtable(xt, add.to.row=addtorow, include.colnames=FALSE, include.rownames=FALSE,
               caption.placement="bottom",
               hline.after=c(tbl_list$n_coef,nrow(tbl_list$table)),
               NA.string="", file=file)
}

lm1<-tidy(reg1)%>%
  mutate(mod="Uncorrected")%>%
  select(term,estimate,std.error,statistic,p.value,mod)

lm1.moc.all<-reg1.moc%>%
  mutate(mod="Error corrected")%>%
  rename(p.value=p_empirical)%>%
  select(term,estimate,std.error,statistic,p.value,mod)

## fit-statistic rows: N is identical across draws (same sample every time, only the
## outcome changes); R2/Adj R2/RMSE differ draw to draw, so "Error corrected" uses
## their mean across the posterior draws
reg1_stats<-tibble(
  term=c("N","R-squared","Adj. R-squared","RMSE"),
  Uncorrected=unname(reg1_stats_uncorrected),
  `Error corrected`=c(reg1_stats_uncorrected[["N"]],
                      mean(moc_draws_exposure$r2),
                      mean(moc_draws_exposure$ar2),
                      mean(moc_draws_exposure$rmse))
)%>%
  pivot_longer(cols=c(Uncorrected,`Error corrected`),names_to="mod",values_to="estimate")

coef_long1<-bind_rows(lm1,lm1.moc.all)%>%
  filter(term!="(Intercept)")%>%
  mutate(term=case_when(term=="exp.std"~"Exposure (st.dev.)",TRUE~term))

tbl1<-make_regression_table(coef_long1, reg1_stats)
## table S6: climate risk exposure and climate concern 

export_regression_table(tbl1,
                        label="tab:regression.exposure.concern",
                        caption="{\\bf Effect of climate risk exposure on climate concern:} OLS regression of climate concern (standardized, 2024-25) on climate risk exposure (standardized), at the national level, with heteroskedasticity-robust standard errors. ``Uncorrected'' p-values are conventional; ``Error corrected'' p-values are empirical (simulation-based, two-sided), from the Method of Composition over the posterior distribution (thinned to 1,000 draws) of national climate concern estimates. R-squared, adjusted R-squared, and RMSE for the error-corrected model are averaged across posterior draws.",
                        file=paste0(figfolder,"regression_results_simple.tex"))



est.nat24<-est.nat%>%filter(year=="2024-25")
est.nat24$mean_2024.stdwithin<-scale(est.nat24$mean,center=TRUE,scale=TRUE)
countrylabs<-est.nat24%>%
  filter((mean_2024.stdwithin>quantile(est.nat24$mean_2024.stdwithin,na.rm=TRUE,probs=0.75)&exp.std<quantile(est.nat24$exp.std,na.rm=TRUE,prob=.25))|## high concern, low exposure
           (mean_2024.stdwithin<quantile(est.nat24$mean_2024.stdwithin,na.rm=TRUE,probs=.25)&exp.std>quantile(est.nat24$exp.std,na.rm=TRUE,prob=.75))| # low concern, high exposure
           (mean_2024.stdwithin<quantile(est.nat24$mean_2024.stdwithin,na.rm=TRUE,probs=.25)&exp.std<quantile(est.nat24$exp.std,na.rm=TRUE,prob=.25))| # low concern, exposure concern
           (mean_2024.stdwithin>quantile(est.nat24$mean_2024.stdwithin,na.rm=TRUE,probs=.75)&exp.std>quantile(est.nat24$exp.std,na.rm=TRUE,prob=.75))| # high concern, high exposure
           NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany"))%>%
  mutate(type=ifelse(NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany"),"emitter","outlier"))
countrylabs%<>%mutate(NAME_0_gadm=ifelse(iso_3166=="CD","DRC",NAME_0_gadm))

quadrants<-est.nat24%>%
  ggplot(aes(x=exp.std,y=mean_2024.stdwithin,label=iso_3166))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+
  geom_smooth(method="lm",se=FALSE,lty="dashed",lwd=.5)+
  geom_text(data=subset(est.nat24,iso_3166%in%countrylabs$iso_3166==FALSE),inherit.aes=TRUE,color="lightgray",size=1.5)+
  geom_point(data=countrylabs,inherit.aes =TRUE)+
  geom_text_repel(data=countrylabs,
                  aes(x=exp.std,y=mean_2024.stdwithin,label=NAME_0_gadm,color=type),
                  nudge_x=0.2,min.segment.length=.3)+
  scale_color_manual(values=c("emitter"="orange","outlier"="black"))+
  guides(color="none")+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.ticks=element_blank())+
  annotate(geom="text",x=2.5,y=-3,label="High exposure,\nlow concern",color="blue")+
  annotate(geom="text",x=2.5,y=3,label="High exposure,\nhigh concern",color="blue")+
  annotate(geom="text",x=-1.8,y=-3,label="Low exposure,\nlow concern",color="blue")+
  annotate(geom="text",x=-1.8,y=3,label="Low exposure,\nhigh concern",color="blue")+ 
  annotate("text",x=3,y=.5,label=paste("beta == ",round(reg1.moc[2,2],2)),color="blue",parse=TRUE)+
  annotate("text",x=3,y=.3,label=paste("p == ",round(reg1.moc[2,"p_empirical"],2)),color="blue",parse=TRUE)+
  labs(x="Climate exposure (st.dev)",y="Climate concern (st.dev)")
quadrants
ggsave(file=paste0(figfolder,"quadrants_exposure_concern_",modelname,".pdf"),width=6.5,height=6.5,quadrants)

# Figure 6: sub-national emissions regression, scatterplot #### 
emissions <- read_csv(paste0(repofolder, "predictors/emissions_eu_regions_2025edgar.csv"))
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
europe <- regions %>% st_drop_geometry() %>% filter(year == "2024-25", iso_3166 %in% eu27) %>%
  inner_join(emissions %>% select(mergekey, log_pc_2021, log_pc_2024), by = "mergekey") %>%
  filter(is.finite(log_pc_2021))
europe$mean.std <- scale(europe$mean)
# europe$emissions.log.std <- scale(europe$log_pc_2024) 

reg2<-feols(mean.std~log_pc_2024|iso_3166,vcov=~iso_3166,data=europe)

## capture fit statistics BEFORE reg2 is overwritten into a coeftable matrix below
reg2_stats_uncorrected<-c(
  N=fitstat(reg2,"n")[[1]],
  `N clusters`=reg2$fixef_sizes[["iso_3166"]], ## clusters = countries remaining after singleton-FE drop
  `R-squared`=fitstat(reg2,"r2")[[1]],
  `Adj. R-squared`=fitstat(reg2,"ar2")[[1]],
  RMSE=fitstat(reg2,"rmse")[[1]]
)

summary(reg2)
reg2<-coeftable(reg2)
rownames(reg2)[1]<-"Emissions (logged)"



## error-corrected regressions over the thinned posterior #### 
load(paste0(repofolder,"outputs_stan/draws_region_2024-25.Rda"))
## error-corrected regression via Method of Composition: EU emissions regression ####
## (region-level outcome with country fixed effects -- mirrors reg2's specification)
## For each posterior draw of regional climate concern:
##   1) take that draw's region-level values, restricted to EU regions already
##      used in the emissions analysis (the `europe` object built above)
##   2) standardize across those EU regions within the draw
##   3) fit the regression with country FEs + country-clustered SEs, matching reg2
##   4) draw ONE composed value from N(estimate, SE) for the emissions coefficient --
##      the actual MOC step, combining posterior uncertainty in the latent region
##      mean with the regression's own sampling uncertainty
## Pooling composed values across the full set of posterior draws (not a subsample)
## gives a sample from the coefficient's proper marginal distribution.

eu_mergekeys<-unique(europe$mergekey)
draws_region_eu<-draws_region%>%
  select(draw, any_of(eu_mergekeys))

## setdiff(eu_mergekeys, names(draws_region)) ## should be empty

## build a draws x mergekey matrix for fast row-wise indexing (row = one draw)
draw_ids<-draws_region_eu$draw
M<-as.matrix(draws_region_eu%>%select(-draw))
rownames(M)<-draw_ids

## structural covariates don't vary by draw, so build this once outside the loop
europe.2<-europe%>%
  st_drop_geometry()%>%
  select(mergekey,iso_3166,log_pc_2024)

moc_draws_emissions<-map_dfr(seq_len(nrow(M)), function(i){
  europe.2$mean.post<-M[i, europe.2$mergekey]
  europe.2$mean.std<-as.numeric(scale(europe.2$mean.post))
  
  mod<-feols(mean.std~log_pc_2024|iso_3166,vcov=~iso_3166,data=europe.2)
  tidy(mod)%>%
    mutate(draw=draw_ids[i],
           composed=rnorm(n(),mean=estimate,sd=std.error),
           r2=fitstat(mod,"r2")[[1]],
           ar2=fitstat(mod,"ar2")[[1]],
           rmse=fitstat(mod,"rmse")[[1]])
})

reg2.moc<-moc_draws_emissions%>%
  group_by(term)%>%
  summarise(estimate=mean(composed),
            std.error=sd(composed),
            ci.low=quantile(composed,.025),
            ci.high=quantile(composed,.975),
            statistic=estimate/std.error,
            p_below_zero=mean(composed<0),
            p_empirical=2*min(mean(composed<0),mean(composed>0)),
            mean_r2=mean(r2),
            mean_ar2=mean(ar2),
            mean_rmse=mean(rmse))%>%
  ungroup()

## combine uncorrected + error-corrected emissions results, append fit statistics, export as a table ####
lm2<-tibble(term="log_pc_2024",
            estimate=reg2[1,"Estimate"],
            std.error=reg2[1,"Std. Error"],
            statistic=reg2[1,"t value"],
            p.value=reg2[1,"Pr(>|t|)"],
            mod="Uncorrected")

lm2.moc<-reg2.moc%>%
  mutate(mod="Error corrected")%>%
  rename(p.value=p_empirical)%>%
  select(term,estimate,std.error,statistic,p.value,mod)

## fit-statistic rows: N and N clusters are identical across draws (same sample/FE
## structure every time, only the outcome changes); R2/Adj R2/RMSE differ draw to
## draw, so "Error corrected" uses their mean across the posterior draws
reg2_stats<-tibble(
  term=c("N","N clusters","R-squared","Adj. R-squared","RMSE"),
  Uncorrected=unname(reg2_stats_uncorrected),
  `Error corrected`=c(reg2_stats_uncorrected[["N"]],
                      reg2_stats_uncorrected[["N clusters"]],
                      mean(moc_draws_emissions$r2),
                      mean(moc_draws_emissions$ar2),
                      mean(moc_draws_emissions$rmse))
)%>%
  pivot_longer(cols=c(Uncorrected,`Error corrected`),names_to="mod",values_to="estimate")

coef_long2<-bind_rows(lm2,lm2.moc)%>%
  mutate(term=case_when(term=="log_pc_2024"~"Emissions (logged)",TRUE~term))

tbl2<-make_regression_table(coef_long2, reg2_stats)
## table S7: energy sector emissions and climate concern

export_regression_table(tbl2,
                        label="tab:regression.emissions.concern",
                        caption="{\\bf Effect of energy-sector CO$_2$ emissions on climate concern:} OLS regression of climate concern (standardized, 2024-25) on logged per-capita energy-sector emissions (2024), 
                        EU sub-national regions, with country fixed effects and SEs clustered by country. ``Uncorrected'' p-values are conventional; ``Error corrected'' p-values are empirical (simulation-based, two-sided), 
                        from the Method of Composition over the posterior distribution (thinned to 1,000 draws) of regional climate concern estimates. R-squared, adjusted R-squared, and RMSE for the error-corrected model are averaged across posterior draws.",
                        file=paste0(figfolder,"regression_results_emissions.tex"))

## plot results, demeaning by country FEs 
europe%<>%
  group_by(iso_3166)%>%
  mutate(mean.std.within = mean.std - mean(mean.std, na.rm=TRUE),
         # emissions.log.std.within = emissions.log.std - mean(emissions.log.std, na.rm=TRUE))%>%
         emissions.log.within=log_pc_2024-mean(log_pc_2024,na.rm=TRUE))%>%
  ungroup()

emissionsplot_fe<-ggplot(data=europe,aes(x=emissions.log.within,y=mean.std.within))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,lty="dashed")+
  annotate("text",x=-3,y=1.2,label=paste("beta == ",round(reg2.moc[1,2],2)),
           color="blue",parse=TRUE)+
  annotate("text",x=-3,y=1.0,label=paste("p == ",round(reg2.moc[1,"p_empirical"],2)),
           color="blue",parse=TRUE)+
  labs(x="Energy-sector carbon dioxide emissions\n(regional deviation from within-country mean of regional logged emissions)",
       y="Climate concern\n(standardized, deviation from within-country mean)")+
  theme_bw()
emissionsplot_fe
ggsave(file=paste0(figfolder,"scatter_emissions_concern_europeregions_withinFE_",modelname,".pdf"),width=6.5,height=3.5,emissionsplot_fe)


# Fig. S3: dimensionality with country-year groups: only questions with 20+ countries #### 
## average each question within group-biennia, 
## center these averages within year (to eliminate time effects), 
## and then average across biennia within groups.

## load survey data and collapse time periods (have to do this anew here b/c survey.data saved in data input only contains questions used...no action or awareness questions) #### 
load("inputs/megapoll_globalmrp_ordinal_v2_IR8.Rda")
survey.data%<>%filter(as.numeric(year)>=2002)

## pasting in from rstan_prepdata_region.R
## identify sources that span years--assign the last year to the ones that span years, and then merge back in
## take year off so that I can id unique sources that are duplicated after doing that. 
survey.data<-survey.data%>%
  mutate(source2=substr(source,1,nchar(source)-5)) 

multi.sources<-survey.data%>%
  select(source,year)%>%
  distinct()%>% ## keep distinct rows for each source-year
  mutate(source2=substr(source,1,nchar(source)-5))%>% ## take year off source name
  mutate(dup=grepl("[0-9]+",source2))%>% ### identify sources that still have numbers in the name but exclude eurobarometers b/c they're all single-year, single-wave. 
  filter(dup==TRUE,
         grepl("eurobarometer",source2)==FALSE)%>%
  group_by(source2)%>%
  summarise(year2=max(year))%>%
  ungroup()

## identify number of years in which each question is asked

survey.data<-left_join(survey.data,multi.sources,by="source2") # sources that are multiwave now have a value in "year2" and "source2" columns
unique(survey.data$year)
survey.data<-survey.data%>%
  mutate(year2=ifelse(is.na(year2),year,year2),
         source2=ifelse(is.na(source2),source,source2))
survey.data<-survey.data%>%mutate(
  year2=case_when(#as.numeric(year2)<=1999~"1998-99",
    #as.numeric(year2)>1999&as.numeric(year2)<=2001~"2000-01",
    as.numeric(year2)>2001&as.numeric(year2)<=2003~"2002-03",
    # as.numeric(year2)<=2003~"1998-2003",
    as.numeric(year2)>2003&as.numeric(year2)<=2005~"2004-05",
    as.numeric(year2)>2005&as.numeric(year2)<=2007~"2006-07",
    as.numeric(year2)>2007&as.numeric(year2)<=2009~"2008-09",
    as.numeric(year2)>2009&as.numeric(year2)<=2011~"2010-11",
    as.numeric(year2)>2011&as.numeric(year2)<=2013~"2012-13",
    as.numeric(year2)>2013&as.numeric(year2)<=2015~"2014-15",
    as.numeric(year2)>2015&as.numeric(year2)<=2017~"2016-17",
    as.numeric(year2)>2017&as.numeric(year2)<=2019~"2018-19",
    as.numeric(year2)>2019&as.numeric(year2)<=2021~"2020-21",
    as.numeric(year2)>2021&as.numeric(year2)<=2023~"2022-23",
    as.numeric(year2)>2024&as.numeric(year2)<=2026~"2024-25")
)
## end pasted part from rstan_prepdata_region.R

sort(names(survey.data))
names(survey.data)<-gsub("crisis_strength_scientific_consensus","attribution_consensus_strength",names(survey.data)) ## hopefully this will be renamed in future versions. check using line above 
names(survey.data)<-gsub("concern","worry",names(survey.data))
names(survey.data)<-gsub("worry_worry","worry",names(survey.data))
names(survey.data)<-gsub("human_caused","attribution",names(survey.data))
names(survey.data)<-gsub("human_cause","attribution",names(survey.data))
names(survey.data)<-gsub("human","attribution",names(survey.data))
names(survey.data)<-gsub("happening_aware_","aware_",names(survey.data))
names(survey.data)<-gsub("happening_aware","aware",names(survey.data))

survey.data2<-survey.data%>%
  select(contains("attribution"),contains("aware"),contains("worry"),mergekey)%>%
  filter_all(any_vars(!is.na(.)))

## identify number of countries per question: ####
qs<-survey.data%>%
  select(contains("attribution"),contains("aware"),contains("worry_")#,contains("action_"))
  )
## temporary: drop "tisp" questions as they are going to get dropped from the megapoll
qs<-qs%>%
  select(-contains("_tisp"))
qs<-names(qs)

d.samples<-survey.data%>%
  select(qs,iso_3166)%>% 
  # mutate_at(contains("concern_"),contains("happening_"),contains("human_"),as.numeric)%>%
  group_by(iso_3166)%>%
  summarise_at(all_of(qs),funs(n=sum(!is.na(.))))%>%
  ungroup()
d.samples<-d.samples%>%
  pivot_longer(cols=2:ncol(d.samples),names_to="question",values_to="size")%>%
  mutate(question=substr(question,1,nchar(question)-2))
n.qns<-d.samples%>%
  group_by(question)%>%
  filter(size!=0)%>%
  summarize(ctry.n=n_distinct(iso_3166))%>%
  arrange(desc(ctry.n))

qlist<-filter(n.qns,ctry.n>=20)
qlist<-unique(qlist$question)

country.yr.means<-list()



for(i in 1:length(qlist)){
  print(i)
  print(qlist[i])
  d<-survey.data%>%
    select(iso_3166,year2,qlist[i])%>%
    drop_na()%>%
    # mutate(qlist[i]=as.numeric(qlist[i]))%>%
    group_by(iso_3166,year2)%>%
    summarise(val=mean(get(qlist[i])))%>% ## take mean within country-year for this question
    # d<-d%>%
    group_by(year2)%>% ## group by year and scale
    mutate(val=scale(val))%>%
    group_by(iso_3166)%>% ## take within-country mean of these scaled values
    summarise(val=mean(val,na.rm=TRUE))%>%
    mutate(question=qlist[i])
  country.yr.means[[i]]<-d
}
country.yr.means<-do.call(rbind,country.yr.means)
country.yr.means%<>%
  bind_rows(country.yr.means.np%>%anti_join(country.yr.means,by=c("iso_3166","question")))
country.yr.means.wide<-pivot_wider(country.yr.means,id_cols=c(iso_3166),names_from="question",values_from="val")
cormat.yr<-cor(country.yr.means.wide[,3:ncol(country.yr.means.wide)],use="pairwise.complete.obs")

## find average correlation for each variable 
cormat.df<-data.frame(cormat.yr)
avgcor<-apply(cormat.df,MARGIN = 2,mean,na.rm=TRUE)
avgcor<-data.frame(var=names(cormat.df),cor=round(avgcor,2))%>%
  mutate(cor=paste(var,cor,sep=": "))
rownames(cormat.yr)<-avgcor$cor
colnames(cormat.yr)<-avgcor$cor

pdf(paste0(figfolder,"corplot_country_year_sub.pdf"))
corrplot(cormat.yr,order="alphabet",
         #order="FPC",## order by first principal component
         tl.pos="lt",
         tl.cex=.5,number.cex=0.5)
dev.off()

# Fig. val.2012-24: validate over-time trends against an independent survey question ####
## compares the model's over-time change in national climate concern against the
## change in national-average responses to a single, independently-fielded survey
## question (Pew: "is climate change a major threat, minor threat, or not a threat"),
## asked in both the 2012-13 and 2022-23 waves -- a direct test of whether the model's
## estimated trends track real survey movement, not just cross-sectional levels.

pew_means<-survey.data%>%
  filter(year2%in%c("2012-13","2024-25"))%>%
  select(iso_3166,year2,worry_climatethreat_pew)%>%
  drop_na()%>%
  group_by(iso_3166,year2)%>%
  summarise(resp=mean(worry_climatethreat_pew,na.rm=TRUE),.groups="drop")%>%
  pivot_wider(names_from=year2,values_from=resp)%>%
  mutate(delta.survey=`2024-25`-`2012-13`)%>%
  filter(!is.na(delta.survey))

model_means<-est.nat.allyears%>%
  filter(year%in%c("2012-13","2024-25"))%>%
  select(iso_3166,year,mean)%>%
  pivot_wider(names_from=year,values_from=mean)%>%
  mutate(delta.model=`2024-25`-`2012-13`)%>%
  filter(!is.na(delta.model))

## each variable standardized by its OWN cross-country SD at the 2012-13 baseline,
## using the full available sample at that wave (not restricted to the comparison subset)
model.sd.2012.13<-sd(est.nat.allyears$mean[est.nat.allyears$year=="2012-13"],na.rm=TRUE)

survey.2012.13.all<-survey.data%>%
  filter(year2=="2012-13")%>%
  select(iso_3166,worry_climatethreat_pew)%>%
  drop_na()%>%
  group_by(iso_3166)%>%
  summarise(resp=mean(worry_climatethreat_pew,na.rm=TRUE),.groups="drop")
survey.sd.2012.13<-sd(survey.2012.13.all$resp,na.rm=TRUE)

val.2012.22<-model_means%>%
  select(iso_3166,delta.model)%>%
  inner_join(pew_means%>%select(iso_3166,delta.survey),by="iso_3166")%>%
  mutate(delta.model.std=delta.model/model.sd.2012.13,
         delta.survey.std=delta.survey/survey.sd.2012.13)

r.2012.24<-cor(val.2012.24$delta.model.std,val.2012.24$delta.survey.std)
cat("Fig val.2012-24: n =",nrow(val.2012.24)," countries, r =",round(r.2012.24,2),"\n")

val.2012.24.plot<-val.2012.24%>%
  ggplot(aes(x=delta.model.std,y=delta.survey.std,label=iso_3166))+
  geom_abline(slope=1,intercept=0,lty="dashed")+
  geom_text(size=3)+
  annotate("text",x=max(val.2012.24$delta.model.std)-0.5,
           y=min(val.2012.24$delta.survey.std)+0.3,
           label=paste0("r = ",round(r.2012.24,2)),color="blue")+
  labs(x="Change in climate concern\n(standard deviations)",
       y="Change in country-aggregated survey responses\n(standard deviations)")+
  theme_bw()
val.2012.24.plot
ggsave(file=paste0(figfolder,"validation_2012-2024_",modelname,".pdf"),width=6.5,height=6.5,val.2012.24.plot)

# dimensionality for surveys for which we have multiple questions ####
sources<-c("worldbank_2010","mildenbergerdynata_2021")
dsub<-survey.data%>%
  filter(source%in%sources)%>%
  select(-year,-year2,-source2,-iso_3166,
         -regioncode,-constructed,
         -mergekey,-reference_survey,-support_weight,-likelihood_route,-n_support,-reference_survey#,-age,
         #-respondent_gender
         )%>%
  select(-contains("respondent"))

## Figure S2: side-by-side screeplots for Mildenberger and World Bank surveys #### 
## (the only 2 in our data set that asked multiple questions in multiple countries)
dsub.m<-dsub%>%filter(source=="mildenbergerdynata_2021")
qs.m<-dsub.m%>%
  select(-source)%>%
  pivot_longer(everything(),names_to="question",values_to="response")%>%
  filter(!is.na(response))%>%
  group_by(question)%>%
  summarise(total=sum(response))%>%
  filter(total>0)
qs.m<-unique(qs.m$question)
dsub.m<-dsub.m%>%select(all_of(qs.m))%>%
  drop_na()
## scale responses
dsub.m2<-apply(dsub.m,MARGIN=2,FUN=function(x){scale(x)})

ev.m<-eigen(cor(dsub.m2))
ap.m<-parallel(subject=nrow(dsub.m2),var=ncol(dsub.m2),
             rep=100,cent=0.05)
nS.m<-nScree(x=ev.m$values,ap=ap.m$eigen$qevpea) 
pdf(height=3.5,width=3,pointsize=8,
    file=paste0(figfolder,"screeplot_mildenberger_2021.pdf"))
plotnScree(nS.m,main="Scree plot for 2021 Validation Survey")
dev.off()

## examine the screeplot with only the 5 remaining items after removing the awareness item 
dsub.m%<>%select(-happening_ccam)
qs.m<-dsub.m%>%
  pivot_longer(everything(),names_to="question",values_to="response")%>%
  filter(!is.na(response))%>%
  group_by(question)%>%
  summarise(total=sum(response))%>%
  filter(total>0)
qs.m<-unique(qs.m$question)
dsub.m<-dsub.m%>%select(all_of(qs.m))%>%
  drop_na()
## scale responses
dsub.m2<-apply(dsub.m,MARGIN=2,FUN=function(x){scale(x)})

ev.m<-eigen(cor(dsub.m2))
ap.m<-parallel(subject=nrow(dsub.m2),var=ncol(dsub.m2),
               rep=100,cent=0.05)
nS.m<-nScree(x=ev.m$values,ap=ap.m$eigen$qevpea) 
pdf(height=3.5,width=6,pointsize=8,
    file=paste0(figfolder,"screeplot_mildenberger_2021_nohappening.pdf"))
plotnScree(nS.m,main="Scree plot for 2021 Validation Survey, without awareness item")
dev.off()




dsub.wb<-dsub%>%filter(source=="worldbank_2010")
qs.wb<-dsub.wb%>%
  select(-source)%>%
  pivot_longer(everything(),names_to="question",values_to="response")%>%
  filter(!is.na(response))%>%
  group_by(question)%>%
  summarise(total=sum(response))%>%
  filter(total>0)
qs.wb<-unique(qs.wb$question)
dsub.wb<-dsub.wb%>%select(all_of(qs.wb))%>%
  drop_na()
## scale responses
dsub.wb2<-apply(dsub.wb,MARGIN=2,FUN=function(x){scale(x)})

ev.wb<-eigen(cor(dsub.wb2))
ap.wb<-parallel(subject=nrow(dsub.wb2),var=ncol(dsub.wb2),
               rep=100,cent=0.05)
nS.wb<-nScree(x=ev.wb$values,ap=ap.wb$eigen$qevpea) 
pdf(height=3.5,width=3,pointsize=8,
    file=paste0(figfolder,"screeplot_worldbank_2010.pdf"))
plotnScree(nS.wb,main="Scree plot for 2010 World Bank Survey")
dev.off()

# table of respondents for which region codes have been constructed #### 
table(survey.data$constructed)
table(nonpublic.constructed$constructed)
unique(survey.data$source[survey.data$constructed==2])
sum(table(survey.data$constructed))
nrow(survey.data)
unique(survey.data[survey.data$constructed=="NA","source"])


