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
est.nat%<>%filter(year%in%c("2010-11","2024-25"))
est.reg%<>%filter(year%in%c("2010-11","2024-25"))
est.nat%>%
  select(iso_3166,mean.scl,year)%>%
  pivot_wider(names_from=year,values_from=mean.scl)%>%
  mutate(delta=`2024-25`-`2010-11`,
         # increase=ifelse(delta>=0,1,0))%>%
         change=case_when(delta==0~"stable",
                            delta>0~"increase",
                            delta<0~"decrease"))%>%
  group_by(change)%>%
  count() ## 108 increase, 61 decrease

# fig of top increasers and decreasers #### 
summ<-est.nat%>%
  select(NAME_0_gadm,mean.scl,year)%>%
  pivot_wider(names_from=year,values_from=mean.scl)%>%
  mutate(delta=`2024-25`-`2010-11`,
         increase=factor(ifelse(delta>0,1,0)))


summtop<-summ%>%
  arrange(desc(delta))%>%
  slice_head(n=10)
summbottom<-summ%>%
  arrange(delta)%>%
  slice_head(n=10)
mypal<-brewer.pal(10,"RdBu")[c(3,8)]
change<-rbind(summtop,summbottom)%>%
  mutate(NAME_0_gadm=factor(NAME_0_gadm),
         NAME_0_gadm=fct_reorder(NAME_0_gadm,delta))%>%
  arrange(NAME_0_gadm)
changefig<-change%>%
  ggplot()+
  geom_segment(aes(x=`2010-11`,y=NAME_0_gadm,xend=`2024-25`,yend=NAME_0_gadm,color=increase),
                            arrow=arrow(length=unit(0.2,"cm")))+
  geom_point(aes(x=`2010-11`,y=NAME_0_gadm,color=increase))+
  scale_color_manual(values=rev(mypal))+
  theme_bw()+
  labs(x="Climate concern",y="")+
  theme(legend.position="none")

#quartz(12,12)
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

countries_proj1<-st_transform(countries, "+proj=moll")

## deltas 2010-2020
countrychange%<>%filter(!is.na(delta))
countrydelta_proj<-st_transform(countrychange,"+proj=moll")

worldplot<-countrynoest_proj%>%
  ggplot()+
  theme_bw()+
  geom_sf(aes(geometry=geom),fill="lightgray",lwd=.1,color="darkgray")+
  geom_sf(data=countryest_proj,aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
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
ggsave(file=paste0(figfolder,"map_projected_paired_",modelname,"_",datafilter,".pdf"),height=6.5,width=6.5,
       worldplot)

## fig. S9: Deltas
worldplot_delta<-countrynoest_proj%>%
  ggplot()+
  theme_bw()+
  geom_sf(aes(geometry=geom),fill="lightgray",lwd=.1,color="darkgray")+
  geom_sf(data=countrydelta_proj,aes(geometry=geom,fill=delta),lwd=.1,color="darkgray",
          show.legend=c(fill=TRUE))+
  scale_fill_distiller(palette="RdBu")+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  labs(fill="Change in \nclimate\nconcern")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf()
worldplot_delta
ggsave(file=paste0(figfolder,"map_delta_",modelname,"_",datafilter,".pdf"),height=6.5,width=6.5,
       worldplot_delta)

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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),
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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
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
  scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
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

## quadrants plot without 2010 values #### 
reg1<-lm_robust(mean.std~exp.std,data=subset(est.nat,year=="2024-25"))

## table S6: climate risk exposure and climate concern 
texreg::texreg(reg1,custom.coef.names=c("(Intercept)","Exposure (st.dev.)"),
       custom.model.names="Climate concern",
       include.ci=FALSE,label="tab:regressionsimple",
       caption="{ \\bf Effect of climate risk exposure on climate concern:} The table shows the results from an OLS regression of climate concern (standardized) on climate risk exposure (standardized), at the national level, in 2022. The model was estimated using OLS regression with heteroskedasticity-robust standard errors.",
       file=paste0(figfolder,"regression_results_simple.tex"),float.pos="!htpb")

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
  annotate("text",x=3,y=.5,label=paste("beta == ",round(reg1$coefficients[2],2)),color="blue",parse=TRUE)+
  annotate("text",x=3,y=.3,label=paste("p == ",round(reg1$p.value[2],2)),color="blue",parse=TRUE)+
  labs(x="Climate exposure (st.dev)",y="Climate concern (st.dev)")
quadrants
ggsave(file=paste0(figfolder,"quadrants_exposure_concern_",modelname,".pdf"),width=6.5,height=6.5,quadrants)

# Figure 6: sub-national emissions regression, scatterplot #### 
emissions<-read_csv(paste0(repofolder,"predictors/emissions_clean.csv"))

regions=left_join(regions,emissions,by=c("mergekey"="ourcodes"))
europe<-regions%>%filter(Continent_Name=="Europe")%>%filter(year=="2024-25") ## filter to Europe and only most recent year
europe%<>%
  filter(!is.na(emissions.percap))
europe$mean.std<-scale(europe$mean,center=TRUE,scale=TRUE) ## this regression standardizes estimates *within* Europe for interpretation. 
europe$emissions.log.std<-scale(log(europe$emissions.percap),center=TRUE,scale=TRUE)

reg2<-feols(mean.std~emissions.log.std|iso_3166,vcov=~iso_3166,data=europe)
summary(reg2)
reg2<-coeftable(reg2)
rownames(reg2)[1]<-"Emissions (logged, standardized)"


## table S7: energy sector emissions and climate concern 
print.xtable(xtable(reg2,label="tab:regression.emissions.concern",caption="{\\bf Effect of energy-sector CO$_2$ emissions on climate concern:} The figure table shows the results from an OLS regression of climate concern (standardized, in 2024-25) on per-capita emissions from the energy sector (standardized, 2021) at the sub-national region level in the European Union. Standard errors are clustered by country."),
             caption.placement="bottom",file=paste0(figfolder,"regression_results_emissions.tex"))

## without country FEs: 
# reg2<-feols(mean.std~emissions.log.std,vcov=~iso_3166,data=europe)
# summary(reg2)
# reg2<-coeftable(reg2)
# rownames(reg2)[2]<-"Emissions (logged, standardized)"

# emissionsplot<-ggplot(data=europe,aes(x=emissions.log.std,y=mean.std))+
#   geom_point()+
#   geom_smooth(method="lm",se=FALSE,lty="dashed")+
#   annotate("text",x=-3,y=-1.5,label=paste("beta == ",round(reg2[1,1],2)),
#                                           # round(reg2$coefficients[2],2)),
#            color="blue",parse=TRUE)+
#   annotate("text",x=-3,y=-1.8,label=paste("p == ",round(reg2[1,4],2)),
#                                         # round(reg2$p.value[2],10)),
#            color="blue",parse=TRUE)+
#   # scale_y_continuous(limits=c(-2,3))+
#   labs(x="Energy-sector carbon dioxide emissions\n(tons per capita, logged, st.dev, 2021)",y="Climate concern (st.dev), 2024-25")+
#   theme_bw()
# emissionsplot
# ggsave(file=paste0(figfolder,"scatter_emissions_concern_europeregions_",modelname,".pdf"),width=6.5,height=3.5,emissionsplot)

## plot results, demeaning by country FEs 
europe%<>%
  group_by(iso_3166)%>%
  mutate(mean.std.within = mean.std - mean(mean.std, na.rm=TRUE),
         emissions.log.std.within = emissions.log.std - mean(emissions.log.std, na.rm=TRUE))%>%
  ungroup()

emissionsplot_fe<-ggplot(data=europe,aes(x=emissions.log.std.within,y=mean.std.within))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,lty="dashed")+
  annotate("text",x=-3,y=1.2,label=paste("beta == ",round((reg2)[1,1],3)),
           color="blue",parse=TRUE)+
  annotate("text",x=-3,y=1.0,label=paste("p == ",round((reg2)[1,4],2)),
           color="blue",parse=TRUE)+
  labs(x="Energy-sector carbon dioxide emissions\n(logged, standardized, within-country deviation)",
       y="Climate concern\n(standardized, within-country deviation)")+
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


