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
## 1. EDGAR energy-sector CO2 by unit 
edgar_file <- "predictors/EDGAR_2025_GHGs_CO2eq_AR5_NUTS2_by_country_sector_1990-2024_b.xlsx"
raw <- suppressMessages(read_excel(edgar_file, sheet = 2, col_names = FALSE, n_max = 20))
hdr <- which(apply(raw, 1, function(r) any(r == "ISO", na.rm = TRUE)))[1]   # the header row (row 1 if the metadata rows were removed, row 7 in the original file)
edgar <- read_excel(edgar_file, sheet = 2, skip = hdr - 1)
edgar <- edgar %>% rename(code = `Subnational code *`, desc = `Subnational desc`) %>%
  filter(code != "N/A", Sector == "Energy") %>%
  group_by(ISO, Country, code, desc) %>% summarise(e2021 = sum(Y_2021, na.rm = TRUE), e2024 = sum(Y_2024, na.rm = TRUE), .groups = "drop")  # kilotonnes CO2

## 2. model regions and populations 
key <- read_csv("basedata/model_region_key.csv", show_col_types = FALSE, na = "") %>% 
  select(mergekey, iso_3166, population, region_name)
regions <- st_read("basedata/simplified geometries/model_regions_IR7_simplified.gpkg", quiet = TRUE) %>% 
  select(mergekey, iso_3166)

## 3a. NUTS 2024 units: population-weighted overlay 
nuts <- st_read("basedata/NUTS_RG_01M_2024_4326.gpkg", quiet = TRUE) %>% 
  filter(LEVL_CODE == 2, NUTS_ID %in% edgar$code) %>% 
  select(NUTS_ID)
countries_nuts <- unique(substr(nuts$NUTS_ID, 1, 2)); countries_nuts[countries_nuts == "EL"] <- "GR"
m <- regions %>% filter(iso_3166 %in% countries_nuts) %>% st_transform(3035) %>% st_make_valid()
n <- nuts %>% st_transform(3035) %>% st_make_valid()
m$a_m <- as.numeric(st_area(m)); n$a_e <- as.numeric(st_area(n))
ix <- suppressWarnings(st_intersection(m, n)); ix$a_x <- as.numeric(st_area(ix))
ov <- st_drop_geometry(ix) %>% mutate(cover_m = a_x / a_m, cover_e = a_x / a_e) %>%
  filter(!(cover_m < 0.20 & cover_e < 0.02)) %>%                      # drop boundary slivers (model region mostly outside the unit and under 2% of it)
  left_join(key %>% select(mergekey, population), by = "mergekey") %>%
  group_by(NUTS_ID) %>% mutate(weight = population * cover_m / sum(population * cover_m)) %>%
  filter(weight >= 0.001) %>% mutate(weight = weight / sum(weight)) %>% ungroup() %>%
  transmute(edgar_code = NUTS_ID, mergekey, weight, method = "nuts2024-overlay")

## 3b. United Kingdom: EDGAR uses NUTS 2021 level-2 codes; our regions are NUTS 1 (UKC1 -> UKC) 
uk <- edgar %>% filter(ISO == "GBR") %>% 
  transmute(edgar_code = code, mergekey = substr(code, 1, 3), weight = 1, method = "uk-nuts1-prefix")

## 3c. Ukraine: EDGAR's own codes -> ISO 3166-2 oblasts 
ua_map <- c(UA11 = "UA-53", UA12 = "UA-59", UA13 = "UA-63", UA14 = "UA-74", UA21 = "UA-14", UA22 = "UA-09", UA31 = "UA-12", UA32 = "UA-23",
            UA33 = "UA-35", UA41 = "UA-48", UA42 = "UA-51", UA43 = "UA-65", UA44 = "UA-43", UA45 = "UA-40", UA51 = "UA-05", UA52 = "UA-61",
            UA53 = "UA-68", UA61 = "UA-32", UA62 = "UA-71", UA63 = "UA-30", UA71 = "UA-21", UA72 = "UA-26", UA73 = "UA-46", UA74 = "UA-77",
            UA81 = "UA-07", UA82 = "UA-18", UA83 = "UA-56")   # Poltava, Sumy, Kharkiv, Chernihiv, Donetsk, Luhansk, Dnipropetrovsk, Zaporizhzhia, Kirovohrad, Mykolaiv, Odesa, Kherson, Crimea, Sevastopol, Vinnytsia, Ternopil, Khmelnytskyi, Kyiv oblast, Cherkasy, Kyiv city, Zakarpattia, Ivano-Frankivsk, Lviv, Chernivtsi, Volyn, Zhytomyr, Rivne
ua <- tibble(edgar_code = names(ua_map), mergekey = unname(ua_map), weight = 1, method = "ukraine-table") %>% filter(edgar_code %in% edgar$code)

crosswalk <- bind_rows(ov, uk, ua) %>%
  left_join(edgar %>% select(edgar_code = code, edgar_iso3 = ISO, edgar_name = desc), by = "edgar_code") %>%
  left_join(key %>% select(mergekey, model_region_name = region_name, iso_3166), by = "mergekey") %>%
  select(edgar_iso3, edgar_code, edgar_name, mergekey, model_region_name, iso_3166, weight, method) %>% arrange(edgar_iso3, edgar_code, desc(weight))
stopifnot(all(abs(tapply(crosswalk$weight, crosswalk$edgar_code, sum) - 1) < 1e-6))   # each EDGAR unit's weights sum to 1
write_csv(crosswalk, "predictors/edgar2025_to_model_crosswalk.csv")

## 4. region emissions = sum(weight x unit emissions); per capita; log 
emissions_regions <- crosswalk %>% 
  inner_join(edgar %>% select(code, e2021, e2024), by = c("edgar_code" = "code")) %>%
  group_by(mergekey) %>% summarise(energy_co2_kt_2021 = sum(weight * e2021), energy_co2_kt_2024 = sum(weight * e2024), .groups = "drop") %>%
  inner_join(key %>% select(mergekey, iso_3166, population), by = "mergekey") %>%
  mutate(co2_pc_t_2021 = energy_co2_kt_2021 * 1000 / population, co2_pc_t_2024 = energy_co2_kt_2024 * 1000 / population,   # tonnes per person
         log_pc_2021 = log(co2_pc_t_2021), log_pc_2024 = log(co2_pc_t_2024))
write_csv(emissions_regions, "predictors/emissions_eu_regions_2025edgar.csv")
cat("crosswalk:", nrow(crosswalk), "rows;", length(unique(crosswalk$edgar_code)), "EDGAR units;", nrow(emissions_regions), "regions with emissions\n")

