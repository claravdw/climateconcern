#required packages
library(rstan)
rstan_options(auto_write = TRUE)
library(parallel)
library(plyr)
library(pryr)
library(gtools)

# to install cmdstanr from source:
# install.packages("remotes")
# remotes::install_github("stan-dev/cmdstanr")
# install_cmdstan()
# set_cmdstan_path(...) #set path to where cmdstanr is installed
# more info:
# https://mc-stan.org/cmdstanr/articles/cmdstanr.html

library(cmdstanr)

##get model and data related parameters

#model, data and mode
modelname <- "climateconcern_model"
dataname <- "d_rstan_region"
dataextension <- "_2yr_concernhuman"
no_chains <- 4

#parameter settings
debug <- T #debugging mode on?

#get number of cores
MC_CORES <- detectCores()
options(mc.cores = MC_CORES)

##function for faster summary of parameters

parsummarise_draws <- function(draws, n_cores=1, n_chunks=NULL){
  
  n_pars <- dim(draws)[3]
  if(is.null(n_chunks)){n_chunks <- ceiling(n_pars/500)}
  chunk_size <- ceiling(n_pars/n_chunks)
  chunk_list <- list()
  for(i in 1:n_chunks){
    if((chunk_size*(i - 1) + 1) <= n_pars){
      chunk_list[[i]] <- draws[,,(chunk_size*(i - 1) + 1):(min(c(chunk_size*i,n_pars)))]
    }
  }
  
  summary_list <- parallel::mclapply(chunk_list, posterior::summarise_draws, mc.cores = n_cores)
  
  output <- do.call("rbind", summary_list)
  return(output)
  
}
#see https://github.com/stan-dev/posterior/issues/104


##read in and prepare data

#load the opinion megapoll
load(paste0("data/", dataname, dataextension, ".Rda"))
d <- d[complete.cases(d),]
d.nat <- d.nat[complete.cases(d.nat),]
d.regnat <- smartbind(d, d.nat)

#count number of thresholds per question
no_of_thres <- sapply(sort(unique(d.regnat$question_int)),
                      function(x) max(d.regnat$option[d.regnat$question_int==x])-1)

#map regions onto countries
regions_countries <- unique(d[,c("regioncode_int","iso_3166_int")])
regions_countries <- regions_countries[order(regions_countries$regioncode_int),]
reg_to_country <- regions_countries$iso_3166_int
save(regions_countries,
     file=paste0("data/regions_countries.Rdata"))
#reg_to_country is a vector of country indices;
#for the region whose index corresponds to this position, what
#is the index of the country that it is in?
#e.g. an 8 in position 3 would mean that region with integer ID 3
#is in country with integer ID 8

#load region weights for poststratification
load(paste0("data/regionweights", dataextension, ".Rda"))
regionpop <- regionpop[complete.cases(regionpop),]
regionpop <- regionpop[order(regionpop$regioncode_int),]
reg_to_weight <- regionpop$wt
#reg_to_weight is a vector of region population weights;
#for the region corresponding to this index, what
# % of its country's population does it contain
#e.g. a 0.50 in position 3 would that region with integer ID 3
#makes up 50% of its country's population


##create data object for stan

opinion_dat <- list(
  
  #constants
  
  C = length(unique(d.regnat$iso_3166_int)), # number of countries
  TT = max(d.regnat$year_int), #number of periods
  #d$year must be recoded such that earliest period is one, because it is being reused as an index
  #called TT because T is not a valid variable name
  J = length(unique(d.regnat$question_int)), #number of questions
  R = max(d$regioncode_int), #number of regions
  K = max(d.regnat$option), #maximum number of answer options *across all questions*
  Kmin1 = no_of_thres, #number of thresholds *for each question*
  
  #subnational region-level data
  
  RTJK = nrow(d), #number of region-period-question-option combos
  #take columns of this dataset and put them into variables;
  #each entry in a column stands for a region-period-question-option
  y = d$yes, #number of respondents choosing this option in this region-period-question
  options = d$option, #number of the answer option
  countries = d$iso_3166_int, #iso of the country; recoded into integers
  years = d$year_int, #period; recoded into integers
  questions = d$question_int, #question ID; recoded into integers
  regions = d$regioncode_int, #region code; recoded into integers
  
  #country-level data
  
  CTJK = nrow(d.nat), #number of country-period-question-option combos
  #take columns of this dataset and put them into variables;
  #each entry in a column stands for a country-period-question-option
  y_nat = d.nat$yes, #number of respondents choosing this option in this region-period-question
  options_nat = d.nat$option, #number of the answer option
  countries_nat = d.nat$iso_3166_int, #iso of the country; recoded into integers
  years_nat = d.nat$year_int, #period; recoded into integers
  questions_nat = d.nat$question_int, #question ID; recoded into integers
  
  #mappings from regions to countries and weights
  reg_to_country = reg_to_country,
  reg_to_weight = reg_to_weight
  
)


##set sampling parameters

smp_para <- list()
if(debug){
  smp_para$iter_total <- 75
  smp_para$warmup <- 25
} else {
  smp_para$iter_total <- 1500
  smp_para$warmup <- 500
}
smp_para$iter_sampling <- smp_para$iter_total - smp_para$warmup
smp_para$no_chains <- no_chains


##fit model with stan

model <- cmdstan_model(paste0(modelname, '.stan'),
                       cpp_options = list(stan_threads = TRUE),
                       pedantic = TRUE)
fit<- model$sample(data = opinion_dat,                                                  
                     iter_sampling = smp_para$iter_sampling,
                     iter_warmup = smp_para$warmup,
                     chains = smp_para$no_chains,
                     parallel_chains = smp_para$no_chains,
                     threads_per_chain = MC_CORES/smp_para$no_chains,
                     seed=123)
gc() #garbage collection


##save results

#save summary of the parameter estimates
summ_stan <- parsummarise_draws(fit$draws(), n_cores=MC_CORES)
#create as column with parameter names
summ_stan$parameter <- sub("\\[.+\\]", "", row.names(summ_stan))
save(summ_stan, file=paste0("outputs_stan/stan_summ.Rdata"),
     compress="xz")

#save whole model object
fit$save_object(file = "stan_full.Rds", compress="xz"))
