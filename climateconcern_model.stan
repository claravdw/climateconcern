/**
 * Latent trait model for climate opinion with country and region random walks
 */


//this function helps us calculate the likelihood of different observations
//(i.e. counts of respondents choosing a certain answer option) in parallel
//to speed up sampling.
functions {

  real partial_sum_lpmf(array[] int slice_y,
                        int start, int end,
                        array[] int options, array[] int questions,array[] int regions, array[] int years,
                        array[] real gamma, array[,] real region_alpha, array[] vector beta) {
                        
    vector[end - start + 1] result;
    for (i in start:end) {
      int j = questions[i];
      result[i - start + 1] = slice_y[i - start + 1] * ordered_logistic_lpmf( options[i] | gamma[j] * region_alpha[ regions[i] , years[i] ], 
                                                                                           gamma[j] * to_vector(beta[j,]) );
    }
    return sum(result);
    
  }
  
}

data {

  int<lower=0> C; //number of countries
  int<lower=0> TT; //number of years
  int<lower=0> J;  //number of items
  int<lower=0> R;  //number of regions
  int<lower=1> K; // max number of answer options
  array[J] int<lower=0> Kmin1; //number of cutpoints for each question (no. of options - 1)
  
  //region-level data
  int<lower=0> RTJK; //number of observed region-year-question-options
  array[RTJK] int<lower=0> y; //responses per observation (country-year-question-option)
  array[RTJK] int<lower=0> options; //options of the observations
  array[RTJK] int<lower=0> countries; //countries of the observations
  array[RTJK] int<lower=0> years; //years of the observations
  array[RTJK] int<lower=0> questions; //questions of the observations
  array[RTJK] int<lower=0> regions; //regions of the observations
  
  //nation-level data
  int<lower=0> CTJK; //number of observed country-year-question-options
  array[CTJK] int<lower=0> y_nat; //responses per observation (country-year-question-option)
  array[CTJK] int<lower=0> options_nat; //options of the observations
  array[CTJK] int<lower=0> countries_nat; //countries of the observations
  array[CTJK] int<lower=0> years_nat; //years of the observations
  array[CTJK] int<lower=0> questions_nat; //questions of the observations
  
  array[R] int<lower=0> reg_to_country; //mapping from region to country
  array[R] real<lower=0,upper=1> reg_to_weight; //mapping from region to weight (population proportion; sum to 1 within countries)

}

parameters {
  
  array[C,TT] real country_alpha_raw; //country ability steps
  array[R,TT] real region_effect_raw; //region effect steps
  
  real<lower=0> country_step; //sd of country random walks
  real<lower=0> region_step; //sd of region random walks
  
  array[J] ordered[K-1]  beta_raw; //raw cutpoints, before adjusting slope and location to K
  array[J] real<lower=0> gamma_raw; //discrimination, before scaling; should be positive
 
}

transformed parameters {

  array[C,TT] real country_alpha; //country abilities
  array[R,TT] real region_effect; //region effects
  array[R,TT] real region_alpha; //region abilities
  array[C,TT] real country_poststrat; //country abilities
  
  array[J] ordered[K-1] beta_uncentered; //cutpoints, before centering
  real mean_beta_mid = 0;
  array[J] ordered[K-1] beta; //centered cutpoints
  array[J] real<lower=0> gamma; //scaled discrimination
  
  //contructing the country random walks and region random walks
  
  //construct country random walks out of steps; for each country, go over periods and take the
  //cumulative sum of the steps taken so far. Mutliply them by country_step, the parameter that
  //determines country step size (since the raw steps just have a standard normal prior)
  for(c in 1:C){
    country_alpha[c,] = to_array_1d(to_vector(cumulative_sum(country_alpha_raw[c,])) * country_step);
  }
  
  //construct region random walks out of steps; for each region, go over periods and take the
  //cumulative sum of the steps taken so far. Mutliply them by region_step, the parameter that
  //determines region step size (since the raw steps just have a standard normal prior)
  for(r in 1:R){
    region_effect[r,] = to_array_1d(to_vector(cumulative_sum(region_effect_raw[r,])) * region_step);
  }
  
  
  //putting them together into the final region alphas ("abilities", climate concern scores)
  
  //calculate region alphas by summing country and region walks
  for(t in 1:TT){ //for each period
    for(r in 1:R){ //for each region
      region_alpha[r,t] =  country_alpha[ reg_to_country[r], t  ] + region_effect[ r, t ];
      //makes use of reg_to_country[r], which gives us the index of the country that region
      //r is in. It is this country whose random walk we need to add to the region-speficic
      //random walk, in order to get the region's total alpha.
    }
  }
  
  
  //calculating the countries' "abilities" (climate concern scores) by weighting their regions

  //get post-stratified country abilities per year; they are the weighted sum of region abilities
  country_poststrat = rep_array(0,C,TT); //intialize them all to zero
  for(t in 1:TT){ //for each period
    for(r in 1:R){ //for each region
      country_poststrat[ reg_to_country[r], t ] += region_alpha[r,t] * reg_to_weight[r];
    }
  }


  //turn the raw question parameters (cutpoints beta and discrimination gamma) into
  //their final versions (whose scale and location is identified)
  
  //go from raw beta to beta (slope and location adjusted for number of cutpoints)
  for (j in 1:J){
    real adj_location = Kmin1[j]  + 1;
    real adj_slope = (Kmin1[j] != 1) ? (Kmin1[j] - 1) : 1;
    beta_uncentered[j] = (2*beta_raw[j] - adj_location) / adj_slope;
  }
  //this results in beta ~ normal((k-adj_location)/adj_slope, 1)
  
  //calculate average mean cutpoint across items
  for(j in 1:J){
      mean_beta_mid += mean( segment(beta_uncentered[j], 1, Kmin1[j]));
  }
  mean_beta_mid = mean_beta_mid / J;
  
  //center betas and scale gammas
  for(j in 1:J){
    //location identification: mean cutpoints must have average 0
    beta[j] = beta_uncentered[j] - mean_beta_mid;
    //scale identification: product of discriminations must be 1
    gamma[j] = gamma_raw[j] / ( pow(prod(gamma_raw), 1.0/J) );
  }
  
}

model {

  int grainsize = 1; //this means grainsize should be picked automatically

  //Priors: random walks for locations' climate concern

  //country random walk steps (take their cumulative sum to construct the walk)
  country_alpha_raw[,1] ~ normal(0, 5);
  for(t in 2:TT){
    country_alpha_raw[,t] ~ normal(0, 1); //sd=1, because steps will be multiplied by country_step later
  }
  
  //subnational region random walk steps (take their cumulative sum to construct the walk)
  region_effect_raw[,1] ~ normal(0, 3);
  for(t in 2:TT){
    region_effect_raw[,t] ~ normal(0, 1); //sd=1, because steps will be multiplied by region_step later
  }

  //informative prior for...
  country_step ~ gamma(2, 8); //...sd of country random walks
  region_step ~ gamma(1, 8); //...sd of region random walks
  
  //note! second parameter in stan gamma distributions is rate, not scale;
  //the the larger this parameter, the *smaller* the mean and variance
  
  
  //Priors: question parameters (threholds beta and discrimination gamma)

  //informative prior for discrimination
  gamma_raw ~ gamma(1, .55);
  
  //set cutpoint (or "threshold") priors such that (1) the middle cutpoint has
  //a prior centered around zero; and (2) the lowest and highest cutpoint also
  //have similar priors across questions
  for (j in 1:J){
    for (k in 1:(K-1)){
        //add 100 to any cutpoints that this question is not using
        //(because it doesn't have that many options), so they don't
        //accidentally limit the actually-in-use cutpoints
        real offset_unused = (k > Kmin1[j]) ? 100 : 0;
        real adj_slope = (Kmin1[j] != 1) ? (Kmin1[j] - 1) : 1;
        beta_raw[j][k] ~ normal(k + offset_unused, adj_slope);
    }
  }

  //log-likelihood, regional data
  target += reduce_sum(partial_sum_lupmf, y, grainsize, options, questions, regions, years, gamma, region_alpha, beta);
  
  //log-likelihood, national data (countries_nat instead of regions, country_poststrat instead of region_alpha)
  target += reduce_sum(partial_sum_lupmf, y_nat, grainsize, options_nat, questions_nat, countries_nat, years_nat, gamma, country_poststrat, beta);

}
