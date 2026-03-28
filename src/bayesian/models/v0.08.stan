// v0.08: Extends v0.08.00 with shared grid covariates on phi_tents and phi_housing alongside tower intercepts and distance-decaying rho.
data {
  int I; // number of grids
  int G; // number of governorates
  int M; // number of municipalities
  int H; // number of neighbourHoods
  int J1; // number of towers from provider 1
  int J2; // number of towers from provider 2
  int K; // number of grid-level covariates
  int N_tot; // total population size
  real<lower=0> s_rho; // fixed distance-decay scale
  array[I] int<lower=1, upper=G> gg; // governorate of each grid
  array[I] int<lower=1, upper=M> mm; // municipality of each grid
  array[I] int<lower=1, upper=H> hh; // neighbourhood of each grid
  array[I] int<lower=0, upper=J1> jj1; // tower 1 coverage area of each grid
  array[I] int<lower=0, upper=J2> jj2; // tower 2 coverage area of each grid
  array[M] int<lower=1, upper=G> gov_of_mun; // governorate of each municipality
  array[H] int<lower=1, upper=M> mun_of_nbr; // municipality of each neighbourhood
  array[G] int<lower=0> I_g; // number of grids in each region
  array[M] int<lower=0> I_m; // number of grids in each region
  array[H] int<lower=0> I_h; // number of grids in each region
  array[J1] int<lower=0> I_j1; // number of grids in each tower 1 coverage area
  array[J2] int<lower=0> I_j2; // number of grids in each tower 2 coverage area
  array[G, max(I_g)] int<lower=0> grids_by_gov;
  array[M, max(I_m)] int<lower=0> grids_by_mun;
  array[H, max(I_h)] int<lower=0> grids_by_nbr;
  array[J1, max(I_j1)] int<lower=0> grids_by_tower1;
  array[J2, max(I_j2)] int<lower=0> grids_by_tower2;
  array[J1] int<lower=0> y1; // number of active subscribers on each tower
  array[J2] int<lower=0> y2; // number of active subscribers on each tower
  int<lower=0, upper=J1> N1_obs; // number of observed towers from provider 1
  int<lower=0, upper=J2> N2_obs; // number of observed towers from provider 2
  array[N1_obs] int<lower=1, upper=J1> idx1_obs; // observed provider 1 tower indices
  array[N2_obs] int<lower=1, upper=J2> idx2_obs; // observed provider 2 tower indices
  array[N1_obs] int<lower=0> y1_obs; // observed subscribers on provider 1 towers
  array[N2_obs] int<lower=0> y2_obs; // observed subscribers on provider 2 towers
  matrix[J1, I] d1; // distance from provider 1 towers to grids
  matrix[J2, I] d2; // distance from provider 2 towers to grids
  matrix[I, K] X; // standardized grid-level covariates
  vector<lower=0>[I] tents; // number of tents in each grid
  vector<lower=0>[I] housing; // number of housing units in each grid
}
parameters {
  real<lower=0> kappa1; // overdispersion for provider 1
  real<lower=0> kappa2; // overdispersion for provider 2
  
  real alpha_rho1; // log detection rate intercept for provider 1
  real alpha_rho2; // log detection rate intercept for provider 2
  real<lower=0> sigma_rho1; // tower-level sd for provider 1 detection
  real<lower=0> sigma_rho2; // tower-level sd for provider 2 detection
  real<lower=0> radius_rho1; // provider 1 distance radius
  real<lower=0> radius_rho2; // provider 2 distance radius
  vector[J1] z_rho1; // tower-level detection effects for provider 1
  vector[J2] z_rho2; // tower-level detection effects for provider 2
  
  real alpha_phi_tents; // log people per tent (intercept)
  real<lower=0> sigma_gov_phi_tents; // log people per tent (governorate effects)
  real<lower=0> sigma_mun_phi_tents; // log people per tent (municipality effects)
  vector[G] z_gov_phi_tents; // log people per tent (governorate effects)
  vector[M] z_mun_phi_tents; // log people per tent (municipality effects)
  vector[K] beta_tents; // grid-level covariate effects on tents
  
  real alpha_phi_housing; // log people per housing unit (intercept)
  real<lower=0> sigma_gov_phi_housing; // log people per housing unit (governorate effects)
  real<lower=0> sigma_mun_phi_housing; // log people per housing unit (municipality effects)
  vector[G] z_gov_phi_housing; // log people per housing unit (governorate effects)
  vector[M] z_mun_phi_housing; // log people per housing unit (municipality effects)
  vector[K] beta_housing; // grid-level covariate effects on housing
}
transformed parameters {
  // detection rate at zero distance on each tower
  vector<lower=0>[J1] rho1;
  vector<lower=0>[J2] rho2;
  rho1 = exp(alpha_rho1 + sigma_rho1 * z_rho1);
  rho2 = exp(alpha_rho2 + sigma_rho2 * z_rho2);
  
  // people per tent
  vector[G] gov_phi_tents; // (governorate effects)
  gov_phi_tents = sigma_gov_phi_tents * z_gov_phi_tents;
  
  vector[M] mun_phi_tents; // (municipality effects)
  for (m in 1 : M) {
    mun_phi_tents[m] = gov_phi_tents[gov_of_mun[m]]
                       + sigma_mun_phi_tents * z_mun_phi_tents[m];
  }
  
  vector<lower=0>[I] phi_tents; // people per tent
  phi_tents = exp(alpha_phi_tents + mun_phi_tents[mm] + X * beta_tents);
  
  // people per housing unit
  vector[G] gov_phi_housing; // (governorate effects)
  gov_phi_housing = sigma_gov_phi_housing * z_gov_phi_housing;
  
  vector[M] mun_phi_housing; // (municipality effects)
  for (m in 1 : M) {
    mun_phi_housing[m] = gov_phi_housing[gov_of_mun[m]]
                         + sigma_mun_phi_housing * z_mun_phi_housing[m];
  }
  
  vector<lower=0>[I] phi_housing; // people per housing unit
  phi_housing = exp(
                    alpha_phi_housing + mun_phi_housing[mm]
                    + X * beta_housing);
  
  // population in each grid
  vector<lower=0>[I] N;
  N = tents .* phi_tents + housing .* phi_housing;
  
  // total population size
  real<lower=0> sum_N;
  sum_N = sum(N);
  
  // population in each tower coverage area with within-catchment distance decay
  vector<lower=0>[J1] N_tower1;
  vector<lower=0>[J2] N_tower2;
  for (j in 1 : J1) {
    real weighted_sum = 0;
    for (k in 1 : I_j1[j]) {
      int i = grids_by_tower1[j, k];
      real w = inv_logit((radius_rho1 - d1[j, i]) / s_rho);
      weighted_sum += N[i] * w;
    }
    N_tower1[j] = weighted_sum;
  }
  for (j in 1 : J2) {
    real weighted_sum = 0;
    for (k in 1 : I_j2[j]) {
      int i = grids_by_tower2[j, k];
      real w = inv_logit((radius_rho2 - d2[j, i]) / s_rho);
      weighted_sum += N[i] * w;
    }
    N_tower2[j] = weighted_sum;
  }
  
  // expected number of active subscribers on each tower
  vector[J1] mu_y1;
  vector[J2] mu_y2;
  mu_y1 = N_tower1 .* rho1;
  mu_y2 = N_tower2 .* rho2;
}
model {
  //--- likelihoods ---//
  y1_obs ~ neg_binomial_2(mu_y1[idx1_obs], kappa1);
  y2_obs ~ neg_binomial_2(mu_y2[idx2_obs], kappa2);
  
  //--- priors ---//
  
  // population
  sum_N ~ lognormal(log(N_tot), 0.01 / 2);
  
  // subscribers
  kappa1 ~ lognormal(log(10), 1);
  kappa2 ~ lognormal(log(10), 1);
  
  // penetration
  alpha_rho1 ~ normal(log(0.4), 0.5);
  alpha_rho2 ~ normal(log(0.2), 0.5);
  z_rho1 ~ std_normal();
  z_rho2 ~ std_normal();
  sigma_rho1 ~ cauchy(0, 1);
  sigma_rho2 ~ cauchy(0, 1);
  radius_rho1 ~ lognormal(log(3000), 0.5);
  radius_rho2 ~ lognormal(log(3000), 0.5);
  
  // people per tent
  alpha_phi_tents ~ normal(log(10), 1);
  z_gov_phi_tents ~ std_normal();
  z_mun_phi_tents ~ std_normal();
  sigma_gov_phi_tents ~ cauchy(0, 1);
  sigma_mun_phi_tents ~ cauchy(0, 1);
  beta_tents ~ normal(0, 0.5);
  
  // people per housing unit
  alpha_phi_housing ~ normal(log(10), 1);
  z_gov_phi_housing ~ std_normal();
  z_mun_phi_housing ~ std_normal();
  sigma_gov_phi_housing ~ cauchy(0, 1);
  sigma_mun_phi_housing ~ cauchy(0, 1);
  beta_housing ~ normal(0, 0.5);
}
generated quantities {
  array[J1] int y1_rep; // posterior predictive for number of active subscribers on each tower
  array[J2] int y2_rep; // posterior predictive for number of active subscribers on each tower
  
  for (j in 1 : J1) {
    y1_rep[j] = neg_binomial_2_rng(mu_y1[j], kappa1);
  }
  for (j in 1 : J2) {
    y2_rep[j] = neg_binomial_2_rng(mu_y2[j], kappa2);
  }
}
