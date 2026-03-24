data {
  int I; // number of grids
  int G; // number of governorates
  int M; // number of municipalities
  int H; // number of neighbourHoods
  int J1; // number of towers from provider 1
  int J2; // number of towers from provider 2
  int N_tot; // total population size
  array[I] int<lower=1, upper=G> gg; // governorate of each grid
  array[I] int<lower=1, upper=M> mm; // municipality of each grid
  array[I] int<lower=1, upper=H> hh; // neighbourhood of each grid
  array[I] int<lower=0, upper=J1> jj1; // tower 1 coverage area of each grid
  array[I] int<lower=0, upper=J2> jj2; // tower 2 coverage area of each grid
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
  vector<lower=0>[I] tents; // number of tents in each grid
  vector<lower=0>[I] housing; // number of housing units in each grid
}
parameters {
  real<lower=0> kappa1; // overdispersion for provider 1
  real<lower=0> kappa2; // overdispersion for provider 2
  
  real<lower=0> rho1; // detection rate (penetration rate) for provider 1
  real<lower=0> rho2; // detection rate (penetration rate) for provider 2
  
  real<lower=0> phi; // people per tent/housing unit (assume the same for both)
}
transformed parameters {
  // population in each grid
  vector<lower=0>[I] N;
  N = (tents + housing) * phi;
  
  // total population size
  real<lower=0> sum_N;
  sum_N = sum(N);
  
  // population in each tower coverage area
  vector<lower=0>[J1] N_tower1;
  vector<lower=0>[J2] N_tower2;
  for (j in 1 : J1) {
    N_tower1[j] = sum(N[grids_by_tower1[j, 1 : I_j1[j]]]);
  }
  for (j in 1 : J2) {
    N_tower2[j] = sum(N[grids_by_tower2[j, 1 : I_j2[j]]]);
  }
  
  // expected number of active subscribers on each tower
  vector[J1] mu_y1;
  vector[J2] mu_y2;
  mu_y1 = N_tower1 * rho1;
  mu_y2 = N_tower2 * rho2;
}
model {
  //--- likelihoods ---//
  y1 ~ neg_binomial_2(mu_y1, kappa1);
  y2 ~ neg_binomial_2(mu_y2, kappa2);
  
  //--- priors ---//
  
  // population
  sum_N ~ lognormal(log(N_tot), 0.01 / 2);
  
  // subscibers
  kappa1 ~ lognormal(log(10), 1);
  kappa2 ~ lognormal(log(10), 1);
  
  // people per unit
  phi ~ lognormal(log(1), 0.5);
}
generated quantities {
  array[J1] int y1_rep; // posterior predictive for number of active subscribers on each tower
  array[J2] int y2_rep; // posterior predictive for number of active subscribers on each tower
  
  for (j in 1 : J1) {
    y1_rep[j] = neg_binomial_2_rng(mu_y1[j], kappa1);
    ;
  }
  for (j in 1 : J2) {
    y2_rep[j] = neg_binomial_2_rng(mu_y2[j], kappa2);
  }
}
