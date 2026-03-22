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
  real<lower=0> rho1; // detection rate (penetration rate) for provider 1
  real<lower=0> rho2; // detection rate (penetration rate) for provider 2
  
  real<lower=0> phi_tents; // people per tent
  real<lower=0> phi_housing; // people per housing unit
}
transformed parameters {
  vector<lower=0>[I] N; // population in each grid
  real<lower=0> sum_N; // total population size
  
  N = tents * phi_tents + housing * phi_housing;
  sum_N = sum(N);
  
  vector<lower=0>[J1] N_tower1; // population in each tower 1 coverage area
  vector<lower=0>[J2] N_tower2; // population in each tower 2 coverage area
  for (j in 1 : J1) {
    N_tower1[j] = sum(N[grids_by_tower1[j, 1 : I_j1[j]]]);
  }
  for (j in 1 : J2) {
    N_tower2[j] = sum(N[grids_by_tower2[j, 1 : I_j2[j]]]);
  }
}
model {
  //--- likelihoods ---//
  y1 ~ poisson(N_tower1 * rho1);
  y2 ~ poisson(N_tower2 * rho2);
  
  //--- priors ---//
  
  // population
  sum_N ~ lognormal(log(N_tot), 0.01 / 2);
  
  // people per unit
  phi_tents ~ lognormal(log(1), 0.5);
  phi_housing ~ lognormal(log(1), 0.5);
}
