seed <- round(runif(1, 0, 1e9))
set.seed(seed)

data_dir <- file.path(env$wd, "out", "data")

model_data <- function(covariate_rasters = NULL, ...) {
  md <- build_model_data(
    covariate_rasters = covariate_rasters,
    seed = seed,
    ...
  )

  select_md_fields(
    md,
    c(
      "I", "G", "M", "H", "J1", "J2",
      "I_g", "I_m", "I_h", "I_j1", "I_j2",
      "grids_by_gov", "grids_by_mun", "grids_by_nbr",
      "grids_by_tower1", "grids_by_tower2",
      "gg", "mm", "hh", "jj1", "jj2",
      "gov_of_mun", "mun_of_nbr",
      "mun_ids", "nbr_ids",
      "tents", "housing",
      "N_tot",
      "tower1_id", "tower2_id",
      "y1", "y2",
      "mastergrid_idx", "seed"
    )
  )
}

init_generator <- function(md) {
  list(
    kappa1 = exp(rnorm(1, log(10), 0.1)),
    kappa2 = exp(rnorm(1, log(10), 0.1)),
    rho1 = exp(rnorm(1, log(0.4), 0.1)),
    rho2 = exp(rnorm(1, log(0.2), 0.1)),
    alpha_phi_tents = rnorm(1, log(10), 0.1),
    sigma_gov_phi_tents = exp(rnorm(1, log(0.05), 0.1)),
    sigma_mun_phi_tents = exp(rnorm(1, log(0.05), 0.1)),
    z_gov_phi_tents = rnorm(md$G, 0, 0.1),
    z_mun_phi_tents = rnorm(md$M, 0, 0.1),
    alpha_phi_housing = rnorm(1, log(10), 0.1),
    sigma_gov_phi_housing = exp(rnorm(1, log(0.05), 0.1)),
    sigma_mun_phi_housing = exp(rnorm(1, log(0.05), 0.1)),
    z_gov_phi_housing = rnorm(md$G, 0, 0.1),
    z_mun_phi_housing = rnorm(md$M, 0, 0.1)
  )
}
