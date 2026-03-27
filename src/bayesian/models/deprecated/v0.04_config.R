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
    rho1 = runif(1, 0.25, 0.75),
    rho2 = runif(1, 0, 0.5),
    phi_tents = runif(md$I, 0, 20),
    phi_housing = runif(md$I, 0, 20)
  )
}
