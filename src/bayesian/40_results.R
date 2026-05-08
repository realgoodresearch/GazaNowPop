# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(dplyr)
library(terra)
library(sf)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)
source(here::here("src", "bayesian", "00_fun.R"))

# working directory
dir.create(env$wd, showWarnings = FALSE, recursive = TRUE)
setwd(env$wd)

#---- USER OPTIONS ----#

# reference date
reference_date <- as.Date("2026-03-24")

# model name
model_name <- "v0.12"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name
reference_date <- if (length(args) >= 2) as.Date(args[[2]]) else reference_date

#----------------------#

log_message("Starting results", model_name)

data_dir <- file.path(env$wd, "out", "data")
model_dir <- file.path(env$wd, "out", "bayesian", model_name, reference_date)
mcmc_dir <- file.path(model_dir, "mcmc")
out_dir <- file.path(model_dir, "results")
supp_dir <- file.path(out_dir, "supplementary_data")

dir.create(supp_dir, showWarnings = FALSE, recursive = TRUE)

fit <- readRDS(file.path(mcmc_dir, "fit.rds"))
md <- readRDS(file.path(mcmc_dir, "md.rds"))
log_message("Loaded fit and model data", model_name)

mastergrid <- rast(file.path(data_dir, "mastergrid.tif"))
gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"), quiet = TRUE)

mun_grid <- rast(file.path(data_dir, "mun_grid.tif"))
mun_geo <- st_read(file.path(data_dir, "mun_geo.gpkg"), quiet = TRUE)

nbr_grid <- rast(file.path(data_dir, "nbr_grid.tif"))
nbr_geo <- st_read(file.path(data_dir, "nbr_geo.gpkg"), quiet = TRUE)

grid_admin_lookup <- build_grid_admin_lookup(md, gov_grid, mun_grid, nbr_grid)

pop_draws <- fit$draws(
  variables = c(
    paste0("N[", seq_len(md$I), "]"),
    paste0("phi_tents[", seq_len(md$I), "]"),
    paste0("phi_housing[", seq_len(md$I), "]")
  ),
  format = "draws_df"
)

n_draws <- as.matrix(pop_draws %>% select(starts_with("N[")))
phi_tents_draws <- as.matrix(pop_draws %>% select(starts_with("phi_tents[")))
phi_housing_draws <- as.matrix(pop_draws %>% select(starts_with("phi_housing[")))
pop_in_tents_draws <- sweep(phi_tents_draws, 2, md$tents, `*`)
pop_in_bldgs_draws <- sweep(phi_housing_draws, 2, md$housing, `*`)

write_draw_mean_raster(
  mastergrid,
  md$mastergrid_idx,
  colMeans(n_draws),
  file.path(supp_dir, paste0("pop_grid_", reference_date, ".tif"))
)
write_draw_mean_raster(
  mastergrid,
  md$mastergrid_idx,
  colMeans(pop_in_tents_draws),
  file.path(supp_dir, paste0("pop_in_tents_", reference_date, ".tif"))
)
write_draw_mean_raster(
  mastergrid,
  md$mastergrid_idx,
  colMeans(pop_in_bldgs_draws),
  file.path(supp_dir, paste0("pop_in_bldgs_", reference_date, ".tif"))
)
log_message("Wrote population rasters", model_name)

write_admin_summary <- function(level, admin_col, admin_sf, stem) {
  total_sf <- summarise_admin_draws(
    n_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "population"
  )
  tents_sf <- summarise_admin_draws(
    pop_in_tents_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "pop_in_tents"
  )
  bldgs_sf <- summarise_admin_draws(
    pop_in_bldgs_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "pop_in_bldgs"
  )
  prop_tents_sf <- summarise_admin_ratio_draws(
    pop_in_tents_draws,
    n_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "perc_in_tents"
  )
  prop_bldgs_sf <- summarise_admin_ratio_draws(
    pop_in_bldgs_draws,
    n_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "perc_in_bldgs"
  )

  key_cols <- admin_key_columns(level)
  csv_cols <- admin_csv_columns(level)

  out_sf <- total_sf %>%
    left_join(
      st_drop_geometry(tents_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    ) %>%
    left_join(
      st_drop_geometry(bldgs_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    ) %>%
    left_join(
      st_drop_geometry(prop_tents_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    ) %>%
    left_join(
      st_drop_geometry(prop_bldgs_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    )

  write.csv(
    st_drop_geometry(out_sf) %>%
      select(
        all_of(csv_cols),
        date,
        population,
        pop_lower,
        pop_upper,
        starts_with("pop_in_tents"),
        starts_with("pop_in_bldgs"),
        starts_with("perc_in_tents"),
        starts_with("perc_in_bldgs")
      ),
    file.path(supp_dir, paste0(stem, "_", reference_date, ".csv")),
    row.names = FALSE
  )

  st_write(
    out_sf %>%
      select(
        all_of(key_cols),
        date,
        population,
        pop_lower,
        pop_upper,
        starts_with("pop_in_tents"),
        starts_with("pop_in_bldgs"),
        starts_with("perc_in_tents"),
        starts_with("perc_in_bldgs")
      ),
    file.path(supp_dir, paste0(stem, "_", reference_date, ".gpkg")),
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

write_admin_summary("gov", "gov_id", gov_geo, "pop_gov")
log_message("Wrote governorate results", model_name)

write_admin_summary("mun", "mun_id", mun_geo, "pop_mun")
log_message("Wrote municipality results", model_name)

write_admin_summary("nbr", "nbr_id", nbr_geo, "pop_nbr")
log_message("Wrote neighbourhood results", model_name)

log_message("Finished results", model_name)
