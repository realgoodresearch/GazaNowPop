# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
baseline_date <- "2025-09-29"
baseline_date <- "2025-12-16"
reference_date <- "2026-01-25"
#----------------------#

# load libraries
library(dplyr)
library(terra)
library(sf)

# encoding for Arabic
Sys.setlocale("LC_ALL", "en_US.UTF-8") # Adjust for your OS

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

src_dir <- file.path(here::here(), "src")
data_dir <- file.path(getwd(), "out", "data")
model_dir <- file.path(getwd(), "out", "model")
results_dir <- file.path(getwd(), "out", "results")
out_dir <- file.path(
  results_dir,
  reference_date,
  "supplementary_data",
  paste0("pop_change_", gsub('-', '', baseline_date))
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# load functions
source(file.path(src_dir, "20_results_fun.R"))

# load data
pop_grid_reference <- rast(
  file.path(
    results_dir,
    reference_date,
    "supplementary_data",
    paste0("pop_grid_", reference_date, ".tif")
  )
)

pop_grid_baseline <- rast(
  file.path(
    results_dir,
    baseline_date,
    "supplementary_data",
    paste0("pop_grid_", baseline_date, ".tif")
  )
)

gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"))

mun_grid <- rast(file.path(data_dir, "mun_grid.tif"))
mun_geo <- st_read(file.path(data_dir, "mun_geo.gpkg"))

nbr_grid <- rast(file.path(data_dir, "nbr_grid.tif"))
nbr_geo <- st_read(file.path(data_dir, "nbr_geo.gpkg"))

#---- population change ----#
write(
  "Estimates of population change were calculated for the reference date relative to the baseline date defined below.\n",
  file.path(out_dir, "README.txt")
)

write(
  paste("Reference date:", reference_date),
  file.path(out_dir, "README.txt"),
  append = TRUE
)

write(
  paste("Baseline date:", baseline_date),
  file.path(out_dir, "README.txt"),
  append = TRUE
)


pop_grid_reference[is.na(pop_grid_reference) & !is.na(pop_grid_baseline)] <- 0
pop_grid_baseline[is.na(pop_grid_baseline) & !is.na(pop_grid_reference)] <- 0

pop_grid_delta <- pop_grid_reference - pop_grid_baseline
writeRaster(
  pop_grid_delta,
  file.path(out_dir, paste0("pop_grid_change_", reference_date, ".tif")),
  overwrite = TRUE
)

#---- summarise by admin unit ----#
# governorate
pop_gov <- summarise_grid_per_governorate(
  pop_ras = pop_grid_delta,
  gov_poly = gov_geo,
  gov_ras = gov_grid,
  ref_date = reference_date
)

st_write(
  pop_gov,
  file.path(
    out_dir,
    paste0("pop_change_gov_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_gov |> st_drop_geometry(),
  file.path(
    out_dir,
    paste0("pop_change_gov_", reference_date, ".csv")
  ),
  row.names = F
)

# municipality
pop_mun <- summarise_grid_per_municipality(
  pop_ras = pop_grid_delta,
  mun_poly = mun_geo,
  mun_ras = mun_grid,
  ref_date = reference_date
)

st_write(
  pop_mun,
  file.path(
    out_dir,
    paste0("pop_change_mun_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_mun |> st_drop_geometry(),
  file.path(
    out_dir,
    paste0("pop_change_mun_", reference_date, ".csv")
  ),
  row.names = F
)


# neighbourhood
pop_nbr <- summarise_grid_per_neighbourhood(
  pop_ras = pop_grid_delta,
  nbr_poly = nbr_geo,
  nbr_ras = nbr_grid,
  ref_date = reference_date
)

st_write(
  pop_nbr,
  file.path(
    out_dir,
    paste0("pop_change_nbr_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_nbr |> st_drop_geometry(),
  file.path(
    out_dir,
    paste0("pop_change_nbr_", reference_date, ".csv")
  ),
  row.names = F
)
