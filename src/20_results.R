# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2025-11-02"
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
model_dir <- file.path(getwd(), "out", "model", reference_date)
out_dir <- file.path(getwd(), "out", "results", reference_date)

dir.create(
  file.path(out_dir, "supplementary_data"),
  showWarnings = F,
  recursive = T
)

# load functions
source(file.path(src_dir, "20_results_fun.R"))

# load data
pop_grid_current <- rast(file.path(
  model_dir,
  "population.tif"
))

gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"))

mun_grid <- rast(file.path(data_dir, "mun_grid.tif"))
mun_geo <- st_read(file.path(data_dir, "mun_geo.gpkg"))

nbr_grid <- rast(file.path(data_dir, "nbr_grid.tif"))
nbr_geo <- st_read(file.path(data_dir, "nbr_geo.gpkg"))


#---- reporting for "current" date ----#

# gridded population
file.copy(
  from = file.path(
    model_dir,
    "population.tif"
  ),
  to = file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_grid_", reference_date, ".tif")
  ),
  overwrite = TRUE
)

# governorate
pop_gov <- summarise_grid_per_governorate(
  pop_ras = pop_grid_current,
  gov_poly = gov_geo,
  gov_ras = gov_grid,
  ref_date = reference_date
)

st_write(
  pop_gov,
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_gov_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_gov |> st_drop_geometry(),
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_gov_", reference_date, ".csv")
  ),
  row.names = F
)

# municipality
pop_mun <- summarise_grid_per_municipality(
  pop_ras = pop_grid_current,
  mun_poly = mun_geo,
  mun_ras = mun_grid,
  ref_date = reference_date
)

st_write(
  pop_mun,
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_mun_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_mun |> st_drop_geometry(),
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_mun_", reference_date, ".csv")
  ),
  row.names = F
)


# neighbourhood
pop_nbr <- summarise_grid_per_neighbourhood(
  pop_ras = pop_grid_current,
  nbr_poly = nbr_geo,
  nbr_ras = nbr_grid,
  ref_date = reference_date
)

st_write(
  pop_nbr,
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_nbr_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_nbr |> st_drop_geometry(),
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_nbr_", reference_date, ".csv")
  ),
  row.names = F
)
