# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
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

data_dir <- file.path(getwd(), "out", "telecom_towers_deterministic", "data")
model_dir <- file.path(getwd(), "out", "telecom_towers_deterministic", "model")
out_dir <- file.path(getwd(), "out", "telecom_towers_deterministic", "timeseries")
dir.create(out_dir, showWarnings = F, recursive = T)

# load data
gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"))

mun_grid <- rast(file.path(data_dir, "mun_grid.tif"))
mun_geo <- st_read(file.path(data_dir, "mun_geo.gpkg"))

nbr_grid <- rast(file.path(data_dir, "nbr_grid.tif"))
nbr_geo <- st_read(file.path(data_dir, "nbr_geo.gpkg"))

lf <- list.files(file.path(model_dir, "telecom_population"))
dates <- sub("^.*_(.*)\\.tif$", "\\1", lf)
pop_grids <- list()
for (i in 1:length(lf)) {
  pop_grids[[dates[i]]] <- rast(file.path(model_dir, "telecom_population", paste0("population_", dates[[i]], ".tif")))
}

# gridded population change
delta_dir <- file.path(out_dir, "delta_rasters")
dir.create(delta_dir, recursive = T, showWarnings = F)

for (i in 2:length(pop_grids)) {
  delta_rast <- pop_grids[[i]] - pop_grids[[i - 1]]
  writeRaster(delta_rast, file.path(delta_dir, paste0("delta_pop_", dates[[i]], ".tif")), overwrite = TRUE)
}
