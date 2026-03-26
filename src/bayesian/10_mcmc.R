# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)
source(here::here("src", "bayesian", "00_fun.R"))

# working directory
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

# model name
model_name <- "v0.09"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

log_message("Starting MCMC", model_name)

# directories
in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src", "bayesian")
out_dir <- file.path(getwd(), "out", "bayesian", model_name, "mcmc")
data_dir <- file.path(env$wd, "out", "data")
dir.create(out_dir, showWarnings = F, recursive = T)

# source model config
source(file.path(src_dir, "01_fun_mcmc.R"))
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))
log_message("Loaded model config", model_name)

# shared covariates applied to all models
covariate_rasters <- list(
  prop_bldg_destroyed = terra::rast(file.path(
    data_dir,
    "prop_bldg_destroyed_500m.tif"
  )),
  housing = terra::rast(file.path(
    data_dir,
    "housing_500m.tif"
  )),
  tents = terra::rast(file.path(
    data_dir,
    "tents_500m.tif"
  )),
  osm_building_coverage = terra::rast(file.path(
    data_dir,
    "osm_building_coverage_500m.tif"
  )),
  flood_reports = terra::rast(file.path(
    data_dir,
    "flood_reports_500m.tif"
  )),
  storm_vulnerability = terra::rast(file.path(
    data_dir,
    "storm_vulnerability_500m.tif"
  )),
  evac_order_count = terra::rast(file.path(
    data_dir,
    "evac_order_count_500m.tif"
  ))
)

# model data
md <- model_data(covariate_rasters = covariate_rasters)
md$N1_obs <- md$J1
md$N2_obs <- md$J2
md$idx1_obs <- seq_len(md$J1)
md$idx2_obs <- seq_len(md$J2)
md$y1_obs <- as.integer(md$y1)
md$y2_obs <- as.integer(md$y2)
log_message("Built model data", model_name)

# save model data to disk
saveRDS(md, file = file.path(out_dir, "md.rds"))
log_message(
  paste0("Saved model data to ", file.path(out_dir, "md.rds")),
  model_name
)

# MCMC configuration
chains <- 4
warmup <- 1000
samples <- 1000
inits <- lapply(1:chains, function(id) init_generator(md = md))

# compile the stan model
log_message("Compiling Stan model", model_name)
mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))
log_message("Finished compiling Stan model", model_name)

# run MCMC
log_message("Starting sampling", model_name)
fit <- mod$sample(
  data = md,
  parallel_chains = chains,
  init = inits,
  iter_sampling = samples,
  iter_warmup = warmup,
  save_warmup = TRUE,
  seed = md$seed
)
log_message("Finished sampling", model_name)

# save fitted model to disk
fit$save_object(file = file.path(out_dir, "fit.rds"))
log_message(
  paste0("Saved fit object to ", file.path(out_dir, "fit.rds")),
  model_name
)

log_message("Finished MCMC", model_name)
