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

# working directory
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

# model name
model_name <- "v0.03"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

# directories
in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src", "bayesian")
out_dir <- file.path(getwd(), "out", "bayesian", model_name, "mcmc")
dir.create(out_dir, showWarnings = F, recursive = T)

# source model config
source(file.path(src_dir, "10_mcmc_fun.R"))
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))

# model data
md <- model_data()

# save model data to disk
saveRDS(md, file = file.path(out_dir, "md.rds"))

# MCMC configuration
chains <- 4
warmup <- 1000
samples <- 1000
inits <- lapply(1:chains, function(id) init_generator(md = md))

# compile the stan model
mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))

# run MCMC
fit <- mod$sample(
  data = md,
  parallel_chains = chains,
  init = inits,
  iter_sampling = samples,
  iter_warmup = warmup,
  save_warmup = TRUE,
  seed = md$seed
)

# save fitted model to disk
fit$save_object(file = file.path(out_dir, "fit.rds"))
