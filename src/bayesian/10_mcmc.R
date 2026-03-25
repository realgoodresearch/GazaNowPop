# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(here)

timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# working directory
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

# model name
model_name <- "v0.09"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

cat("[", timestamp(), "] Starting MCMC for ", model_name, "\n", sep = "")

# directories
in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src", "bayesian")
out_dir <- file.path(getwd(), "out", "bayesian", model_name, "mcmc")
dir.create(out_dir, showWarnings = F, recursive = T)

# source model config
source(file.path(src_dir, "10_mcmc_fun.R"))
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))
cat("[", timestamp(), "] Loaded model config for ", model_name, "\n", sep = "")

# model data
md <- model_data()
cat("[", timestamp(), "] Built model data for ", model_name, "\n", sep = "")

# save model data to disk
saveRDS(md, file = file.path(out_dir, "md.rds"))
cat("[", timestamp(), "] Saved model data to ", file.path(out_dir, "md.rds"), "\n", sep = "")

# MCMC configuration
chains <- 4
warmup <- 1000
samples <- 1000
inits <- lapply(1:chains, function(id) init_generator(md = md))

# compile the stan model
cat("[", timestamp(), "] Compiling Stan model for ", model_name, "\n", sep = "")
mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))
cat("[", timestamp(), "] Finished compiling Stan model for ", model_name, "\n", sep = "")

# run MCMC
cat("[", timestamp(), "] Starting sampling for ", model_name, "\n", sep = "")
fit <- mod$sample(
  data = md,
  parallel_chains = chains,
  init = inits,
  iter_sampling = samples,
  iter_warmup = warmup,
  save_warmup = TRUE,
  seed = md$seed
)
cat("[", timestamp(), "] Finished sampling for ", model_name, "\n", sep = "")

# save fitted model to disk
fit$save_object(file = file.path(out_dir, "fit.rds"))
cat("[", timestamp(), "] Saved fit object to ", file.path(out_dir, "fit.rds"), "\n", sep = "")

cat("[", timestamp(), "] Finished MCMC for ", model_name, "\n", sep = "")
