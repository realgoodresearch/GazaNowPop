# cleanup
rm(list = ls())
gc()

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)
source(here::here("src", "bayesian", "00_fun.R"))

# working directory
dir.create(env$wd, showWarnings = FALSE, recursive = TRUE)
setwd(env$wd)

# model name
model_name <- "v0.09"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

log_message("Starting cross-validation results", model_name)

# directories
src_dir <- file.path(here::here(), "src", "bayesian")
model_out_dir <- file.path(getwd(), "out", "bayesian", model_name, "xval")

# source helpers
source(file.path(src_dir, "30_xval_fun.R"))

oos_draws <- readRDS(file.path(model_out_dir, "oos_draws.rds"))
fold_diagnostics <- read.csv(
  file.path(model_out_dir, "fold_diagnostics.csv")
)
xval_artifacts <- readRDS(file.path(model_out_dir, "xval_artifacts.rds"))

log_message("Loaded cross-validation artifacts", model_name)

write_xval_results(
  model_name = model_name,
  model_out_dir = model_out_dir,
  oos_draws = oos_draws,
  fold_diagnostics = fold_diagnostics,
  fit_paths = xval_artifacts$fit_paths,
  md_paths = xval_artifacts$md_paths,
  fold_assignments = xval_artifacts$fold_assignments,
  k_folds = xval_artifacts$k_folds
)

log_message("Finished cross-validation results", model_name)
