# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
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

#---- USER OPTIONS ----#

# reference date
reference_date <- as.Date("2026-03-24")

# model name
model_name <- "v0.11"

# command line arguments can override defaults
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name
reference_date <- if (length(args) >= 2) as.Date(args[[2]]) else reference_date

#----------------------#

log_message("Starting cross-validation", model_name)

# directories
src_dir <- file.path(here::here(), "src", "bayesian")
model_out_dir <- file.path(
  getwd(),
  "out",
  "bayesian",
  model_name,
  reference_date,
  "xval"
)
fold_out_dir <- file.path(model_out_dir, "folds")
dir.create(model_out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fold_out_dir, showWarnings = FALSE, recursive = TRUE)

# source helpers and model config
source(file.path(src_dir, "03_fun_xval.R"))
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))
log_message("Loaded helpers and model config", model_name)

# model data
md_full <- readRDS(file.path(
  getwd(),
  "out",
  "bayesian",
  model_name,
  reference_date,
  "mcmc",
  "md.rds"
))

log_message("Loaded full model data", model_name)

# cross-validation configuration
k_folds <- 5L
chains <- 4L
concurrent_folds <- 2L
warmup <- 1000L
samples <- 1000L

folds1 <- make_stratified_folds(md_full$J1, k = k_folds, seed = md_full$seed)
folds2 <- make_stratified_folds(
  md_full$J2,
  k = k_folds,
  seed = md_full$seed + 1L
)

fold_assignments <- bind_rows(
  tibble(
    provider = 1L,
    tower_index = seq_len(md_full$J1),
    tower_id = md_full$tower1_id,
    fold = NA_integer_
  ),
  tibble(
    provider = 2L,
    tower_index = seq_len(md_full$J2),
    tower_id = md_full$tower2_id,
    fold = NA_integer_
  )
)

for (fold_id in seq_len(k_folds)) {
  fold_assignments$fold[
    fold_assignments$provider == 1L &
      fold_assignments$tower_index %in% folds1[[fold_id]]
  ] <- fold_id

  fold_assignments$fold[
    fold_assignments$provider == 2L &
      fold_assignments$tower_index %in% folds2[[fold_id]]
  ] <- fold_id
}

write.csv(
  fold_assignments,
  file.path(model_out_dir, "fold_assignments.csv"),
  row.names = FALSE
)
log_message("Wrote fold assignments", model_name)

# compile the stan model once
log_message("Compiling Stan model", model_name)
mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))
log_message("Finished compiling Stan model", model_name)

run_fold <- function(fold_id) {
  log_message(paste0("Starting fold ", fold_id, " of ", k_folds), model_name)

  md_fold <- add_observation_mask(
    md_full,
    holdout1 = folds1[[fold_id]],
    holdout2 = folds2[[fold_id]],
    seed = md_full$seed + fold_id
  )

  md_path <- file.path(
    fold_out_dir,
    paste0("fold_", sprintf("%02d", fold_id), "_md.rds")
  )
  saveRDS(md_fold, file = md_path)

  inits <- lapply(seq_len(chains), function(id) init_generator(md = md_fold))

  fit <- mod$sample(
    data = md_fold,
    parallel_chains = chains,
    init = inits,
    iter_sampling = samples,
    iter_warmup = warmup,
    save_warmup = TRUE,
    seed = md_fold$seed
  )

  fit_path <- file.path(
    fold_out_dir,
    paste0("fold_", sprintf("%02d", fold_id), "_fit.rds")
  )
  fit$save_object(file = fit_path)

  oos_draws <- extract_oos_prediction_draws(
    fit,
    md_fold,
    fold_id
  )
  diagnostics <- extract_fit_diagnostics(fit, fold_id, summary_cores = chains)

  log_message(paste0("Finished fold ", fold_id, " of ", k_folds), model_name)

  list(
    oos_draws = oos_draws,
    diagnostics = diagnostics,
    fit_path = fit_path,
    md_path = md_path
  )
}

fold_results <- parallel::mclapply(
  seq_len(k_folds),
  run_fold,
  mc.cores = concurrent_folds,
  mc.preschedule = FALSE
)

oos_draws_list <- lapply(fold_results, `[[`, "oos_draws")
diagnostics_list <- lapply(fold_results, `[[`, "diagnostics")
fit_paths <- vapply(fold_results, `[[`, character(1), "fit_path")
md_paths <- vapply(fold_results, `[[`, character(1), "md_path")

oos_draws <- bind_rows(oos_draws_list)
fold_diagnostics <- bind_rows(diagnostics_list)

write.csv(
  fold_diagnostics,
  file.path(model_out_dir, "fold_diagnostics.csv"),
  row.names = FALSE
)

saveRDS(oos_draws, file = file.path(model_out_dir, "oos_draws.rds"))
saveRDS(
  list(
    model_name = model_name,
    k_folds = k_folds,
    fit_paths = fit_paths,
    md_paths = md_paths,
    fold_assignments = fold_assignments
  ),
  file = file.path(model_out_dir, "xval_artifacts.rds")
)
log_message("Saved cross-validation artifacts", model_name)
log_message("Finished cross-validation", model_name)
