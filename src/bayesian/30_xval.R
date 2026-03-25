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

timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# working directory
dir.create(env$wd, showWarnings = FALSE, recursive = TRUE)
setwd(env$wd)

# model name
model_name <- "v0.09"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

cat("[", timestamp(), "] Starting cross-validation for ", model_name, "\n", sep = "")

# directories
src_dir <- file.path(here::here(), "src", "bayesian")
model_out_dir <- file.path(getwd(), "out", "bayesian", model_name, "xval")
fold_out_dir <- file.path(model_out_dir, "folds")
dir.create(model_out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fold_out_dir, showWarnings = FALSE, recursive = TRUE)

# source helpers and model config
source(file.path(src_dir, "10_mcmc_fun.R"))
source(file.path(src_dir, "30_xval_fun.R"))
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))
cat("[", timestamp(), "] Loaded helpers and model config for ", model_name, "\n", sep = "")

# model data
md_full <- model_data()
md_full$N1_obs <- md_full$J1
md_full$N2_obs <- md_full$J2
md_full$idx1_obs <- seq_len(md_full$J1)
md_full$idx2_obs <- seq_len(md_full$J2)
md_full$y1_obs <- as.integer(md_full$y1)
md_full$y2_obs <- as.integer(md_full$y2)

saveRDS(md_full, file = file.path(model_out_dir, "md_full.rds"))
cat("[", timestamp(), "] Built and saved full model data for ", model_name, "\n", sep = "")

# cross-validation configuration
k_folds <- 5L
chains <- 4L
warmup <- 1000L
samples <- 1000L

folds1 <- make_stratified_folds(md_full$J1, k = k_folds, seed = md_full$seed)
folds2 <- make_stratified_folds(md_full$J2, k = k_folds, seed = md_full$seed + 1L)

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
cat("[", timestamp(), "] Wrote fold assignments for ", model_name, "\n", sep = "")

# compile the stan model once
cat("[", timestamp(), "] Compiling Stan model for ", model_name, "\n", sep = "")
mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))
cat("[", timestamp(), "] Finished compiling Stan model for ", model_name, "\n", sep = "")

oos_draws_list <- vector("list", k_folds)
diagnostics_list <- vector("list", k_folds)
fit_paths <- character(k_folds)
md_paths <- character(k_folds)

for (fold_id in seq_len(k_folds)) {
  cat("[", timestamp(), "] Starting fold ", fold_id, " of ", k_folds, "\n", sep = "")

  md_fold <- add_observation_mask(
    md_full,
    holdout1 = folds1[[fold_id]],
    holdout2 = folds2[[fold_id]],
    seed = md_full$seed + fold_id
  )

  md_paths[[fold_id]] <- file.path(
    fold_out_dir,
    paste0("fold_", sprintf("%02d", fold_id), "_md.rds")
  )
  saveRDS(md_fold, file = md_paths[[fold_id]])

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

  fit_paths[[fold_id]] <- file.path(
    fold_out_dir,
    paste0("fold_", sprintf("%02d", fold_id), "_fit.rds")
  )
  fit$save_object(file = fit_paths[[fold_id]])

  oos_draws_list[[fold_id]] <- extract_oos_prediction_draws(fit, md_fold, fold_id)
  diagnostics_list[[fold_id]] <- extract_fit_diagnostics(fit, fold_id)

  cat("[", timestamp(), "] Finished fold ", fold_id, " of ", k_folds, "\n", sep = "")
}

oos_draws <- bind_rows(oos_draws_list)
oos_summary <- summarize_prediction_draws(oos_draws)
fold_metrics <- compute_prediction_metrics(oos_summary)
fold_diagnostics <- bind_rows(diagnostics_list)

write.csv(
  oos_summary,
  file.path(model_out_dir, "oos_prediction_summary.csv"),
  row.names = FALSE
)
write.csv(
  fold_metrics,
  file.path(model_out_dir, "fold_metrics.csv"),
  row.names = FALSE
)
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
    fold_assignments = fold_assignments,
    oos_draws = oos_draws,
    oos_summary = oos_summary,
    fold_metrics = fold_metrics,
    fold_diagnostics = fold_diagnostics
  ),
  file = file.path(model_out_dir, "xval_results.rds")
)
cat("[", timestamp(), "] Saved cross-validation outputs for ", model_name, "\n", sep = "")

p_oos <- make_oos_plot(oos_summary, model_name)
ggsave(
  filename = file.path(model_out_dir, "observed_vs_oos_predicted.png"),
  plot = p_oos,
  width = 8,
  height = 6,
  dpi = 300
)

p_rmse <- make_fold_metric_plot(fold_metrics, model_name)
ggsave(
  filename = file.path(model_out_dir, "oos_rmse_by_fold.png"),
  plot = p_rmse,
  width = 8,
  height = 5,
  dpi = 300
)

p_diag <- make_diagnostic_plot(fold_diagnostics, model_name)
ggsave(
  filename = file.path(model_out_dir, "fold_diagnostics.png"),
  plot = p_diag,
  width = 9,
  height = 6,
  dpi = 300
)
cat("[", timestamp(), "] Wrote cross-validation plots for ", model_name, "\n", sep = "")

cat("[", timestamp(), "] Finished cross-validation for ", model_name, "\n", sep = "")
