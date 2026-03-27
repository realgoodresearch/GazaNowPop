# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(posterior)
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

# source xval helper functions
source(file.path(src_dir, "03_fun_xval.R"))

# load saved xval artifacts
oos_draws <- readRDS(file.path(model_out_dir, "oos_draws.rds"))
fold_diagnostics <- read.csv(
  file.path(model_out_dir, "fold_diagnostics.csv")
)
xval_artifacts <- readRDS(file.path(model_out_dir, "xval_artifacts.rds"))
fit <- readRDS(file.path(getwd(), "out", "bayesian", model_name, "mcmc", "fit.rds"))
md <- readRDS(file.path(getwd(), "out", "bayesian", model_name, "mcmc", "md.rds"))

log_message("Loaded cross-validation artifacts and full fit", model_name)

# summarize out-of-sample predictions and fold-level metrics
oos_summary <- summarize_prediction_draws(oos_draws)
fold_metrics <- compute_prediction_metrics(oos_summary)

# join tower-level OOS errors back to Voronoi layers
rho_pars <- c(
  grep("^rho1\\[", fit$metadata()$model_params, value = TRUE),
  grep("^rho2\\[", fit$metadata()$model_params, value = TRUE)
)
rho_tower_summary <- NULL

if (length(rho_pars) > 0) {
  rho_draws <- posterior::as_draws_df(fit$draws(rho_pars)) %>%
    select(all_of(rho_pars)) %>%
    mutate(.draw = row_number()) %>%
    pivot_longer(
      cols = -.draw,
      names_to = c("provider", "tower"),
      names_pattern = "rho(1|2)\\[(\\d+)\\]",
      values_to = "rho"
    ) %>%
    mutate(
      provider = as.integer(provider),
      tower_index = as.integer(tower)
    )

  rho_tower_summary <- rho_draws %>%
    group_by(provider, tower_index) %>%
    summarise(
      rho_mean = mean(rho),
      rho_lower = quantile(rho, 0.025),
      rho_upper = quantile(rho, 0.975),
      .groups = "drop"
    ) %>%
    mutate(
      tower_id = ifelse(
        provider == 1L,
        md$tower1_id[tower_index],
        md$tower2_id[tower_index]
      )
    )
}

if (!is.null(rho_tower_summary)) {
  oos_summary <- oos_summary %>%
    left_join(
      rho_tower_summary,
      by = c("provider", "tower_index", "tower_id")
    )
}

# write tabular xval outputs
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

write_tower_voronoi_predictions(
  pred_summary = oos_summary,
  model_name = model_name,
  model_out_dir = model_out_dir,
  env_wd = getwd(),
  output_stem = "oos_prediction_summary"
)

# save a bundled results object for downstream analysis
saveRDS(
  list(
    model_name = model_name,
    k_folds = xval_artifacts$k_folds,
    fit_paths = xval_artifacts$fit_paths,
    md_paths = xval_artifacts$md_paths,
    fold_assignments = xval_artifacts$fold_assignments,
    oos_draws = oos_draws,
    oos_summary = oos_summary,
    fold_metrics = fold_metrics,
    fold_diagnostics = fold_diagnostics
  ),
  file = file.path(model_out_dir, "xval_results.rds")
)

# write standard xval plots
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

log_message("Finished cross-validation results", model_name)
