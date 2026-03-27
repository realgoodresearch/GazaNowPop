# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(here)
library(terra)
library(tidyr)
library(ggplot2)

# load environment
env <- new.env()
source(here::here(".env"), local = env)
source(here::here("src", "bayesian", "00_fun.R"))

# cores for parallel processing
ncores <- 4

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
out_dir <- file.path(env$wd, "out", "bayesian")

# model name
model_name <- "v0.08"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

log_message("Starting eval", model_name)

#---- load data ----#
fit <- readRDS(file.path(out_dir, model_name, "mcmc", "fit.rds"))
md <- readRDS(file.path(out_dir, model_name, "mcmc", "md.rds"))
mastergrid <- rast(file.path(env$wd, "out", "data", "mastergrid.tif"))
log_message("Loaded fit, model data, and mastergrid", model_name)

model_out_dir <- file.path(out_dir, model_name, "eval")
dir.create(model_out_dir, showWarnings = F, recursive = T)

fit_summary <- fit$summary(.cores = ncores)
parameter_summary <- fit_summary %>%
  filter(
    !grepl("^(N|phi_tents|phi_housing|mu_y1|mu_y2|y1_rep|y2_rep)\\[", variable)
  )

write.csv(
  parameter_summary,
  file.path(model_out_dir, "parameter_summary.csv"),
  row.names = FALSE
)
log_message("Wrote parameter summary", model_name)

#---- traceplots ----#
draws <- fit$draws()
pars <- variables(draws)
pars_select <- c(
  "sum_N",
  "rho1",
  "rho2",
  "phi",
  "phi_tents",
  "phi_housing",
  "alpha_phi_tents",
  "sigma_nbr_phi_tents",
  "sigma_mun_phi_tents",
  "sigma_gov_phi_tents",
  "alpha_phi_housing",
  "sigma_nbr_phi_housing",
  "sigma_mun_phi_housing",
  "sigma_gov_phi_housing",
  "sigma_rho1",
  "sigma_rho2",
  paste0("gov_phi_tents[", 1:md$G, "]"),
  paste0("gov_phi_housing[", 1:md$G, "]"),
  paste0("mun_phi_tents[", 1:md$M, "]"),
  paste0("mun_phi_housing[", 1:md$M, "]"),
  paste0("nbr_phi_tents[", 1:md$H, "]"),
  paste0("nbr_phi_housing[", 1:md$H, "]")
)
pars <- pars[pars %in% pars_select]

par_groups <- split(pars, ceiling(seq_along(pars) / 16))


pdf(
  file.path(model_out_dir, "traceplots.pdf"),
  width = 11,
  height = 8.5
)

for (grp in par_groups) {
  p <- mcmc_trace(
    draws,
    pars = grp,
    facet_args = list(ncol = 4, nrow = 4)
  )
  print(p)
}

dev.off()
log_message("Wrote traceplots", model_name)

# mcmc_trace(draws, pars = "sum_N")
# mcmc_trace(draws, pars = "phi")
# mcmc_trace(draws, pars = "psi_tents")
# mcmc_trace(draws, pars = "psi_housing")

# i <- sample(1:md$I, 1)
# mcmc_trace(draws, pars = paste0("psi_tents[", i, "]"))
# mcmc_trace(draws, pars = paste0("psi_housing[", i, "]"))

#---- covariate effects ----#
covariate_names <- attr(md, "covariate_names")
if (!is.null(covariate_names)) {
  beta_tents_pars <- paste0("beta_tents[", seq_along(covariate_names), "]")
  beta_housing_pars <- paste0("beta_housing[", seq_along(covariate_names), "]")
  beta_pars <- c(beta_tents_pars, beta_housing_pars)
  beta_pars <- beta_pars[beta_pars %in% variables(draws)]

  if (length(beta_pars) > 0) {
    beta_draws <- as_draws_df(fit$draws(beta_pars)) %>%
      select(all_of(beta_pars)) %>%
      mutate(.draw = row_number()) %>%
      pivot_longer(
        cols = -.draw,
        names_to = c("process", "idx"),
        names_pattern = "beta_(tents|housing)\\[(\\d+)\\]",
        values_to = "value"
      ) %>%
      mutate(
        idx = as.integer(idx),
        covariate = covariate_names[idx],
        process = ifelse(process == "tents", "Tents", "Housing")
      )

    beta_summary <- beta_draws %>%
      group_by(process, covariate) %>%
      summarise(
        mean = mean(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(
        covariate = factor(
          covariate,
          levels = rev(unique(covariate_names))
        )
      )

    p_beta <- ggplot(
      beta_summary,
      aes(x = mean, y = covariate, xmin = lower, xmax = upper, color = process)
    ) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
      geom_pointrange(
        position = position_dodge(width = 0.5),
        fatten = 1.5
      ) +
      labs(
        x = "Posterior mean and 95% interval",
        y = NULL,
        color = NULL,
        title = paste("Covariate Effects -", model_name)
      ) +
      theme_minimal()

    ggsave(
      filename = file.path(model_out_dir, "covariate_effects.png"),
      plot = p_beta,
      width = 8,
      height = 5,
      dpi = 300
    )
    log_message("Wrote covariate effects plot", model_name)
  }
}

#---- tower-level detection rates ----#
rho1_pars <- grep("^rho1\\[", variables(draws), value = TRUE)
rho2_pars <- grep("^rho2\\[", variables(draws), value = TRUE)
rho_pars <- c(rho1_pars, rho2_pars)
has_decay <- !is.null(md$s_rho) &&
  all(c("radius_rho1", "radius_rho2") %in% variables(draws))

if (length(rho_pars) > 0) {
  rho_draws <- as_draws_df(fit$draws(rho_pars)) %>%
    select(all_of(rho_pars)) %>%
    mutate(.draw = row_number()) %>%
    pivot_longer(
      cols = -.draw,
      names_to = c("provider", "tower"),
      names_pattern = "rho(1|2)\\[(\\d+)\\]",
      values_to = "rho"
    ) %>%
    mutate(
      provider = ifelse(provider == "1", "Provider 1", "Provider 2"),
      tower = as.integer(tower)
    )

  rho_summary <- rho_draws %>%
    group_by(provider, tower) %>%
    summarise(
      mean = mean(rho),
      lower = quantile(rho, 0.025),
      upper = quantile(rho, 0.975),
      .groups = "drop"
    )

  p_rho <- ggplot(
    rho_summary,
    aes(x = tower, y = mean, ymin = lower, ymax = upper)
  ) +
    geom_pointrange() +
    facet_wrap(~provider, scales = "free_x") +
    labs(
      x = "Tower index",
      y = if (has_decay) "Detection rate at d = 0" else "Detection rate",
      title = if (has_decay) {
        paste("Tower-Level Rho Intercepts -", model_name)
      } else {
        paste("Tower-Level Detection Rates -", model_name)
      }
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(model_out_dir, "tower_detection_rates.png"),
    plot = p_rho,
    width = 10,
    height = 6,
    dpi = 300
  )
  log_message("Wrote tower detection plot", model_name)
}

#---- provider-level rho decay ----#
decay_pars_alpha <- c("alpha_rho1", "alpha_rho2", "radius_rho1", "radius_rho2")
decay_pars_scalar <- c("rho1", "rho2", "radius_rho1", "radius_rho2")
has_decay_alpha <- !is.null(md$s_rho) &&
  all(decay_pars_alpha %in% variables(draws))
has_decay_scalar <- !is.null(md$s_rho) &&
  all(decay_pars_scalar %in% variables(draws))

if (has_decay_alpha || has_decay_scalar) {
  decay_pars <- if (has_decay_alpha) decay_pars_alpha else decay_pars_scalar

  decay_draws <- as_draws_df(fit$draws(decay_pars)) %>%
    select(all_of(decay_pars))

  max_distance <- 10000

  distance_grid <- seq(0, max_distance, length.out = 200)

  decay_curve <- bind_rows(
    lapply(
      c("1", "2"),
      function(provider_id) {
        radius_vals <- decay_draws[[paste0("radius_rho", provider_id)]]
        rho0_vals <- if (has_decay_alpha) {
          exp(decay_draws[[paste0("alpha_rho", provider_id)]])
        } else {
          decay_draws[[paste0("rho", provider_id)]]
        }

        do.call(
          rbind,
          lapply(
            distance_grid,
            function(dist) {
              rho_vals <- rho0_vals * plogis((radius_vals - dist) / md$s_rho)
              data.frame(
                provider = paste("Provider", provider_id),
                distance_m = dist,
                mean = mean(rho_vals),
                lower = quantile(rho_vals, 0.025),
                upper = quantile(rho_vals, 0.975)
              )
            }
          )
        )
      }
    )
  )

  p_decay <- ggplot(
    decay_curve,
    aes(
      x = distance_m,
      y = mean,
      ymin = lower,
      ymax = upper,
      color = provider,
      fill = provider
    )
  ) +
    geom_ribbon(alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    scale_x_continuous(limits = c(0, max_distance)) +
    labs(
      x = "Distance from tower (m)",
      y = "Detection rate",
      color = NULL,
      fill = NULL,
      title = paste("Provider-Level Rho Decay -", model_name)
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(model_out_dir, "rho_decay_curves.png"),
    plot = p_decay,
    width = 8,
    height = 5,
    dpi = 300
  )
  log_message("Wrote rho decay plot", model_name)
}

#---- pop raster ----#
N_hat <- as_draws_df(draws) %>%
  select(starts_with("N[")) %>%
  apply(2, mean)

N_rast <- mastergrid
N_rast[N_rast == 1] <- 0
N_rast[md$mastergrid_idx] <- N_hat

writeRaster(
  N_rast,
  file.path(model_out_dir, "N_hat.tif"),
  overwrite = TRUE
)
log_message("Wrote N_hat raster", model_name)

#---- in sample fit ----#
draws_df <- as_draws_df(fit$draws(c("mu_y1", "mu_y2", "y1_rep", "y2_rep")))

pred_y1 <- draws_df %>%
  select(.draw, starts_with("mu_y1["), starts_with("y1_rep[")) %>%
  pivot_longer(
    cols = -.draw,
    names_to = c(".value", "tower"),
    names_pattern = "(mu_y1|y1_rep)\\[(\\d+)\\]"
  ) %>%
  transmute(
    .draw = .draw,
    provider = 1L,
    tower_index = as.integer(tower),
    tower_id = if (!is.null(md$tower1_id)) {
      md$tower1_id[tower_index]
    } else {
      tower_index
    },
    mu_y = mu_y1,
    y_rep = y1_rep,
    y_obs = md$y1[tower_index]
  )

pred_y2 <- draws_df %>%
  select(.draw, starts_with("mu_y2["), starts_with("y2_rep[")) %>%
  pivot_longer(
    cols = -.draw,
    names_to = c(".value", "tower"),
    names_pattern = "(mu_y2|y2_rep)\\[(\\d+)\\]"
  ) %>%
  transmute(
    .draw = .draw,
    provider = 2L,
    tower_index = as.integer(tower),
    tower_id = if (!is.null(md$tower2_id)) {
      md$tower2_id[tower_index]
    } else {
      tower_index
    },
    mu_y = mu_y2,
    y_rep = y2_rep,
    y_obs = md$y2[tower_index]
  )

pred_df <- bind_rows(pred_y1, pred_y2)

pred_summary <- pred_df %>%
  group_by(provider, tower_index, tower_id, y_obs) %>%
  summarise(
    mu_y = mean(mu_y),
    y_rep_mean = mean(y_rep),
    y_rep_lower = quantile(y_rep, 0.025),
    y_rep_upper = quantile(y_rep, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    pred_obs_ratio = y_rep_mean / pmax(y_obs, 1),
    mu_obs_ratio = mu_y / pmax(y_obs, 1),
    overprediction = y_rep_mean - y_obs,
    percent_error = overprediction / pmax(y_obs, 1)
  )

write.csv(
  pred_summary,
  file.path(model_out_dir, "tower_prediction_summary.csv"),
  row.names = FALSE
)
log_message("Wrote tower prediction summary", model_name)


# prediction accuaracy mapped to tower catchments
write_tower_voronoi_predictions(
  pred_summary = pred_summary,
  model_name = model_name,
  model_out_dir = model_out_dir,
  env_wd = env$wd,
  output_stem = "in_sample_prediction_summary"
)
log_message("Wrote tower in-sample prediction geopackages", model_name)


# evaluation metrics as csv
coverage <- mean(
  pred_summary$y_obs >= pred_summary$y_rep_lower &
    pred_summary$y_obs <= pred_summary$y_rep_upper
)

rmse <- sqrt(mean((pred_summary$y_rep_mean - pred_summary$y_obs)^2))
r <- cor(pred_summary$y_obs, pred_summary$y_rep_mean)

eval_metrics <- data.frame(
  model_name = model_name,
  rmse = rmse,
  r = r,
  coverage = coverage
)

write.csv(
  eval_metrics,
  file.path(model_out_dir, "eval_metrics.csv"),
  row.names = FALSE
)
log_message("Wrote eval metrics", model_name)


# observed vs predicted plot
label_txt <- paste0(
  "RMSE = ",
  round(rmse, 1),
  "\nR = ",
  round(r, 2),
  "\nCoverage = ",
  round(100 * coverage, 1),
  "%"
)

p_pred <- ggplot(
  pred_summary,
  aes(x = y_obs, y = y_rep_mean, color = factor(provider))
) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = y_rep_lower, ymax = y_rep_upper), width = 0) +
  geom_point(size = 2, alpha = 0.8) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = label_txt,
    hjust = 1.1,
    vjust = -0.5,
    size = 4,
    color = "black"
  ) +
  scale_color_discrete(name = "Provider") +
  labs(
    x = "Observed subscribers",
    y = "Predicted subscribers",
    title = paste("Observed vs Predicted Tower Subscribers -", model_name)
  ) +
  theme_minimal()

ggsave(
  filename = file.path(model_out_dir, "observed_vs_predicted.png"),
  plot = p_pred,
  width = 8,
  height = 6,
  dpi = 300
)
log_message("Wrote observed vs predicted plot", model_name)

log_message("Finished eval", model_name)
