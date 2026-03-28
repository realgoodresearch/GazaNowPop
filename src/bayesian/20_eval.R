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
library(sf)
library(tidyr)
library(ggplot2)

# load environment
env <- new.env()
source(here::here(".env"), local = env)
source(here::here("src", "bayesian", "00_fun.R"))

#---- USER OPTIONS ----#

# reference date
reference_date <- as.Date("2026-03-24")

# cores for parallel processing
ncores <- 4

# model name
model_name <- "v0.11"

# command line arguments can override defaults
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name
reference_date <- if (length(args) >= 2) as.Date(args[[2]]) else reference_date

#----------------------#

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
out_dir <- file.path(env$wd, "out", "bayesian")

log_message("Starting eval", model_name)

#---- load data ----#
fit <- readRDS(file.path(
  out_dir,
  model_name,
  reference_date,
  "mcmc",
  "fit.rds"
))
md <- readRDS(file.path(out_dir, model_name, reference_date, "mcmc", "md.rds"))
mastergrid <- rast(file.path(env$wd, "out", "data", "mastergrid.tif"))
gov_grid <- rast(file.path(env$wd, "out", "data", "gov_grid.tif"))
gov_geo <- st_read(file.path(env$wd, "out", "data", "gov_geo.gpkg"), quiet = TRUE)
mun_grid <- rast(file.path(env$wd, "out", "data", "mun_grid.tif"))
mun_geo <- st_read(file.path(env$wd, "out", "data", "mun_geo.gpkg"), quiet = TRUE)
nbr_grid <- rast(file.path(env$wd, "out", "data", "nbr_grid.tif"))
nbr_geo <- st_read(file.path(env$wd, "out", "data", "nbr_geo.gpkg"), quiet = TRUE)
log_message("Loaded fit, model data, and mastergrid", model_name)

model_out_dir <- file.path(out_dir, model_name, reference_date, "eval")
dir.create(model_out_dir, showWarnings = F, recursive = T)
supp_dir <- file.path(model_out_dir, "supplementary_data")
dir.create(supp_dir, showWarnings = FALSE, recursive = TRUE)

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
  "alpha_rho1",
  "alpha_rho2",
  "radius_rho1",
  "radius_rho2",
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
  "sigma_rho1_out",
  "sigma_rho2_out",
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
rho_tower_summary <- NULL
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

  rho_tower_summary <- rho_summary %>%
    transmute(
      provider = ifelse(provider == "Provider 1", 1L, 2L),
      tower_index = tower,
      tower_id = ifelse(
        provider == 1L,
        md$tower1_id[tower_index],
        md$tower2_id[tower_index]
      ),
      rho_mean = mean,
      rho_lower = lower,
      rho_upper = upper
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
pop_draws <- as_draws_df(draws) %>%
  select(starts_with("N["), starts_with("phi_tents["), starts_with("phi_housing["))

N_hat <- pop_draws %>%
  select(starts_with("N[")) %>%
  apply(2, mean)

phi_tents_hat <- pop_draws %>%
  select(starts_with("phi_tents[")) %>%
  apply(2, mean)

phi_housing_hat <- pop_draws %>%
  select(starts_with("phi_housing[")) %>%
  apply(2, mean)

pop_in_tents_hat <- md$tents * phi_tents_hat
pop_in_bldgs_hat <- md$housing * phi_housing_hat

write_draw_mean_raster(
  mastergrid,
  md$mastergrid_idx,
  N_hat,
  file.path(model_out_dir, "N_hat.tif")
)
log_message("Wrote N_hat raster", model_name)

write_draw_mean_raster(
  mastergrid,
  md$mastergrid_idx,
  pop_in_tents_hat,
  file.path(model_out_dir, "pop_in_tents_hat.tif")
)
log_message("Wrote pop_in_tents raster", model_name)

write_draw_mean_raster(
  mastergrid,
  md$mastergrid_idx,
  pop_in_bldgs_hat,
  file.path(model_out_dir, "pop_in_bldgs_hat.tif")
)
log_message("Wrote pop_in_bldgs raster", model_name)

grid_admin_lookup <- build_grid_admin_lookup(md, gov_grid, mun_grid, nbr_grid)
n_draws <- as.matrix(pop_draws %>% select(starts_with("N[")))
phi_tents_draws <- as.matrix(pop_draws %>% select(starts_with("phi_tents[")))
phi_housing_draws <- as.matrix(pop_draws %>% select(starts_with("phi_housing[")))
pop_in_tents_draws <- sweep(phi_tents_draws, 2, md$tents, `*`)
pop_in_bldgs_draws <- sweep(phi_housing_draws, 2, md$housing, `*`)

write_eval_admin_summary <- function(level, admin_col, admin_sf, stem) {
  total_sf <- summarise_admin_draws(
    n_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "population"
  )
  tents_sf <- summarise_admin_draws(
    pop_in_tents_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "pop_in_tents"
  )
  bldgs_sf <- summarise_admin_draws(
    pop_in_bldgs_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "pop_in_bldgs"
  )
  prop_tents_sf <- summarise_admin_ratio_draws(
    pop_in_tents_draws,
    n_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "perc_in_tents"
  )
  prop_bldgs_sf <- summarise_admin_ratio_draws(
    pop_in_bldgs_draws,
    n_draws,
    grid_admin_lookup,
    admin_col,
    admin_sf,
    level,
    reference_date,
    prefix = "perc_in_bldgs"
  )

  key_cols <- admin_key_columns(level)
  csv_cols <- admin_csv_columns(level)

  out_sf <- total_sf %>%
    left_join(
      st_drop_geometry(tents_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    ) %>%
    left_join(
      st_drop_geometry(bldgs_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    ) %>%
    left_join(
      st_drop_geometry(prop_tents_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    ) %>%
    left_join(
      st_drop_geometry(prop_bldgs_sf) %>% select(-all_of(c(csv_cols, "date"))),
      by = "id"
    )

  write.csv(
    st_drop_geometry(out_sf) %>%
      select(
        all_of(csv_cols),
        date,
        population,
        pop_lower,
        pop_upper,
        starts_with("pop_in_tents"),
        starts_with("pop_in_bldgs"),
        starts_with("perc_in_tents"),
        starts_with("perc_in_bldgs")
      ),
    file.path(supp_dir, paste0(stem, "_", reference_date, ".csv")),
    row.names = FALSE
  )

  st_write(
    out_sf %>%
      select(
        all_of(key_cols),
        date,
        population,
        pop_lower,
        pop_upper,
        starts_with("pop_in_tents"),
        starts_with("pop_in_bldgs"),
        starts_with("perc_in_tents"),
        starts_with("perc_in_bldgs")
      ),
    file.path(supp_dir, paste0(stem, "_", reference_date, ".gpkg")),
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

write_eval_admin_summary("gov", "gov_id", gov_geo, "pop_gov")
write_eval_admin_summary("mun", "mun_id", mun_geo, "pop_mun")
write_eval_admin_summary("nbr", "nbr_id", nbr_geo, "pop_nbr")
log_message("Wrote admin population summaries", model_name)

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

ntower_pars <- c(
  grep("^N_tower1\\[", variables(draws), value = TRUE),
  grep("^N_tower2\\[", variables(draws), value = TRUE)
)
ntower_summary <- NULL

if (length(ntower_pars) > 0) {
  ntower_draws <- as_draws_df(fit$draws(ntower_pars)) %>%
    select(all_of(ntower_pars)) %>%
    mutate(.draw = row_number()) %>%
    pivot_longer(
      cols = -.draw,
      names_to = c("provider", "tower"),
      names_pattern = "N_tower(1|2)\\[(\\d+)\\]",
      values_to = "N_tower"
    ) %>%
    mutate(
      provider = as.integer(provider),
      tower_index = as.integer(tower)
    )

  ntower_summary <- ntower_draws %>%
    group_by(provider, tower_index) %>%
    summarise(
      N_tower_mean = mean(N_tower),
      N_tower_lower = quantile(N_tower, 0.025),
      N_tower_upper = quantile(N_tower, 0.975),
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
  pred_summary <- pred_summary %>%
    left_join(
      rho_tower_summary,
      by = c("provider", "tower_index", "tower_id")
    )
}

if (!is.null(ntower_summary)) {
  pred_summary <- pred_summary %>%
    left_join(
      ntower_summary,
      by = c("provider", "tower_index", "tower_id")
    ) %>%
    mutate(
      rho_needed = y_obs / pmax(N_tower_mean, 1),
      rho_fit_gap = rho_mean - rho_needed,
      rho_fit_ratio = rho_mean / pmax(rho_needed, 1e-6)
    )
}

other_provider_support <- bind_rows(
  lapply(seq_len(md$J1), function(j) {
    grids <- md$grids_by_tower1[j, seq_len(md$I_j1[j])]
    other_towers <- md$jj2[grids]
    other_towers <- other_towers[other_towers > 0]
    if (length(other_towers) == 0) {
      return(NULL)
    }

    overlap_share <- as.numeric(prop.table(table(other_towers)))
    overlap_tower_idx <- as.integer(names(table(other_towers)))

    tibble(
      provider = 1L,
      tower_index = j,
      tower_id = md$tower1_id[j],
      other_provider_overlap_y = sum(overlap_share * md$y2[overlap_tower_idx]),
      other_provider_overlap_towers = length(overlap_tower_idx),
      other_provider_max_overlap_share = max(overlap_share)
    )
  }),
  lapply(seq_len(md$J2), function(j) {
    grids <- md$grids_by_tower2[j, seq_len(md$I_j2[j])]
    other_towers <- md$jj1[grids]
    other_towers <- other_towers[other_towers > 0]
    if (length(other_towers) == 0) {
      return(NULL)
    }

    overlap_share <- as.numeric(prop.table(table(other_towers)))
    overlap_tower_idx <- as.integer(names(table(other_towers)))

    tibble(
      provider = 2L,
      tower_index = j,
      tower_id = md$tower2_id[j],
      other_provider_overlap_y = sum(overlap_share * md$y1[overlap_tower_idx]),
      other_provider_overlap_towers = length(overlap_tower_idx),
      other_provider_max_overlap_share = max(overlap_share)
    )
  })
)

pred_summary <- pred_summary %>%
  left_join(
    other_provider_support,
    by = c("provider", "tower_index", "tower_id")
  )

write.csv(
  pred_summary,
  file.path(model_out_dir, "tower_prediction_summary.csv"),
  row.names = FALSE
)
log_message("Wrote tower prediction summary", model_name)

if (all(c("rho_mean", "rho_needed") %in% names(pred_summary))) {
  p_rho_needed <- ggplot(
    pred_summary,
    aes(
      x = rho_needed,
      y = rho_mean,
      ymin = rho_lower,
      ymax = rho_upper,
      color = factor(provider)
    )
  ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray30"
    ) +
    geom_errorbar(width = 0) +
    geom_point(size = 2, alpha = 0.85) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      x = "Implied tower penetration y / N_tower",
      y = "Fitted tower penetration rho",
      color = "Provider",
      title = paste("Fitted vs Implied Tower Penetration -", model_name)
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(model_out_dir, "rho_fitted_vs_implied.png"),
    plot = p_rho_needed,
    width = 8,
    height = 6,
    dpi = 300
  )
  log_message("Wrote fitted vs implied rho plot", model_name)
}

if (all(c("rho_needed", "other_provider_overlap_y") %in% names(pred_summary))) {
  p_cross_provider <- ggplot(
    pred_summary,
    aes(
      x = other_provider_overlap_y,
      y = rho_needed,
      color = factor(provider),
      size = y_obs
    )
  ) +
    geom_point(alpha = 0.8) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      x = "Other-provider local subscriber support",
      y = "Implied tower penetration y / N_tower",
      color = "Provider",
      size = "Observed subscribers",
      title = paste(
        "Cross-Provider Support vs Implied Penetration -",
        model_name
      )
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(
      model_out_dir,
      "cross_provider_support_vs_rho_needed.png"
    ),
    plot = p_cross_provider,
    width = 8,
    height = 6,
    dpi = 300
  )
  log_message("Wrote cross-provider support plot", model_name)
}


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
