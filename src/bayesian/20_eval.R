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

# cores for parallel processing
ncores <- 8

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
out_dir <- file.path(env$wd, "out", "bayesian")

# model name
model_name <- "v0.01"
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name

#---- load data ----#
fit <- readRDS(file.path(out_dir, model_name, "mcmc", "fit.rds"))
md <- readRDS(file.path(out_dir, model_name, "mcmc", "md.rds"))
mastergrid <- rast(file.path(env$wd, "out", "data", "mastergrid.tif"))

model_out_dir <- file.path(out_dir, model_name, "eval")
dir.create(model_out_dir, showWarnings = F, recursive = T)

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

# mcmc_trace(draws, pars = "sum_N")
# mcmc_trace(draws, pars = "phi")
# mcmc_trace(draws, pars = "psi_tents")
# mcmc_trace(draws, pars = "psi_housing")

# i <- sample(1:md$I, 1)
# mcmc_trace(draws, pars = paste0("psi_tents[", i, "]"))
# mcmc_trace(draws, pars = paste0("psi_housing[", i, "]"))

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
    tower = as.integer(tower),
    mu_y = mu_y1,
    y_rep = y1_rep,
    y_obs = md$y1[tower]
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
    tower = as.integer(tower),
    mu_y = mu_y2,
    y_rep = y2_rep,
    y_obs = md$y2[tower]
  )

pred_df <- bind_rows(pred_y1, pred_y2)

pred_summary <- pred_df %>%
  group_by(provider, tower, y_obs) %>%
  summarise(
    mu_y = mean(mu_y),
    y_rep_mean = mean(y_rep),
    y_rep_lower = quantile(y_rep, 0.025),
    y_rep_upper = quantile(y_rep, 0.975),
    .groups = "drop"
  )

coverage <- mean(
  pred_summary$y_obs >= pred_summary$y_rep_lower &
    pred_summary$y_obs <= pred_summary$y_rep_upper
)

rmse <- sqrt(mean((pred_summary$y_rep_mean - pred_summary$y_obs)^2))
r <- cor(pred_summary$y_obs, pred_summary$y_rep_mean)

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
