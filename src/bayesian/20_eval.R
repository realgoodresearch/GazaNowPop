# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(here)

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

#---- load data ----#
model_name <- "v0.01a"
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
  "psi_tents",
  "psi_housing",
  "bias_tents",
  "prec_tents",
  "alpha_psi",
  "sigma_beta_psi",
  "sigma_gamma_psi",
  "sigma_delta_psi",
  "kappa_psi",
  paste0("beta_psi[", 1:md$G, "]")
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
