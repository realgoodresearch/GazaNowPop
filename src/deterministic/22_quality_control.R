# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2025-10-26"
#----------------------#

# load libraries
library(dplyr)
library(terra)
library(sf)

# encoding for Arabic
Sys.setlocale("LC_ALL", "en_US.UTF-8") # Adjust for your OS

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src", "deterministic")
results_dir <- file.path(
  getwd(),
  "out",
  "deterministic",
  "results",
  reference_date
)
out_dir <- file.path(
  results_dir,
  "quality_control"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# load data
val_dat <- read.csv(file.path(
  in_dir,
  "quality_control",
  "validation_20251026.csv"
))

ref_dat <- st_read(file.path(
  results_dir,
  "supplementary_data",
  paste0("pop_nbr_", reference_date, ".gpkg")
))

val_result <- ref_dat %>%
  left_join(
    val_dat %>%
      select(ADM4_PCODE, population_validation) %>%
      mutate(
        ADM4_PCODE = as.character(ADM4_PCODE),
        population_validation = as.numeric(population_validation) / 1.000
      )
  )
head(val_result)

# rescale validation to sum to total population (only scale where pop_val != pop)
i_eq <- round(val_result$population) == round(val_result$population_validation)
scale_factor <- (sum(val_result$population) -
  sum(val_result$population_validation[i_eq])) /
  sum(val_result$population_validation[!i_eq])

val_result$population_validation_scaled[
  i_eq
] <- val_result$population_validation[i_eq]
val_result$population_validation_scaled[
  !i_eq
] <- val_result$population_validation[!i_eq] * scale_factor

sum(val_result$population_validation_scaled)

val_result <- val_result %>%
  mutate(
    population_diff = population - population_validation,
    population_perc_diff = population_diff / population_validation,
    population_diff_scaled = population - population_validation_scaled,
    population_perc_diff_scaled = population_diff_scaled /
      population_validation_scaled
  )

# save to disk
st_write(
  val_result,
  file.path(out_dir, paste0("pop_val_nbr_", reference_date, ".gpkg")),
  append = FALSE
)
