# !! WARNING!!
# This script overwrites existing files:
# out/deterministic/results/[reference_date]/supplementary_data/pop_gov_[reference_date].csv
# out/deterministic/results/[reference_date]/supplementary_data/pop_gov_[reference_date].gpkg

# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2026-05-04"
#----------------------#

# load libraries
library(dplyr)
library(terra)
library(sf)

# encoding for Arabic
set_utf8_locale <- function() {
  for (locale in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
    result <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (!is.na(result) && result != "") {
      return(invisible(result))
    }
  }
  warning("Could not set a UTF-8 locale; Arabic text may not render correctly.")
  invisible(NULL)
}
set_utf8_locale()

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src", "deterministic")
data_dir <- file.path(getwd(), "out", "data")
model_dir <- file.path(getwd(), "out", "deterministic", "model")
results_dir <- file.path(getwd(), "out", "deterministic", "results")
out_dir <- file.path(
  results_dir,
  reference_date,
  "supplementary_data"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# load functions
source(file.path(src_dir, "20_results_fun.R"))

#---- load data ----#
pop_grid <- rast(
  file.path(
    results_dir,
    reference_date,
    "supplementary_data",
    paste0("pop_grid_", reference_date, ".tif")
  )
)

pop_gov <- read.csv(file.path(
  results_dir,
  reference_date,
  "supplementary_data",
  paste0("pop_gov_", reference_date, ".csv")
))

cols_pop_gov <- names(pop_gov)

pop_gov_geo <- st_read(file.path(
  results_dir,
  reference_date,
  "supplementary_data",
  paste0("pop_gov_", reference_date, ".gpkg")
))

interzone <- vect(file.path(
  in_dir,
  "evacuation_buffers",
  "yellow-orange-interzone_2026-03-12.gpkg"
))

gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"))

#--- process data ----#

# interzone to raster
interzone <- project(interzone, crs(pop_grid))
interzone$value <- 1
interzone_ras <- rasterize(interzone, pop_grid, field = "value")
interzone_ras[is.na(interzone_ras)] <- 0

# interzone population
pop_interzone <- pop_grid
pop_interzone[interzone_ras != 1] <- NA

# writeRaster(
#   pop_interzone,
#   file.path(
#     out_dir,
#     paste0("pop_grid_orange-yellow_", reference_date, ".tif")
#   ),
#   overwrite = TRUE
# )

# governorate level
pop_gov_interzone <- summarise_grid_per_governorate(
  pop_ras = pop_interzone,
  gov_poly = gov_geo,
  gov_ras = gov_grid,
  ref_date = reference_date
) %>%
  rename(pop_raw = population) %>%
  mutate(
    population = ifelse(
      pop_raw < 1000,
      round(pop_raw / 100) * 100,
      round(pop_raw / 1000) * 1000
    ),
    ADM2_PCODE = as.integer(ADM2_PCODE)
  ) %>%
  rename(pop_orange_yellow = population) %>%
  relocate(geom, .after = last_col())

pop_gov_geo_result <- pop_gov_geo %>%
  mutate(ADM2_PCODE = as.integer(ADM2_PCODE)) %>%
  left_join(
    pop_gov_interzone %>%
      st_drop_geometry() %>%
      select(ADM2_PCODE, pop_orange_yellow),
    by = "ADM2_PCODE"
  ) %>%
  select(
    ADM2_PCODE,
    ADM2_EN,
    date,
    pop_raw,
    population,
    pop_orange_yellow,
    everything()
  )

write.csv(
  pop_gov_geo_result %>% st_drop_geometry(),
  file.path(
    out_dir,
    paste0("pop_gov_orange-yellow_interzone_", reference_date, ".csv")
  ),
  row.names = F
)
