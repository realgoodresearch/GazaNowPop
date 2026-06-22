# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2026-06-09"
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

data_dir <- file.path(getwd(), "out", "data")
src_dir <- file.path(here::here(), "src", "deterministic")
model_dir <- file.path(getwd(), "out", "deterministic", "model", reference_date)
out_dir <- file.path(getwd(), "out", "deterministic", "results", reference_date)

dir.create(
  file.path(out_dir, "supplementary_data"),
  showWarnings = F,
  recursive = T
)

# load functions
source(file.path(src_dir, "20_results_fun.R"))

# load data
pop_grid_current <- rast(file.path(
  model_dir,
  "population.tif"
))

gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"))

mun_grid <- rast(file.path(data_dir, "mun_grid.tif"))
mun_geo <- st_read(file.path(data_dir, "mun_geo.gpkg"))

nbr_grid <- rast(file.path(data_dir, "nbr_grid.tif"))
nbr_geo <- st_read(file.path(data_dir, "nbr_geo.gpkg"))


pop_tents <- rast(file.path(model_dir, "pop_tents.tif"))
pop_bldgs <- rast(file.path(model_dir, "pop_bldgs.tif"))

#--- gridded population ---#

file.copy(
  from = file.path(
    model_dir,
    "population.tif"
  ),
  to = file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_grid_", reference_date, ".tif")
  ),
  overwrite = TRUE
)

# governorate
pop_gov <- summarise_grid_per_governorate(
  pop_ras = pop_grid_current,
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
    )
  ) %>%
  left_join(
    summarise_grid_per_governorate(
      pop_ras = pop_bldgs,
      gov_poly = gov_geo,
      gov_ras = gov_grid,
      ref_date = reference_date
    ) %>%
      rename(pop_in_bldgs = population) %>%
      st_drop_geometry() %>%
      select(ADM2_PCODE, pop_in_bldgs)
  ) %>%
  left_join(
    summarise_grid_per_governorate(
      pop_ras = pop_tents,
      gov_poly = gov_geo,
      gov_ras = gov_grid,
      ref_date = reference_date
    ) %>%
      rename(pop_in_tents = population) %>%
      st_drop_geometry() %>%
      select(ADM2_PCODE, pop_in_tents)
  ) %>%
  mutate(
    prop_in_bldgs = round(pop_in_bldgs / pop_raw, 2),
    prop_in_tents = round(pop_in_tents / pop_raw, 2)
  ) %>%
  select(-pop_in_bldgs, -pop_in_tents) %>%
  relocate(geom, .after = last_col())

st_write(
  pop_gov,
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_gov_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_gov |> st_drop_geometry(),
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_gov_", reference_date, ".csv")
  ),
  row.names = F
)

# municipality
pop_mun <- summarise_grid_per_municipality(
  pop_ras = pop_grid_current,
  mun_poly = mun_geo,
  mun_ras = mun_grid,
  ref_date = reference_date
) %>%
  rename(pop_raw = population) %>%
  mutate(
    population = ifelse(
      pop_raw < 1000,
      round(pop_raw / 100) * 100,
      round(pop_raw / 1000) * 1000
    )
  ) %>%
  left_join(
    summarise_grid_per_municipality(
      pop_ras = pop_bldgs,
      mun_poly = mun_geo,
      mun_ras = mun_grid,
      ref_date = reference_date
    ) %>%
      rename(pop_in_bldgs = population) %>%
      st_drop_geometry() %>%
      select(ADM3_PCODE, pop_in_bldgs)
  ) %>%
  left_join(
    summarise_grid_per_municipality(
      pop_ras = pop_tents,
      mun_poly = mun_geo,
      mun_ras = mun_grid,
      ref_date = reference_date
    ) %>%
      rename(pop_in_tents = population) %>%
      st_drop_geometry() %>%
      select(ADM3_PCODE, pop_in_tents)
  ) %>%
  mutate(
    prop_in_bldgs = round(pop_in_bldgs / pop_raw, 2),
    prop_in_tents = round(pop_in_tents / pop_raw, 2)
  ) %>%
  select(-pop_in_bldgs, -pop_in_tents) %>%
  relocate(geom, .after = last_col())

st_write(
  pop_mun,
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_mun_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_mun |> st_drop_geometry(),
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_mun_", reference_date, ".csv")
  ),
  row.names = F
)


# neighbourhood
pop_nbr <- summarise_grid_per_neighbourhood(
  pop_ras = pop_grid_current,
  nbr_poly = nbr_geo,
  nbr_ras = nbr_grid,
  ref_date = reference_date
) %>%
  rename(pop_raw = population) %>%
  mutate(
    population = ifelse(
      pop_raw < 1000,
      round(pop_raw / 100) * 100,
      round(pop_raw / 1000) * 1000
    )
  ) %>%
  left_join(
    summarise_grid_per_neighbourhood(
      pop_ras = pop_bldgs,
      nbr_poly = nbr_geo,
      nbr_ras = nbr_grid,
      ref_date = reference_date
    ) %>%
      rename(pop_in_bldgs = population) %>%
      st_drop_geometry() %>%
      select(ADM4_PCODE, pop_in_bldgs)
  ) %>%
  left_join(
    summarise_grid_per_neighbourhood(
      pop_ras = pop_tents,
      nbr_poly = nbr_geo,
      nbr_ras = nbr_grid,
      ref_date = reference_date
    ) %>%
      rename(pop_in_tents = population) %>%
      st_drop_geometry() %>%
      select(ADM4_PCODE, pop_in_tents)
  ) %>%
  mutate(
    prop_in_bldgs = round(pop_in_bldgs / pop_raw, 2),
    prop_in_tents = round(pop_in_tents / pop_raw, 2)
  ) %>%
  select(-pop_in_bldgs, -pop_in_tents) %>%
  relocate(geom, .after = last_col())

st_write(
  pop_nbr,
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_nbr_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_nbr |> st_drop_geometry(),
  file.path(
    out_dir,
    "supplementary_data",
    paste0("pop_nbr_", reference_date, ".csv")
  ),
  row.names = F
)

rm(pop_gov, pop_mun, pop_nbr)
