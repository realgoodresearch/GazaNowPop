# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2025-10-19"
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

data_dir <- file.path(getwd(), "out", "data")
model_dir <- file.path(getwd(), "out", "model", reference_date)
out_dir <- file.path(getwd(), "out", "results")
dir.create(out_dir, showWarnings = F, recursive = T)

# packaged supplementary data directory
pkg_dir <- file.path(out_dir, paste0("Gaza_NowPop_", reference_date))
dir.create(
  file.path(pkg_dir, "supplementary_data"),
  showWarnings = F,
  recursive = T
)

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


#---- reporting for "current" date ----#

# gridded population
file.copy(
  from = file.path(
    model_dir,
    "population.tif"
  ),
  to = file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_grid_", reference_date, ".tif")
  ),
  overwrite = TRUE
)

# governorate
pop_gov <- gov_geo |>
  rename(
    ADM2_EN = Name,
    ADM2_PCODE = PCODE
  ) |>
  select(id, ADM2_EN, ADM2_PCODE) |>
  mutate(
    population = NA,
    date = reference_date
  )

for (i in 1:nrow(pop_gov)) {
  pop_gov$population[i] <- sum(
    pop_grid_current[gov_grid == pop_gov$id[i]],
    na.rm = T
  )
}

pop_gov <- pop_gov |>
  select(ADM2_PCODE, ADM2_EN, date, population) |>
  arrange(ADM2_PCODE)

st_write(
  pop_gov,
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_gov_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_gov |> st_drop_geometry(),
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_gov_", reference_date, ".csv")
  ),
  row.names = F
)

# municipality
pop_mun <- mun_geo |>
  rename(
    ADM2_EN = Governorat,
    ADM3_EN = Name,
    ADM3_PCODE = PCOE_Munic,
  ) |>
  mutate(ADM2_PCODE = substr(ADM3_PCODE, 1, 3)) |>
  select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE) |>
  mutate(
    population = NA,
    date = reference_date
  )

for (i in 1:nrow(pop_mun)) {
  pop_mun$population[i] <- sum(
    pop_grid_current[mun_grid == pop_mun$id[i]],
    na.rm = T
  )
}

pop_mun <- pop_mun |>
  select(ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE, date, population) |>
  arrange(ADM3_PCODE)

st_write(
  pop_mun,
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_mun_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_mun |> st_drop_geometry(),
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_mun_", reference_date, ".csv")
  ),
  row.names = F
)


# neighbourhood
pop_nbr <- nbr_geo |>
  rename(
    ADM2_EN = Governorat,
    ADM2_PCODE = PCODE_Gove,
    ADM3_EN = Name_Munic,
    ADM3_PCODE = PCOE_Munic,
    ADM4_EN = Neighbourh,
    ADM4_PCODE = PCODE_Neig
  ) |>
  select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE, ADM4_EN, ADM4_PCODE) |>
  mutate(
    population = NA,
    date = reference_date
  )

for (i in 1:nrow(pop_nbr)) {
  pop_nbr$population[i] <- sum(
    pop_grid_current[nbr_grid == pop_nbr$id[i]],
    na.rm = T
  )
}

pop_nbr <- pop_nbr |>
  select(
    ADM2_EN,
    ADM2_PCODE,
    ADM3_EN,
    ADM3_PCODE,
    ADM4_EN,
    ADM4_PCODE,
    date,
    population
  ) |>
  arrange(ADM4_PCODE)

st_write(
  pop_nbr,
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_nbr_", reference_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_nbr |> st_drop_geometry(),
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_nbr_", reference_date, ".csv")
  ),
  row.names = F
)
