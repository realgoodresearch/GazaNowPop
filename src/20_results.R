# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
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
model_dir <- file.path(getwd(), "out", "model")
out_dir <- file.path(getwd(), "out", "results")
dir.create(out_dir, showWarnings = F, recursive = T)

# current date
lf <- list.files(file.path(model_dir, "telecom_population"))
dates <- as.Date(sub("^.*_(.*)\\.tif$", "\\1", lf))
current_date <- max(dates)
# current_date <- "2025-04-10"

# packaged supplementary data directory
pkg_dir <- file.path(out_dir, paste0("Oxford_Gaza_NowPop_", current_date))
dir.create(
  file.path(pkg_dir, "supplementary_data"),
  showWarnings = F,
  recursive = T
)
dir.create(file.path(pkg_dir, "all_dates"), showWarnings = F, recursive = T)

# load data
pop_grid_current <- rast(file.path(
  model_dir,
  "telecom_population",
  paste0("population_", current_date, ".tif")
))

gov_grid <- rast(file.path(data_dir, "gov_grid.tif"))
gov_geo <- st_read(file.path(data_dir, "gov_geo.gpkg"))

mun_grid <- rast(file.path(data_dir, "mun_grid.tif"))
mun_geo <- st_read(file.path(data_dir, "mun_geo.gpkg"))

nbr_grid <- rast(file.path(data_dir, "nbr_grid.tif"))
nbr_geo <- st_read(file.path(data_dir, "nbr_geo.gpkg"))

#---- results for all dates in one csv ----#

# neighbourhood-level
lf <- list.files(file.path(model_dir, "telecom_population_neighbourhood"))
lf_dates <- as.Date(sub("^.*_(.*)\\.gpkg$", "\\1", lf))
lf <- lf[lf_dates <= current_date]

dat <- st_read(file.path(model_dir, "telecom_population_neighbourhood", lf[1]))
for (i in 2:length(lf)) {
  dat_next <- st_read(file.path(
    model_dir,
    "telecom_population_neighbourhood",
    lf[i]
  ))
  dat <- rbind(dat, dat_next)
}

dat <- dat |>
  st_drop_geometry() |>
  select(-id)

write.csv(
  dat,
  file.path(pkg_dir, "all_dates", "pop_nbr_all_dates.csv"),
  row.names = F
)

# municipality-level
lf <- list.files(file.path(model_dir, "telecom_population_municipality"))
lf_dates <- as.Date(sub("^.*_(.*)\\.gpkg$", "\\1", lf))
lf <- lf[lf_dates <= current_date]

dat <- st_read(file.path(model_dir, "telecom_population_municipality", lf[1]))
for (i in 2:length(lf)) {
  dat_next <- st_read(file.path(
    model_dir,
    "telecom_population_municipality",
    lf[i]
  ))
  dat <- rbind(dat, dat_next)
}

dat <- dat |>
  st_drop_geometry() |>
  select(-id)

write.csv(
  dat,
  file.path(pkg_dir, "all_dates", "pop_mun_all_dates.csv"),
  row.names = F
)

# governorate-level
lf <- list.files(file.path(model_dir, "telecom_population_governorate"))
lf_dates <- as.Date(sub("^.*_(.*)\\.gpkg$", "\\1", lf))
lf <- lf[lf_dates <= current_date]

dat <- st_read(file.path(model_dir, "telecom_population_governorate", lf[1]))
for (i in 2:length(lf)) {
  dat_next <- st_read(file.path(
    model_dir,
    "telecom_population_governorate",
    lf[i]
  ))
  dat <- rbind(dat, dat_next)
}

dat <- dat |>
  st_drop_geometry() |>
  select(-id)

write.csv(
  dat,
  file.path(pkg_dir, "all_dates", "pop_gov_all_dates.csv"),
  row.names = F
)

#---- reporting for "current" date ----#

# gridded population
file.copy(
  from = file.path(
    model_dir,
    "telecom_population",
    paste0("population_", current_date, ".tif")
  ),
  to = file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_grid_", current_date, ".tif")
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
    date = current_date
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
    paste0("pop_gov_", current_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_gov |> st_drop_geometry(),
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_gov_", current_date, ".csv")
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
    date = current_date
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
    paste0("pop_mun_", current_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_mun |> st_drop_geometry(),
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_mun_", current_date, ".csv")
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
    date = current_date
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
    paste0("pop_nbr_", current_date, ".gpkg")
  ),
  append = FALSE
)

write.csv(
  pop_nbr |> st_drop_geometry(),
  file.path(
    pkg_dir,
    "supplementary_data",
    paste0("pop_nbr_", current_date, ".csv")
  ),
  row.names = F
)
