# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
kappa <- 2
binary_bldg_mask <- FALSE

date_link <- data.frame(
  provider1 = c(
    "2025-01-30",
    "2025-02-02",
    "2025-02-06",
    "2025-02-10",
    "2025-02-13",
    "2025-02-17",
    "2025-02-20",
    "2025-02-24",
    "2025-03-03",
    "2025-03-13",
    "2025-03-17",
    "2025-03-24",
    "2025-03-31",
    "2025-04-10",
    "2025-04-14",
    "2025-04-17",
    "2025-04-21",
    "2025-05-12",
    "2025-05-19",
    "2025-05-25",
    "2025-06-09"
  ),
  provider2 = c(
    "2025-01-27",
    "2025-02-02",
    "2025-02-05",
    "2025-02-10",
    "2025-02-13",
    "2025-02-16",
    "2025-02-20",
    "2025-02-24",
    "2025-03-03",
    "2025-03-13",
    "2025-03-17",
    "2025-03-24",
    "2025-04-01",
    "2025-04-09",
    "2025-04-14",
    "2025-04-15",
    "2025-04-21",
    "2025-05-13",
    "2025-05-19",
    "2025-05-25",
    "2025-06-09"
  )
)

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

data_dir <- file.path(getwd(), "out", "telecom_towers_deterministic", "data")
polio_dir <- file.path(getwd(), "out", "polio3", "1_base")
src_dir <- file.path(here::here(), "src", "telecom_towers_deterministic")
out_dir <- file.path(getwd(), "out", "telecom_towers_deterministic", "model")
dir.create(out_dir, showWarnings = F, recursive = T)

# load functions
source(file.path(src_dir, "10_model_fun.R"))

# load data
mastergrid <- rast(file.path(data_dir, "mastergrid.tif"))
distance1 <- readRDS(file.path(data_dir, "distance1.rds"))
distance2 <- readRDS(file.path(data_dir, "distance2.rds"))
radius1 <- readRDS(file.path(data_dir, "radius1.rds"))
radius2 <- readRDS(file.path(data_dir, "radius2.rds"))
telco1 <- read.csv(file.path(data_dir, "telco1.csv"))
telco2 <- read.csv(file.path(data_dir, "telco2.csv"))

gov_geo <- vect(file.path(data_dir, "gov_geo.gpkg"))
gov_rast <- rast(file.path(data_dir, "gov_grid.tif"))
mun_geo <- vect(file.path(data_dir, "mun_geo.gpkg"))
mun_rast <- rast(file.path(data_dir, "mun_grid.tif"))
nbr_geo <- vect(file.path(data_dir, "nbr_geo.gpkg"))
nbr_rast <- rast(file.path(data_dir, "nbr_grid.tif"))

fit_polio <- readRDS(file.path(polio_dir, "fit.rds"))
mun_polio <- read.csv(file.path(polio_dir, "dat_mun_all.csv"))

bldg_cover <- rast(file.path(data_dir, "osm_building_coverage.tif"))

evac <- read.csv(file.path(
  here::here(),
  "data",
  "evacuation_orders",
  "evacuation_orders.csv"
))
evac_grid <- rast(file.path(data_dir, "evac_grid.tif"))

evac_buffers <- list()
lf <- list.files(file.path(data_dir, "evacuation_buffers"))
for (f in lf) {
  date <- sub("^.*_(.*)\\.tif$", "\\1", f)
  evac_buffers[[date]] <- rast(file.path(data_dir, "evacuation_buffers", f))
}


# check telecoms dates
telco1 |>
  select(date) |>
  distinct() |>
  arrange() |>
  pull()

telco2 |>
  select(date) |>
  distinct() |>
  arrange() |>
  pull()


#---- tower coverage probabilities ----#
p1 <- tower_coverage(
  dist = distance1,
  rad = radius1,
  mastergrid = mastergrid,
  kappa = kappa
)

p2 <- tower_coverage(
  dist = distance2,
  rad = radius2,
  mastergrid = mastergrid,
  kappa = kappa
)

saveRDS(p1, file.path(out_dir, "tower_coverage1.rds"))
saveRDS(p2, file.path(out_dir, "tower_coverage2.rds"))

# random spot checks
p_spot_check(
  tower_id = round(runif(1, 1, length(radius1))),
  p = p1
)

#---- mask ----#

# building cover
bldg_ras <- bldg_cover
if (binary_bldg_mask) bldg_ras[bldg_ras > 0] <- 1
plot(bldg_ras)

bldg_mask <- matrix(
  bldg_ras[],
  nrow = nrow(bldg_cover),
  ncol = ncol(bldg_cover)
)
bldg_mask <- bldg_mask / max(bldg_mask, na.rm = T)

# masks
p_mask1 <- build_mask(
  dates = sort(unique(telco1$date)),
  evac = evac,
  evac_buffers = evac_buffers,
  bldg_mask = bldg_mask,
  mastergrid = mastergrid
)

p_mask2 <- build_mask(
  dates = sort(unique(telco2$date)),
  evac = evac,
  evac_buffers = evac_buffers,
  bldg_mask = bldg_mask,
  mastergrid = mastergrid
)

#---- subscriber rasters ----#
subscriber_raster(
  telco = telco1,
  p_tower = p1,
  p_mask = p_mask1,
  mastergrid = mastergrid,
  outpath = file.path(out_dir, "provider_rasters", "provider1.tif")
)

subscriber_raster(
  telco = telco2,
  p_tower = p2,
  p_mask = p_mask2,
  mastergrid = mastergrid,
  outpath = file.path(out_dir, "provider_rasters", "provider2.tif")
)

#---- sum subscribers across providers ----#
dir.create(
  file.path(out_dir, "subscribers_combined"),
  showWarnings = F,
  recursive = T
)

for (row in 1:nrow(date_link)) {
  dates <- unlist(as.vector(date_link[row, ]))

  # total subscribers across both providers
  ras1 <- rast(file.path(
    out_dir,
    "provider_rasters",
    paste0("provider1_", dates[1], ".tif")
  ))
  ras2 <- rast(file.path(
    out_dir,
    "provider_rasters",
    paste0("provider2_", dates[2], ".tif")
  ))
  ras <- ras1 + ras2
  writeRaster(
    ras,
    file.path(
      out_dir,
      "subscribers_combined",
      paste0("subscribers_", max(dates), ".tif")
    ),
    overwrite = T
  )
}

#---- extrapolate subscribers to population ----#
dir.create(
  file.path(out_dir, "telecom_population"),
  showWarnings = F,
  recursive = T
)

# penetration (based on polio)
ras <- rast(file.path(
  out_dir,
  "subscribers_combined",
  "subscribers_2025-02-24.tif"
))
pop_gov_polio <- c(373190, 724521, 346678, 494301, 161310)
pop_gov_mosd <- c(283559, 672363, NA, NA, NA)
pop_gov_ocha <- c(283559, 672363, 436387, 594653, 91487)

true_pop_gov <- c(pop_gov_mosd[1:2], pop_gov_polio[3:5])

penetration <- c()
for (i in 1:nrow(gov_geo)) {
  subscribers <- sum(ras[gov_rast == i], na.rm = T)
  penetration[i] <- subscribers / true_pop_gov[i]
}

penetration_df <- gov_geo |>
  as.data.frame() |>
  rename(ADM2_EN = Name, ADM2_PCODE = PCODE) |>
  select(ADM2_EN, ADM2_PCODE) |>
  mutate(pent = penetration) |>
  rename(penetration = pent)

write.csv(penetration_df, file.path(out_dir, "penetration.csv"), row.names = F)


lf <- list.files(file.path(out_dir, "subscribers_combined"))
dates <- sub("^.*_(.*)\\.tif$", "\\1", lf)
adjustment_df <- data.frame(
  date = dates,
  adjustment = NA
)
for (fi in 1:length(lf)) {
  # penetration <- sum(ras[], na.rm = T) / 2.1e6
  # ras_scaled <- ras / penetration

  f <- lf[fi]
  date <- dates[fi]

  subscribers <- rast(file.path(out_dir, "subscribers_combined", f))

  population <- mastergrid
  population[mastergrid == 1] <- 0

  for (i in 1:nrow(gov_geo)) {
    population[gov_rast == i] <- subscribers[gov_rast == i] / penetration[i]
  }

  penetration_adjustment <- sum(population[], na.rm = T) / 2.1e6
  population <- population / penetration_adjustment

  adjustment_df[fi, "adjustment"] <- penetration_adjustment

  writeRaster(
    population,
    file.path(
      out_dir,
      "telecom_population",
      paste0("population_", date, ".tif")
    ),
    overwrite = T
  )
}

write.csv(
  adjustment_df,
  file.path(out_dir, "penetration_adjustment.csv"),
  row.names = F
)


#---- populations by admin ----#
dir.create(
  file.path(out_dir, "telecom_population_neighbourhood"),
  showWarnings = F,
  recursive = T
)
dir.create(
  file.path(out_dir, "telecom_population_municipality"),
  showWarnings = F,
  recursive = T
)
dir.create(
  file.path(out_dir, "telecom_population_governorate"),
  showWarnings = F,
  recursive = T
)


lf <- list.files(file.path(out_dir, "telecom_population"))
for (f in lf) {
  pop_ras <- rast(file.path(out_dir, "telecom_population", f))
  current_date <- sub("^.*_(.*)\\.tif$", "\\1", f)

  # neighbourhoods
  pop_nbr <- nbr_geo |>
    st_as_sf() |>
    rename(
      ADM2_EN = Governorat,
      ADM2_PCODE = PCODE_Gove,
      ADM3_EN = Name_Munic,
      ADM3_PCODE = PCOE_Munic,
      ADM4_EN = Neighbourh,
      ADM4_PCODE = PCODE_Neig
    ) |>
    mutate(
      date = current_date,
      population = NA
    ) |>
    select(
      id,
      ADM2_EN,
      ADM2_PCODE,
      ADM3_EN,
      ADM3_PCODE,
      ADM4_EN,
      ADM4_PCODE,
      date,
      population
    ) |>
    arrange(ADM2_PCODE, ADM3_PCODE, ADM4_PCODE)

  for (i in 1:nrow(pop_nbr)) {
    pop_nbr[i, "population"] <- sum(pop_ras[nbr_rast == pop_nbr$id[i]])
  }

  st_write(
    pop_nbr,
    file.path(
      out_dir,
      "telecom_population_neighbourhood",
      paste0("population_", current_date, ".gpkg")
    ),
    append = F
  )

  # municipality
  pop_mun <- mun_geo |>
    st_as_sf() |>
    rename(
      ADM2_EN = Governorat,
      ADM3_EN = Name,
      ADM3_PCODE = PCOE_Munic
    ) |>
    mutate(
      ADM2_PCODE = substr(ADM3_PCODE, 1, 3),
      date = current_date,
      population = NA
    ) |>
    select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE, date, population) |>
    arrange(ADM2_PCODE, ADM3_PCODE)

  for (i in 1:nrow(pop_mun)) {
    pop_mun$population[i] <- sum(pop_ras[mun_rast == pop_mun$id[i]], na.rm = T)
  }

  st_write(
    pop_mun,
    file.path(
      out_dir,
      "telecom_population_municipality",
      paste0("population_", current_date, ".gpkg")
    ),
    append = F
  )

  # governorate
  pop_gov <- gov_geo |>
    st_as_sf() |>
    rename(
      ADM2_EN = Name,
      ADM2_PCODE = PCODE
    ) |>
    mutate(
      population = NA,
      date = current_date
    ) |>
    select(id, ADM2_EN, ADM2_PCODE, date, population) |>
    arrange(ADM2_PCODE)

  for (i in 1:nrow(pop_gov)) {
    pop_gov$population[i] <- sum(pop_ras[gov_rast == pop_gov$id[i]], na.rm = T)
  }

  st_write(
    pop_gov,
    file.path(
      out_dir,
      "telecom_population_governorate",
      paste0("population_", current_date, ".gpkg")
    ),
    append = F
  )
}

# #---- align with polio-based estimates ----#

# dir.create(file.path(out_dir, "polio_adjusted"), showWarnings = F, recursive = T)

# # municipality scaling factors from polio analysis
# mun_scale <- as.data.frame(mun_geo) |>
#   rename(ADM2_EN = Governorat, ADM3_EN = Name, ADM3_PCODE = PCOE_Munic) |>
#   mutate(
#     ADM2_PCODE = as.numeric(substr(ADM3_PCODE, 1, 3)),
#     ADM3_PCODE = as.numeric(ADM3_PCODE)
#   ) |>
#   arrange(id) |>
#   select(ADM2_PCODE, ADM2_EN, ADM3_PCODE, ADM3_EN, id) |>
#   left_join(mun_polio |>
#     select(ADM3_PCODE, idx) |>
#     rename(polio_id = idx))

# for (i in 1:nrow(mun_scale)) {
#   polio_id <- mun_scale$polio_id[i]
#   if (is.na(polio_id)) {
#     mun_scale[i, "polio_estimate"] <- 0
#   } else {
#     draws <- fit_polio$draws(paste0("N[", polio_id, "]"), format = "df") |>
#       select(-contains(".")) |>
#       pull()
#     mun_scale[i, "polio_estimate"] <- mean(draws)
#   }
# }

# date <- "2025-02-24"
# subs <- rast(file.path(out_dir, "telecom_population", paste0("population_", date, ".tif")))
# scale_rast <- mastergrid
# for (i in 1:nrow(mun_scale)) {
#   polio_est <- mun_scale$polio_estimate[i]
#   mun_id <- mun_scale$id[i]
#   telco_est <- sum(subs[mun_rast == mun_id])
#   scale_factor <- polio_est / telco_est
#   scale_rast[mun_rast == mun_id] <- scale_factor
# }

# writeRaster(scale_rast, file.path(out_dir, "polio_adjusted", "scale_factor.tif"), overwrite = T)

# # population raster
# pop_ras <- subs * scale_rast
# plot(pop_ras)
# sum(pop_ras[], na.rm = T)

# writeRaster(pop_ras, file.path(out_dir, "polio_adjusted", paste0("polio_population_", date, ".tif")), overwrite = T)

# # population per neighbourhood
# pop_nbr <- nbr_geo |>
#   st_as_sf() |>
#   rename(
#     ADM2_EN = Governorat,
#     ADM2_PCODE = PCODE_Gove,
#     ADM3_EN = Name_Munic,
#     ADM3_PCODE = PCOE_Munic,
#     ADM4_EN = Neighbourh,
#     ADM4_PCODE = PCODE_Neig
#   ) |>
#   select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE, ADM4_EN, ADM4_PCODE)

# pop_col <- paste0("pop_", date)
# pop_nbr[, pop_col] <- NA

# for (i in 1:nrow(pop_nbr)) {
#   pop_nbr[i, pop_col] <- sum(pop_ras[nbr_rast == pop_nbr$id[i]])
# }

# st_write(pop_nbr, file.path(out_dir, "polio_adjusted", paste0("pop_nbr_", date, ".gpkg")), append = F)
