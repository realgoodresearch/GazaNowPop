# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2026-03-24"
max_tower_radius <- 2 # max tower radius (km)
#----------------------#

# load libraries
library(dplyr)
library(terra)
library(sf)
library(purrr)

# encoding for Arabic
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

in_dir <- file.path(getwd(), "in")
data_dir <- file.path(getwd(), "out", "data")
src_dir <- file.path(here::here(), "src", "deterministic")
out_dir <- file.path(getwd(), "out", "deterministic", "model", reference_date)
dir.create(out_dir, showWarnings = F, recursive = T)

# load functions
source(file.path(src_dir, "10_model_fun.R"))

#---- load data ----#
date_link <- read.csv(file.path(in_dir, "telecoms", "telecoms_date_link.csv"))

mastergrid <- rast(file.path(data_dir, "mastergrid.tif"))
telco1 <- read.csv(file.path(data_dir, "telco1.csv"))
telco2 <- read.csv(file.path(data_dir, "telco2.csv"))

gov_geo <- vect(file.path(data_dir, "gov_geo.gpkg"))
gov_rast <- rast(file.path(data_dir, "gov_grid.tif"))
mun_geo <- vect(file.path(data_dir, "mun_geo.gpkg"))
mun_rast <- rast(file.path(data_dir, "mun_grid.tif"))
nbr_geo <- vect(file.path(data_dir, "nbr_geo.gpkg"))
nbr_rast <- rast(file.path(data_dir, "nbr_grid.tif"))

bldg_cover <- rast(file.path(data_dir, "osm_building_coverage.tif"))

evac <- read.csv(file.path(
  in_dir,
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

bldg_destroyed <- rast(file.path(data_dir, "bldg_destroyed.tif"))
housing <- rast(file.path(data_dir, "housing.tif"))
housing_prop <- rast(file.path(data_dir, "housing_proportion_undamaged.tif"))
tents <- rast(file.path(data_dir, "tent_count.tif"))

#---- telecoms ----#

# filter to reference date
telco_dates <- date_link %>%
  filter(date == reference_date) %>%
  select(provider1, provider2) %>%
  as.vector()

telco1 <- telco1 %>%
  filter(date == telco_dates[1])

telco2 <- telco2 %>%
  filter(date == telco_dates[2])


#---- towers ----#
towers1_geo <- telco1 |>
  distinct(latitude, longitude, .keep_all = TRUE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

towers2_geo <- telco2 |>
  distinct(latitude, longitude, .keep_all = TRUE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

st_write(towers1_geo, file.path(out_dir, "towers1_geo.gpkg"), append = FALSE)
st_write(towers2_geo, file.path(out_dir, "towers2_geo.gpkg"), append = FALSE)


#---- tower buffers ----#
towers1_buffer <- st_buffer(towers1_geo, max_tower_radius * 1e3)
plot(towers1_buffer %>% select(tower_id))

towers2_buffer <- st_buffer(towers2_geo, max_tower_radius * 1e3)
plot(towers2_buffer %>% select(tower_id))

st_write(
  towers1_buffer,
  file.path(out_dir, "towers1_buffer.gpkg"),
  append = FALSE
)

st_write(
  towers2_buffer,
  file.path(out_dir, "towers2_buffer.gpkg"),
  append = FALSE
)


#---- tower voronoi ----#
env <- st_union(st_as_sf(gov_geo)) %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG: 32636")

# provider 1
pnt1 <- st_union(towers1_geo) %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG: 32636")

voronoi1 <- st_voronoi(pnt1, env) %>%
  st_collection_extract("POLYGON") %>%
  st_sfc() %>%
  st_intersection(env) %>%
  st_make_valid() %>%
  st_transform(st_crs(towers1_geo))

# provider 2
pnt2 <- st_union(towers2_geo) %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG: 32636")

voronoi2 <- st_voronoi(pnt2, env) %>%
  st_collection_extract("POLYGON") %>%
  st_sfc() %>%
  st_intersection(env) %>%
  st_make_valid() %>%
  st_transform(st_crs(towers2_geo))

towers1_voronoi <- join_towers_to_voronoi(towers1_geo, voronoi1)
towers2_voronoi <- join_towers_to_voronoi(towers2_geo, voronoi2)

plot(towers1_voronoi %>% select(tower_id))
plot(towers2_voronoi %>% select(tower_id))

st_write(
  towers1_voronoi,
  file.path(out_dir, "towers1_voronoi.gpkg"),
  append = FALSE
)

st_write(
  towers2_voronoi,
  file.path(out_dir, "towers2_voronoi.gpkg"),
  append = FALSE
)


#---- buffered tower voronoi polygons ----#
towers1_buff_voronoi <- intersect_voronoi_buffer(
  towers1_voronoi,
  towers1_buffer
)
towers2_buff_voronoi <- intersect_voronoi_buffer(
  towers2_voronoi,
  towers2_buffer
)

plot(towers1_buff_voronoi %>% select(tower_id))
plot(towers1_buff_voronoi %>% select(tower_id))

st_write(
  towers1_buff_voronoi,
  file.path(out_dir, "towers1_buff_voronoi.gpkg"),
  append = FALSE
)
st_write(
  towers2_buff_voronoi,
  file.path(out_dir, "towers2_buff_voronoi.gpkg"),
  append = FALSE
)


#---- mask (housing availability) ----#
housing <- ifel(mastergrid == 1 & is.na(housing), 0, housing)
tents <- ifel(mastergrid == 1 & is.na(tents), 0, tents)

# average building destruction within 500 m focal window
bldg_destroyed_focal <- focal(
  x = bldg_destroyed %>% project(mastergrid),
  w = focalMat(mastergrid, d = 500, type = "circle"),
  fun = mean,
  na.rm = TRUE
)

# average proportion of housing units remaining within 500 m focal window
housing_prop_focal <- focal(
  x = housing_prop %>% project(mastergrid),
  w = focalMat(mastergrid, d = 500, type = "circle"),
  fun = mean,
  na.rm = TRUE
)

# remove housing for large areas of destroyed buildings where there are no tents
housing_nonisolated <- ifel(
  (bldg_destroyed_focal > 0.9 | housing_prop_focal < 0.1) & tents == 0,
  0,
  housing
)
plot(housing_nonisolated)

# undamanged housing and tents
housing_tents <- housing_nonisolated + tents
plot(housing_tents)

# building mask
bldg_mask <- matrix(
  housing_tents[],
  nrow = nrow(housing_tents),
  ncol = ncol(housing_tents)
)
# bldg_mask <- bldg_mask / max(bldg_mask, na.rm = T)

# masks
mask <- build_mask(
  ref_date = reference_date,
  evac = evac,
  evac_buffers = evac_buffers, # NULL,
  bldg_mask = bldg_mask,
  mastergrid = mastergrid
)
plot(mask)
writeRaster(mask, file.path(out_dir, "mask.tif"), overwrite = TRUE)


#---- re-assign towers if catchment 100% masked out ----#

towers1_catchment <- merge_masked_voronoi(
  towers = towers1_geo,
  voronoi = towers1_buff_voronoi,
  mask = mask
)

towers2_catchment <- merge_masked_voronoi(
  towers = towers2_geo,
  voronoi = towers2_buff_voronoi,
  mask = mask
)

st_write(
  towers1_catchment,
  file.path(out_dir, "towers1_catchment.gpkg"),
  append = FALSE
)

st_write(
  towers2_catchment,
  file.path(out_dir, "towers2_catchment.gpkg"),
  append = FALSE
)


# tower catchment as rasters for zonal statistics
towers1_zones <- terra::rasterize(
  x = towers1_catchment %>%
    st_transform(crs = crs(mastergrid)) %>%
    vect(),
  y = rast(mastergrid),
  field = "tower_id",
  background = NA
)

towers2_zones <- terra::rasterize(
  x = towers2_catchment %>%
    st_transform(crs = crs(mastergrid)) %>%
    vect(),
  y = rast(mastergrid),
  field = "tower_id",
  background = NA
)

coverage_mask <- (!is.na(towers1_zones) | !is.na(towers2_zones)) + 0

#---- subscriber rasters ----#

# rasterise total subscribers per zone
subscribers1_per_zone <- terra::rasterize(
  x = towers1_catchment %>%
    st_transform(crs = crs(mastergrid)) %>%
    vect(),
  y = rast(mastergrid),
  field = "subscribers",
  background = NA
)

subscribers2_per_zone <- terra::rasterize(
  x = towers2_catchment %>%
    st_transform(crs = crs(mastergrid)) %>%
    vect(),
  y = rast(mastergrid),
  field = "subscribers",
  background = NA
)

# subscriber spatial redistribution factors (mask values rescaled to sum to 1 within zones)
mask1 <- rast(mastergrid)
for (tower_id in unique(sort(towers1_catchment$tower_id))) {
  values <- as.vector(mask[towers1_zones == tower_id])
  denom <- sum(values, na.rm = T)
  mask1[towers1_zones == tower_id] <- values / denom
  # check: sum(mask1[towers1_zones == tower_id], na.rm=T) == 1
}

mask2 <- rast(mastergrid)
for (tower_id in unique(sort(towers2_catchment$tower_id))) {
  values <- as.vector(mask[towers2_zones == tower_id])
  denom <- sum(values, na.rm = T)
  mask2[towers2_zones == tower_id] <- values / denom
  # check: sum(mask2[towers2_zones == tower_id], na.rm=T) == 1
}

plot(mask1)
plot(mask2)

# subscriber rasters
subscribers1 <- subscribers1_per_zone * mask1
subscribers2 <- subscribers2_per_zone * mask2

plot(subscribers1)
plot(subscribers2)

writeRaster(
  subscribers1,
  file.path(out_dir, "subscribers1.tif"),
  overwrite = TRUE
)
writeRaster(
  subscribers2,
  file.path(out_dir, "subscribers2.tif"),
  overwrite = TRUE
)


#---- population estimation ----#

# housing with and without coverage
hh_with_cov <- zonal(mask, coverage_mask, fun = "sum", na.rm = T)

# population with and without coverage
# N = population in coverage area
# M = population outside coverage area
# phi = average household size

pop_with_cov <- pop_with_coverage(
  T = 2.1e6,
  H = hh_with_cov %>% filter(tower_id == 1) %>% pull(mastergrid),
  J = hh_with_cov %>% filter(tower_id == 0) %>% pull(mastergrid)
)


# penetration rates (inside coverage area)
penetration1 <- sum(values(subscribers1), na.rm = T) / pop_with_cov$N
penetration2 <- sum(values(subscribers2), na.rm = T) / pop_with_cov$N

# population
population <- mastergrid
population[mastergrid == 1] <- NA

# coverage: neither provider
idx0 <- coverage_mask == 0 & mastergrid == 1
population[idx0] <- mask[idx0] * pop_with_cov$phi

# coverage: only provider 1
idx1 <- !is.na(subscribers1) & is.na(subscribers2)
population[idx1] <- subscribers1[idx1] / penetration1

# coverage: only provider 2
idx2 <- is.na(subscribers1) & !is.na(subscribers2)
population[idx2] <- subscribers2[idx2] / penetration2

# coverage: both providers
idx3 <- !is.na(subscribers1) & !is.na(subscribers2)
population[idx3] <- (subscribers1[idx3] + subscribers2[idx3]) /
  (penetration1 + penetration2)

# rescale to 2.1e6 total population
population = population * (2.1e6 / sum(values(population), na.rm = T))
plot(population)

writeRaster(population, file.path(out_dir, "population.tif"), overwrite = TRUE)


#--- derived household size and housing ---#

# housing_tents <- housing_nonisolated + tents

hhsize <- population / housing_tents

pop_tents <- tents * hhsize
pop_bldgs <- housing_nonisolated * hhsize

# checks
sum(pop_tents[], na.rm = T)
sum(pop_bldgs[], na.rm = T)
sum(pop_tents[], na.rm = T) + sum(pop_bldgs[], na.rm = T)

check <- (pop_tents + pop_bldgs) - population
summary(check[])

# save to disk
writeRaster(hhsize, file.path(out_dir, "hhsize.tif"), overwrite = TRUE)
writeRaster(pop_tents, file.path(out_dir, "pop_tents.tif"), overwrite = TRUE)
writeRaster(pop_bldgs, file.path(out_dir, "pop_bldgs.tif"), overwrite = TRUE)
writeRaster(tents, file.path(out_dir, "tents.tif"), overwrite = TRUE)
writeRaster(
  housing_nonisolated,
  file.path(out_dir, "housing_units.tif"),
  overwrite = TRUE
)
