# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
binary_bldg_mask <- FALSE
reference_date <- "2025-11-23"
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
src_dir <- file.path(here::here(), "src")
out_dir <- file.path(getwd(), "out", "model", reference_date)
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
tents <- rast(file.path(data_dir, "tents.tif"))

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


#---- mask ----#

# create initial building cover raster
bldg_ras <- bldg_cover
if (binary_bldg_mask) {
  bldg_ras[bldg_ras > 0] <- 1
}

# NAs to zero for building cover raster
bldg_ras <- ifel(mastergrid == 1 & is.na(bldg_ras), 0, bldg_ras)

# assume 100% of housing remaining in OSM buildings where UNOSAT did not assess damage
housing_prop <- ifel(
  mastergrid == 1 & is.na(housing_prop) & bldg_ras > 0,
  1,
  housing_prop
)

# NAs to zero for housing proportion raster
housing_prop <- ifel(mastergrid == 1 & is.na(housing_prop), 0, housing_prop)

# reduce building coverage proportional to proportion of housing units remaining undamaged
bldg_ras <- bldg_ras * housing_prop

# add 10% of tent coverage to building coverage
# NOTE: Building coverage is footprints that do not account for multi-story buildings.
# NOTE: Tent coverage is extent of areas with tents, so includes ground area between tents.
# NOTE: 10% is rough adjustment for this difference.
tents <- ifel(mastergrid == 1 & is.na(tents), 0, tents)
bldg_ras <- bldg_ras + (tents * 0.1)
plot(bldg_ras)


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

# remove buildings for large areas of destroyed buildings where there are no tents
bldg_ras <- ifel(
  (bldg_destroyed_focal > 0.9 | housing_prop_focal < 0.1) & tents == 0,
  0,
  bldg_ras
)
plot(bldg_ras)

# building mask
bldg_mask <- matrix(
  bldg_ras[],
  nrow = nrow(bldg_cover),
  ncol = ncol(bldg_cover)
)
bldg_mask <- bldg_mask / max(bldg_mask, na.rm = T)

# masks
mask <- build_mask(
  ref_date = reference_date,
  evac = evac,
  evac_buffers = evac_buffers,
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

# penetration rates
penetration1 <- sum(values(subscribers1), na.rm = T) / 2.1e6
penetration2 <- sum(values(subscribers2), na.rm = T) / 2.1e6

# population
population <- mastergrid
population[mastergrid == 1] <- NA

# # coverage: neither provider
# idx <- is.na(subscribers1) & is.na(subscribers2) & mastergrid == 1
# population[idx] <- 0

# coverage: only provider 1
idx <- !is.na(subscribers1) & is.na(subscribers2)
population[idx] <- subscribers1[idx] / penetration1

# coverage: only provider 2
idx <- is.na(subscribers1) & !is.na(subscribers2)
population[idx] <- subscribers2[idx] / penetration2

# coverage: both providers
idx <- !is.na(subscribers1) & !is.na(subscribers2)
population[idx] <- (subscribers1[idx] + subscribers2[idx]) /
  (penetration1 + penetration2)

# rescale to 2.1e6 total population
population = population * (2.1e6 / sum(values(population), na.rm = T))
plot(population)

writeRaster(population, file.path(out_dir, "population.tif"), overwrite = TRUE)
