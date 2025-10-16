# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
grid_size <- 100
tower_radius_neighbours <- 3
tower_radius_scaling <- 1
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
src_dir <- file.path(here::here(), "src")
out_dir <- file.path(getwd(), "out", "data")
dir.create(out_dir, showWarnings = F, recursive = T)

# load data
gov_geo <- st_read(file.path(
  in_dir,
  "admin_boundaries",
  "OCHA",
  "Governorates",
  "Governorates_Population_polygons.shp"
))
mun_geo <- st_read(file.path(
  in_dir,
  "admin_boundaries",
  "OCHA",
  "Municipalities",
  "GazaMunicipalities.shp"
))
nbr_geo <- st_read(file.path(
  in_dir,
  "admin_boundaries",
  "OCHA",
  "Neighbourhoods",
  "Neighbourhoods_Population_polygons.shp"
))

telecoms <- read.csv(file.path(in_dir, "telecoms", "telecoms_20250615.csv"))

bldgs <- st_read(file.path(in_dir, "osm_buildings", "osm_buildings_gaza.gpkg"))

evac <- read.csv(file.path(
  here::here(),
  "data",
  "evacuation_orders",
  "evacuation_orders.csv"
))
evac_geo <- st_read(file.path(
  here::here(),
  "data",
  "evacuation_orders",
  "dash_population_blocks.geojson"
))

evac_buffers <- list()
lf <- list.files(file.path("in", "evacuation"))
for (f in lf) {
  date <- sub("^.*_(.*)\\.gpkg$", "\\1", f)
  evac_buffers[[date]] <- st_read(file.path("in", "evacuation", f))
}

# mastergrid
template <- rast(
  ext(gov_geo),
  resolution = grid_size,
  crs = st_crs(gov_geo)$wkt
)

gov_geo$AOI <- 1
mastergrid <- rasterize(vect(gov_geo), template, field = "AOI")

plot(mastergrid)

writeRaster(mastergrid, file.path(out_dir, "mastergrid.tif"), overwrite = TRUE)

# governorate grids
gov_geo <- gov_geo |>
  arrange(PCODE) |>
  mutate(id = 1:n())

gov_grid <- rasterize(vect(gov_geo), mastergrid, field = "id")

plot(gov_grid)
mean(unique(gov_geo$id) %in% gov_grid[])

writeRaster(gov_grid, file.path(out_dir, "gov_grid.tif"), overwrite = TRUE)
st_write(gov_geo, file.path(out_dir, "gov_geo.gpkg"), append = FALSE)
write.csv(
  as.data.frame(gov_geo),
  file.path(out_dir, "gov_geo.csv"),
  row.names = FALSE
)

# municipality grids
mun_geo <- mun_geo |>
  arrange(PCOE_Munic) |>
  mutate(id = 1:n())

mun_grid <- rasterize(vect(mun_geo), mastergrid, field = "id")
coverage <- mean(unique(mun_geo$id) %in% mun_grid[])
while (coverage < 1) {
  mun_geo <- mun_geo |>
    rename(id_old = id)

  n_mun_grid <- length(unique(mun_grid[!is.na(mun_grid)]))
  mun_geo[mun_geo$id_old %in% mun_grid[], "id"] <- 1:n_mun_grid

  mun_grid <- rasterize(vect(mun_geo), mastergrid, field = "id")
  coverage <- mean(
    unique(mun_geo$id[!is.na(mun_geo$id)]) %in% mun_grid[!is.na(mun_grid)]
  )
}

plot(mun_grid)
sort(mun_geo$id)

writeRaster(mun_grid, file.path(out_dir, "mun_grid.tif"), overwrite = TRUE)
st_write(mun_geo, file.path(out_dir, "mun_geo.gpkg"), append = FALSE)
write.csv(
  as.data.frame(mun_geo),
  file.path(out_dir, "mun_geo.csv"),
  row.names = FALSE
)

# neighbourhood grids
nbr_geo <- nbr_geo |>
  arrange(PCODE_Neig) |>
  mutate(id = 1:n())

nbr_grid <- rasterize(vect(nbr_geo), mastergrid, field = "id")
coverage <- mean(unique(nbr_geo$id) %in% nbr_grid[])
while (coverage < 1) {
  nbr_geo <- nbr_geo |>
    rename(id_old = id)

  n_nbr_grid <- length(unique(nbr_grid[!is.na(nbr_grid)]))
  nbr_geo[nbr_geo$id_old %in% nbr_grid[], "id"] <- 1:n_nbr_grid

  nbr_grid <- rasterize(vect(nbr_geo), mastergrid, field = "id")
  coverage <- mean(
    unique(nbr_geo$id[!is.na(nbr_geo$id)]) %in% nbr_grid[!is.na(nbr_grid)]
  )
}

plot(nbr_grid)
sort(nbr_geo$id)

writeRaster(nbr_grid, file.path(out_dir, "nbr_grid.tif"), overwrite = TRUE)
st_write(nbr_geo, file.path(out_dir, "nbr_geo.gpkg"), append = FALSE)
write.csv(
  as.data.frame(nbr_geo),
  file.path(out_dir, "nbr_geo.csv"),
  row.names = FALSE
)

# cleanup telecoms data
telecoms <- telecoms |>
  mutate(
    provider = case_when(
      provider == "P1" ~ 1,
      provider == "P2" ~ 2
    ),
    date = as.Date(date, format = "%m/%d/%Y")
  )

# towers
towers1_geo <- telecoms |>
  filter(provider == 1) |>
  distinct(latitude, longitude, .keep_all = TRUE) |>
  arrange(site_name) |>
  mutate(id = 1:n()) |>
  select(id, provider, site_name, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

towers2_geo <- telecoms |>
  filter(provider == 2) |>
  distinct(latitude, longitude, .keep_all = TRUE) |>
  arrange(site_name) |>
  mutate(id = 1:n()) |>
  select(id, provider, site_name, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

st_write(towers1_geo, file.path(out_dir, "towers1_geo.gpkg"), append = FALSE)
st_write(towers2_geo, file.path(out_dir, "towers2_geo.gpkg"), append = FALSE)

# telco subscribers
telco1 <- telecoms |>
  filter(provider == 1) |>
  left_join(
    towers1_geo |>
      st_drop_geometry() |>
      select(id, longitude, latitude)
  ) |>
  rename(
    tower_id = id,
    subscribers = number_of_subscriptions
  ) |>
  arrange(provider, date, tower_id) |>
  select(provider, tower_id, date, subscribers)

telco2 <- telecoms |>
  filter(provider == 2) |>
  left_join(
    towers2_geo |>
      st_drop_geometry() |>
      select(id, longitude, latitude)
  ) |>
  rename(
    tower_id = id,
    subscribers = number_of_subscriptions
  ) |>
  arrange(provider, date, tower_id) |>
  select(provider, tower_id, date, subscribers)

write.csv(telco1, file.path(out_dir, "telco1.csv"), row.names = FALSE)
write.csv(telco2, file.path(out_dir, "telco2.csv"), row.names = FALSE)

# join telco subscribers (latest date) to towers
date_latest <- max(telco1$date)
telco1_latest <- merge(
  x = towers1_geo,
  y = telco1 |> filter(date == date_latest),
  by.x = "id",
  by.y = "tower_id"
)

date_latest <- max(telco2$date)
telco2_latest <- merge(
  x = towers2_geo,
  y = telco2 |> filter(date == date_latest),
  by.x = "id",
  by.y = "tower_id"
)

st_write(
  telco1_latest,
  file.path(out_dir, "telco1_towers1.gpkg"),
  append = FALSE
)
st_write(
  telco2_latest,
  file.path(out_dir, "telco2_towers2.gpkg"),
  append = FALSE
)


#---- tower-to-pixel distances ----#

grid_coords <- as.data.frame(mastergrid, xy = TRUE)[, c("x", "y")]

towers1_coords <- vect(towers1_geo) |>
  project(mastergrid) |>
  as.data.frame(geom = "XY") |>
  select(x, y)

d1 <- distance(towers1_coords, grid_coords)
rownames(d1) <- towers1_geo$id
colnames(d1) <- 1:ncol(d1)

towers2_coords <- vect(towers2_geo) |>
  project(mastergrid) |>
  as.data.frame(geom = "XY") |>
  select(x, y)

d2 <- distance(towers2_coords, grid_coords)
rownames(d2) <- towers2_geo$id
colnames(d2) <- 1:ncol(d2)

# save to disk
saveRDS(d1, file.path(out_dir, "distance1.rds"))
saveRDS(d2, file.path(out_dir, "distance2.rds"))


#--- tower coverage radii ----#
towers1_dist <- as.matrix(dist(towers1_coords))
towers1_spacing <- c()
for (j in 1:nrow(towers1_coords)) {
  j_dist <- towers1_dist[j, -j]
  towers1_spacing[j] <- mean(sort(j_dist)[1:tower_radius_neighbours]) *
    tower_radius_scaling
}
names(towers1_spacing) <- towers1_geo$id

towers2_dist <- as.matrix(dist(towers2_coords))
towers2_spacing <- c()
for (j in 1:nrow(towers2_coords)) {
  j_dist <- towers2_dist[j, -j]
  towers2_spacing[j] <- mean(sort(j_dist)[1:tower_radius_neighbours]) *
    tower_radius_scaling
}
names(towers2_spacing) <- towers2_geo$id

saveRDS(towers1_spacing, file.path(out_dir, "radius1.rds"))
saveRDS(towers2_spacing, file.path(out_dir, "radius2.rds"))

#---- OSM buildings ----#

# gridded building coverage
bldg_coverage <- rasterize(
  x = bldgs |>
    filter(
      !building %in% c("greenhouse", "greenhouse_horticulture", "water_tower")
    ) |>
    vect() |>
    project(mastergrid),
  y = mastergrid,
  cover = TRUE
)

writeRaster(
  bldg_coverage,
  file.path(out_dir, "osm_building_coverage.tif"),
  overwrite = TRUE
)

#---- evacuation orders ----#
# evacuation blocks
evac_vect <- vect(evac_geo) |>
  project(mastergrid) |>
  st_as_sf() |>
  mutate(COD_mod = as.numeric(as.character(Name))) |>
  vect()

evac_grid <- rasterize(evac_vect, mastergrid, field = "Name")
plot(evac_grid)

writeRaster(evac_grid, file.path(out_dir, "evac_grid.tif"), overwrite = T)

# evacuation buffer zones
dir.create(
  file.path(out_dir, "evacuation_buffers"),
  showWarnings = F,
  recursive = T
)
for (d in names(evac_buffers)) {
  evac_buffer_vect <- vect(evac_buffers[[d]]) |>
    project(mastergrid)

  evac_buffer_vect$id <- 1

  evac_buffer_grid <- rasterize(evac_buffer_vect, mastergrid, field = "id")
  plot(evac_buffer_grid)
  writeRaster(
    evac_buffer_grid,
    file.path(out_dir, "evacuation_buffers", paste0("evac_buffer_", d, ".tif")),
    overwrite = T
  )
}

# evacuation orders
dir.create(
  file.path(out_dir, "evacuation_orders"),
  showWarnings = F,
  recursive = T
)
dates <- sort(unique(evac$date))
for (d in dates) {
  blocks <- evac |>
    filter(date == d) |>
    select(COD_mod) |>
    pull()
  ras <- mastergrid
  ras[ras == 1 & !evac_grid %in% blocks] <- 0
  writeRaster(
    ras,
    file.path(out_dir, "evacuation_orders", paste0("evac_", d, ".tif")),
    overwrite = T
  )
}
