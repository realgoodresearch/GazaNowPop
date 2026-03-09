# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
grid_size <- 100
#----------------------#

# load libraries
library(dplyr)
library(terra)
library(sf)

# encoding for Arabic
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
if (dir.exists(env$wd)) {
  setwd(env$wd)
} else {
  stop("Working directory does not exist.")
}

in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src")
out_dir <- file.path(getwd(), "out", "data")
dir.create(out_dir, showWarnings = F, recursive = T)

#---- load data ----#
telecoms <- read.csv(file.path(in_dir, "telecoms", "telecoms_20260309.csv"))

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

bldgs <- st_read(file.path(in_dir, "osm_buildings", "osm_buildings_gaza.gpkg"))

evac <- read.csv(file.path(
  in_dir,
  "evacuation_orders",
  "evacuation_orders.csv"
))
evac_geo <- st_read(file.path(
  in_dir,
  "evacuation_orders",
  "dash_population_blocks.geojson"
))

evac_buffers <- list()
lf <- list.files(file.path("in", "evacuation_buffers"))
for (f in lf) {
  date <- sub("^.*_(.*)\\.gpkg$", "\\1", f)
  evac_buffers[[date]] <- st_read(file.path("in", "evacuation_buffers", f))
}

st_layers(
  file.path(
    in_dir,
    "unosat",
    "OCHA-CBPF-OPT-031_UNOSAT_Gaza_Strip_CDA_11October2025_GDB_HU",
    "UNOSAT_GazaStrip_CDA_11October2025_HU.gdb"
  )
)
bldg_damage <- st_read(
  file.path(
    in_dir,
    "unosat",
    "OCHA-CBPF-OPT-031_UNOSAT_Gaza_Strip_CDA_11October2025_GDB_HU",
    "UNOSAT_GazaStrip_CDA_11October2025_HU.gdb"
  ),
  layer = "Damage_Sites_GazaStrip_20251011_HU"
)

tent_pnts <- st_read(
  file.path(
    in_dir,
    "tents",
    # "UNOSAT_Gaza_IDP_Tent_points_20251212.gpkg"
    "UNOSAT_Gaza_IDP_Tent_points_20260111.gpkg"
  )
)

#-----------------#

# mastergrid
template <- rast(
  ext(gov_geo),
  resolution = grid_size,
  crs = st_crs(gov_geo)$wkt
)

gov_geo$AOI <- 1
mastergrid <- rasterize(vect(gov_geo), template, field = "AOI")
names(mastergrid) <- "mastergrid"

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
  ) %>%
  filter(latitude < 31.6 & longitude < 34.57) # select inside Gaza

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
  select(provider, tower_id, longitude, latitude, date, subscribers)

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
  select(provider, tower_id, longitude, latitude, date, subscribers)

write.csv(telco1, file.path(out_dir, "telco1.csv"), row.names = FALSE)
write.csv(telco2, file.path(out_dir, "telco2.csv"), row.names = FALSE)


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

#---- building damage ----#

# undamaged housing units
bldg_damage <- bldg_damage %>%
  mutate(
    HU_undamaged_14 = HU_tot - HU_damaged_14,
    HU_undamaged_prop_14 = HU_undamaged_14 / HU_tot
  )

housing <- rasterize(
  x = vect(bldg_damage) %>% project(mastergrid),
  y = mastergrid,
  field = "HU_undamaged_14",
  fun = sum,
  na.rm = TRUE
)
names(housing) <- "housing"

housing
plot(housing)

writeRaster(
  housing,
  file.path(out_dir, "housing.tif"),
  overwrite = TRUE
)


housing_prop <- rasterize(
  x = vect(bldg_damage) %>% project(mastergrid),
  y = mastergrid,
  field = "HU_undamaged_prop_14",
  fun = mean,
  na.rm = TRUE
)
names(housing_prop) <- "housing_proportion_undamaged"

housing_prop
plot(housing_prop)

writeRaster(
  housing_prop,
  file.path(out_dir, "housing_proportion_undamaged.tif"),
  overwrite = TRUE
)


# proportion destroyed or severely damaged buildings
bldg_damage <- bldg_damage %>%
  mutate(
    destroyed_or_severely_damaged_14 = ifelse(
      Main_Damage_Site_Class_14 %in% 1:2,
      1,
      0
    )
  )

bldg_destroyed <- rasterize(
  x = vect(bldg_damage) %>% project(mastergrid),
  y = mastergrid,
  field = "destroyed_or_severely_damaged_14",
  fun = mean,
  na.rm = TRUE
)
names(bldg_destroyed) <- "bldg_destroyed"

bldg_destroyed
plot(bldg_destroyed > 0.9)

writeRaster(
  bldg_destroyed,
  file.path(out_dir, "bldg_destroyed.tif"),
  overwrite = TRUE
)


# # tents extent
# tents <- terra::rasterize(
#   x = vect(tents_extent) %>%
#     project(mastergrid),
#   y = mastergrid,
#   cover = TRUE
# )
# plot(tents)

# writeRaster(
#   tents,
#   filename = file.path(out_dir, "tents_cover.tif"),
#   overwrite = TRUE
# )

# tent count
tents <- terra::rasterize(
  x = tent_pnts %>% mutate(count = 1) %>% vect() %>% project(mastergrid),
  y = mastergrid,
  field = "count",
  fun = sum
)

plot(tents)

writeRaster(
  tents,
  filename = file.path(out_dir, "tent_count.tif"),
  overwrite = TRUE
)
