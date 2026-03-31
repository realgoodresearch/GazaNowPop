# cleanup
rm(list = ls())
gc()

# load libraries
library(here)
library(httr)
library(sf)
library(terra)

# load environment
env <- new.env()
source(here::here(".env"), local = env)
source(here::here("src", "bayesian", "00_fun.R"))

#---- USER OPTIONS ----#

# reference date
reference_date <- as.Date("2026-03-24")

# model name
model_name <- "v0.13"

# command line arguments can override defaults
args <- commandArgs(trailingOnly = TRUE)
model_name <- if (length(args) >= 1) args[[1]] else model_name
reference_date <- if (length(args) >= 2) as.Date(args[[2]]) else reference_date

#----------------------#

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "wd"))

log_message("Starting maps", model_name)

model_dir <- file.path(env$wd, "out", "bayesian", model_name, reference_date)
eval_dir <- file.path(model_dir, "eval")
maps_dir <- file.path(eval_dir, "maps")
admin_dir <- file.path(env$wd, "in", "admin_boundaries", "OCHA")

dir.create(maps_dir, showWarnings = FALSE, recursive = TRUE)

resolve_raster_path <- function(directory, candidates) {
  matches <- file.path(directory, candidates)
  matches <- matches[file.exists(matches)]

  if (length(matches) == 0) {
    stop(
      paste(
        "Could not find any of these rasters in",
        directory,
        ":",
        paste(candidates, collapse = ", ")
      )
    )
  }

  matches[[1]]
}

load_admin_boundaries <- function(target_crs) {
  governorates <- st_read(
    file.path(admin_dir, "Governorates", "Governorates_Population_polygons.shp"),
    quiet = TRUE
  ) %>%
    st_make_valid() %>%
    st_transform(target_crs)

  municipalities <- st_read(
    file.path(admin_dir, "Municipalities", "GazaMunicipalities.shp"),
    quiet = TRUE
  ) %>%
    st_make_valid() %>%
    st_transform(target_crs)

  neighbourhoods <- st_read(
    file.path(admin_dir, "Neighbourhoods", "Neighbourhoods_Population_polygons.shp"),
    quiet = TRUE
  ) %>%
    st_make_valid() %>%
    st_transform(target_crs)

  list(
    governorates = governorates,
    municipalities = municipalities,
    neighbourhoods = neighbourhoods
  )
}

lon_to_tile_x <- function(lon, zoom) {
  floor((lon + 180) / 360 * 2^zoom)
}

lat_to_tile_y <- function(lat, zoom) {
  lat_rad <- lat * pi / 180
  floor(
    (1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * 2^zoom
  )
}

tile_bounds_mercator <- function(x, y, zoom) {
  origin_shift <- 20037508.342789244
  tile_size <- 2 * origin_shift / 2^zoom

  xmin <- -origin_shift + x * tile_size
  xmax <- xmin + tile_size
  ymax <- origin_shift - y * tile_size
  ymin <- ymax - tile_size

  c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

fetch_esri_imagery <- function(reference_raster, cache_path, zoom = 13) {
  if (!file.exists(cache_path)) {
    reference_raster_3857 <- terra::project(reference_raster, "EPSG:3857")
    bbox_ll <- sf::st_as_sfc(
      sf::st_bbox(terra::as.polygons(terra::ext(reference_raster), crs = terra::crs(reference_raster)))
    ) %>%
      sf::st_transform(4326) %>%
      sf::st_bbox()

    x_min <- lon_to_tile_x(bbox_ll["xmin"], zoom)
    x_max <- lon_to_tile_x(bbox_ll["xmax"], zoom)
    y_min <- lat_to_tile_y(bbox_ll["ymax"], zoom)
    y_max <- lat_to_tile_y(bbox_ll["ymin"], zoom)
    max_tile <- 2^zoom - 1

    x_min <- max(0, x_min - 1)
    x_max <- min(max_tile, x_max + 1)
    y_min <- max(0, y_min - 1)
    y_max <- min(max_tile, y_max + 1)

    tile_dir <- tempfile(pattern = "esri_tiles_")
    dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)

    tile_rasters <- list()

    for (x in x_min:x_max) {
      for (y in y_min:y_max) {
        tile_path <- file.path(tile_dir, paste0("z", zoom, "_x", x, "_y", y, ".jpg"))
        tile_url <- paste0(
          "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/",
          zoom, "/", y, "/", x
        )

        response <- httr::GET(tile_url, httr::write_disk(tile_path, overwrite = TRUE))
        httr::stop_for_status(response)

        tile_raster <- terra::rast(tile_path)
        tile_extent <- tile_bounds_mercator(x, y, zoom)
        terra::ext(tile_raster) <- tile_extent
        terra::crs(tile_raster) <- "EPSG:3857"
        tile_rasters[[length(tile_rasters) + 1]] <- tile_raster
      }
    }

    basemap_3857 <- do.call(terra::merge, tile_rasters)
    basemap_3857 <- terra::crop(basemap_3857, reference_raster_3857)
    terra::writeRaster(basemap_3857, cache_path, overwrite = TRUE)
  }

  terra::rast(cache_path)
}

plot_colors <- c(
  rgb(48, 18, 59, maxColorValue = 255),
  rgb(70, 98, 216, maxColorValue = 255),
  rgb(53, 171, 248, maxColorValue = 255),
  rgb(27, 229, 181, maxColorValue = 255),
  rgb(116, 254, 93, maxColorValue = 255),
  rgb(201, 239, 52, maxColorValue = 255),
  rgb(251, 185, 56, maxColorValue = 255),
  rgb(245, 105, 24, maxColorValue = 255),
  rgb(201, 41, 3, maxColorValue = 255),
  rgb(122, 4, 3, maxColorValue = 255)
)
plot_values <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 1000)
plot_palette <- scales::gradient_n_pal(
  plot_colors,
  values = scales::rescale(plot_values, to = c(0, 1), from = c(0, 1000))
)(seq(0, 1, length.out = 256))

write_population_map <- function(
  raster_path,
  title,
  out_path,
  basemap,
  boundaries,
  subtitle
) {
  pop_raster <- terra::rast(raster_path)
  pop_raster <- terra::project(pop_raster, basemap[[1]], method = "bilinear")

  png(filename = out_path, width = 1800, height = 1800, res = 220)
  on.exit(dev.off(), add = TRUE)

  par(mar = c(1, 1, 4.8, 5.8), xaxs = "i", yaxs = "i")

  terra::plotRGB(
    basemap,
    r = 1,
    g = 2,
    b = 3,
    stretch = "lin",
    axes = FALSE,
    mar = c(1, 1, 4.8, 5.8),
    plg = FALSE,
    main = ""
  )

  mtext(title, side = 3, line = 2.3, cex = 1.35, font = 2)
  mtext(subtitle, side = 3, line = 0.9, cex = 0.95)

  terra::plot(
    pop_raster,
    add = TRUE,
    col = adjustcolor(plot_palette, alpha.f = 0.62),
    range = c(0, 1000),
    axes = FALSE,
    legend = TRUE,
    plg = list(
      title = "People",
      cex = 0.9,
      x = "right",
      title.cex = 0.9,
      at = plot_values,
      labels = format(plot_values, trim = TRUE)
    )
  )

  terra::plot(
    terra::vect(boundaries$neighbourhoods),
    add = TRUE,
    border = adjustcolor("white", alpha.f = 0.25),
    lwd = 0.2
  )
  terra::plot(
    terra::vect(boundaries$municipalities),
    add = TRUE,
    border = adjustcolor("white", alpha.f = 0.55),
    lwd = 0.55
  )
  terra::plot(
    terra::vect(boundaries$governorates),
    add = TRUE,
    border = adjustcolor("white", alpha.f = 0.9),
    lwd = 1.2
  )
}

subtitle_text <- paste("Full model", model_name, "-", format(reference_date))

map_specs <- list(
  list(
    candidates = c("N_hat.tif"),
    title = "Estimated Population",
    out_name = "N_hat_map.png"
  ),
  list(
    candidates = c("pop_in_tents.tif", "pop_in_tents_hat.tif"),
    title = "Estimated Population in Tents",
    out_name = "pop_in_tents_map.png"
  ),
  list(
    candidates = c("pop_in_bldgs.tif", "pop_in_bldgs_hat.tif"),
    title = "Estimated Population in Buildings",
    out_name = "pop_in_bldgs_map.png"
  )
)

reference_raster_path <- resolve_raster_path(eval_dir, map_specs[[1]]$candidates)
reference_raster <- terra::rast(reference_raster_path)
plot_crs <- sf::st_crs(terra::crs(reference_raster))

basemap_cache_path <- file.path(
  maps_dir,
  paste0("esri_world_imagery_tiles_z13_pad1_3857_", model_name, "_", reference_date, ".tif")
)

basemap <- fetch_esri_imagery(
  reference_raster = reference_raster,
  cache_path = basemap_cache_path
)
log_message("Fetched ESRI imagery basemap", model_name)
boundaries <- load_admin_boundaries(sf::st_crs(terra::crs(basemap)))

for (map_spec in map_specs) {
  raster_path <- resolve_raster_path(eval_dir, map_spec$candidates)
  out_path <- file.path(maps_dir, map_spec$out_name)

  write_population_map(
    raster_path = raster_path,
    title = map_spec$title,
    out_path = out_path,
    basemap = basemap,
    boundaries = boundaries,
    subtitle = subtitle_text
  )

  log_message(paste("Wrote map", basename(out_path)), model_name)
}

log_message("Finished maps", model_name)
