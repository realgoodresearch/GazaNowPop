# libraries
library(terra)
library(dplyr)
library(sf)

seed <- round(runif(1, 0, 1e9))
set.seed(seed)

# directories
data_dir <- file.path(env$wd, "out", "data")

# fixed distance-decay scale in meters for logistic attenuation
s_rho <- 250

scale_covariate <- function(x) {
  x[is.na(x)] <- 0
  mu <- mean(x)
  s <- sd(x)
  if (is.na(s) || s == 0) {
    z <- x - mu
    s <- 1
  } else {
    z <- (x - mu) / s
  }

  list(values = z, mean = mu, sd = s)
}

# create model data object
model_data <- function(
  telco1 = read.csv(file.path(data_dir, "telco1.csv")),
  telco2 = read.csv(file.path(data_dir, "telco2.csv")),
  towers1 = vect(file.path(data_dir, "towers1_geo.gpkg")),
  towers2 = vect(file.path(data_dir, "towers2_geo.gpkg")),
  mun_grid = rast(file.path(data_dir, "mun_grid.tif")),
  nbr_grid = rast(file.path(data_dir, "nbr_grid.tif")),
  gov_grid = rast(file.path(data_dir, "gov_grid.tif")),
  gov_geo = vect(file.path(data_dir, "gov_geo.gpkg")),
  tents = rast(file.path(data_dir, "tent_count.tif")),
  housing = rast(file.path(data_dir, "housing.tif")),
  prop_bldg_destroyed_500m = rast(file.path(data_dir, "prop_bldg_destroyed_500m.tif")),
  housing_500m = rast(file.path(data_dir, "housing_500m.tif")),
  tents_500m = rast(file.path(data_dir, "tents_500m.tif")),
  osm_building_coverage_500m = rast(file.path(data_dir, "osm_building_coverage_500m.tif")),
  evac_order_count_500m = rast(file.path(data_dir, "evac_order_count_500m.tif")),
  evac_buffer = rast(file.path(
    data_dir,
    "evacuation_buffers",
    "evac_buffer_2025-10-10.tif"
  )),
  mastergrid = rast(file.path(data_dir, "mastergrid.tif"))
) {
  # subset telecoms to most recent date
  telco_date <- max(telco1$date)

  telco1 <- telco1 |>
    filter(date == telco_date)

  telco2 <- telco2 |>
    filter(date == telco_date)

  # subset towers to those in telecoms data
  towers1_geo <- st_as_sf(towers1) %>%
    rename(tower_id = id) %>%
    filter(tower_id %in% telco1$tower_id)
  towers2_geo <- st_as_sf(towers2) %>%
    rename(tower_id = id) %>%
    filter(tower_id %in% telco2$tower_id)

  #---- tents and housing ----#
  tents[mastergrid == 1 & is.na(tents)] <- 0
  housing[mastergrid == 1 & is.na(housing)] <- 0
  evac_buffer[mastergrid == 1 & is.na(evac_buffer)] <- 0

  mastergrid_idx <- mastergrid == 1 &
    evac_buffer == 0 &
    (tents > 0 | housing > 0)

  mastergrid_idx_vect <- which(as.vector(mastergrid_idx[]))

  tent_vect <- as.vector(tents[mastergrid_idx])
  housing_vect <- as.vector(housing[mastergrid_idx])

  #---- grid-level covariates ----#
  covariate_names <- c(
    "prop_bldg_destroyed_500m",
    "housing_500m",
    "tents_500m",
    "osm_building_coverage_500m",
    "evac_order_count_500m"
  )

  covariate_rasters <- list(
    prop_bldg_destroyed_500m = prop_bldg_destroyed_500m,
    housing_500m = housing_500m,
    tents_500m = tents_500m,
    osm_building_coverage_500m = osm_building_coverage_500m,
    evac_order_count_500m = evac_order_count_500m
  )

  covariate_values <- lapply(covariate_rasters, function(x) {
    as.vector(x[mastergrid_idx])
  })

  covariate_scaled <- lapply(covariate_values, scale_covariate)

  X <- do.call(
    cbind,
    lapply(covariate_scaled, function(x) x$values)
  )
  storage.mode(X) <- "double"

  covariate_means <- sapply(covariate_scaled, function(x) x$mean)
  covariate_sds <- sapply(covariate_scaled, function(x) x$sd)

  #---- pixel-to-tower distances ----#
  grid_coords <- xyFromCell(mastergrid, mastergrid_idx_vect) %>%
    as.data.frame() %>%
    setNames(c("x", "y"))

  towers1_coords <- towers1_geo %>%
    st_transform(crs = st_crs(mastergrid)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    select(x = X, y = Y)

  d1 <- distance(towers1_coords, grid_coords)

  towers2_coords <- towers2_geo |>
    st_transform(crs = st_crs(mastergrid)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    select(x = X, y = Y)

  d2 <- distance(towers2_coords, grid_coords)

  #---- tower voronoi ----#
  env <- st_union(st_as_sf(gov_geo)) %>%
    st_make_valid() %>%
    st_transform(crs = "EPSG: 32636")

  # provider 1
  pnt1 <- towers1_geo %>%
    st_union() %>%
    st_make_valid() %>%
    st_transform(crs = "EPSG:32636")

  voronoi1 <- st_voronoi(pnt1, env) %>%
    st_collection_extract("POLYGON") %>%
    st_sfc() %>%
    st_intersection(env) %>%
    st_make_valid() %>%
    st_transform(st_crs(towers1_geo))

  # provider 2
  pnt2 <- towers2_geo %>%
    st_union() %>%
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

  # tower grids
  towers1_zones <- terra::rasterize(
    x = towers1_voronoi %>%
      st_transform(crs = crs(mastergrid)) %>%
      vect(),
    y = rast(mastergrid),
    field = "tower_index",
    background = NA
  )

  towers2_zones <- terra::rasterize(
    x = towers2_voronoi %>%
      st_transform(crs = crs(mastergrid)) %>%
      vect(),
    y = rast(mastergrid),
    field = "tower_index",
    background = NA
  )

  tower1_vect <- as.vector(towers1_zones[mastergrid_idx])
  tower2_vect <- as.vector(towers2_zones[mastergrid_idx])

  J1 <- max(tower1_vect)
  J2 <- max(tower2_vect)

  I_j1 <- c()
  for (j in 1:J1) {
    I_j1[j] <- sum(tower1_vect == j)
  }

  I_j2 <- c()
  for (j in 1:J2) {
    I_j2[j] <- sum(tower2_vect == j)
  }

  grids_by_tower1 <- matrix(0, nrow = J1, ncol = max(I_j1))
  for (j in 1:J1) {
    grids_by_tower1[j, 1:I_j1[j]] <- which(tower1_vect == j)
  }

  grids_by_tower2 <- matrix(0, nrow = J2, ncol = max(I_j2))
  for (j in 1:J2) {
    grids_by_tower2[j, 1:I_j2[j]] <- which(tower2_vect == j)
  }

  #---- admin units ----#

  # grid count per governorate
  gov_vect <- as.vector(gov_grid[mastergrid_idx])
  gov_ids <- sort(unique(gov_vect))

  G <- length(gov_ids)
  gov_vect_idx <- match(gov_vect, gov_ids)

  I_g <- c()
  for (g in 1:G) {
    I_g[g] <- sum(gov_vect_idx == g)
  }

  grids_by_gov <- matrix(0, nrow = G, ncol = max(I_g))
  for (g in 1:G) {
    grids_by_gov[g, 1:I_g[g]] <- which(gov_vect_idx == g)
  }

  # grid count per municipality
  mun_vect <- as.vector(mun_grid[mastergrid_idx])
  mun_ids <- sort(unique(mun_vect))
  M <- length(mun_ids)
  mun_vect_idx <- match(mun_vect, mun_ids)

  I_m <- c()
  for (m in 1:M) {
    I_m[m] <- sum(mun_vect_idx == m)
  }

  grids_by_mun <- matrix(0, nrow = M, ncol = max(I_m))
  for (m in 1:M) {
    grids <- which(mun_vect_idx == m)
    if (length(grids) > 0) {
      grids_by_mun[m, 1:I_m[m]] <- grids
    }
  }

  gov_of_mun <- integer(M)
  for (m in 1:M) {
    gov_of_mun[m] <- gov_vect_idx[match(m, mun_vect_idx)]
  }

  # grid count per neighbourhood
  nbr_vect <- as.vector(nbr_grid[mastergrid_idx])
  nbr_ids <- sort(unique(nbr_vect))
  nbr_vect_idx <- match(nbr_vect, nbr_ids)

  H <- length(nbr_ids)

  I_h <- c()
  for (h in 1:H) {
    I_h[h] <- sum(nbr_vect_idx == h)
  }

  grids_by_nbr <- matrix(0, nrow = H, ncol = max(I_h))
  for (h in 1:H) {
    grids <- which(nbr_vect_idx == h)
    if (length(grids) > 0) {
      grids_by_nbr[h, 1:I_h[h]] <- which(nbr_vect_idx == h)
    }
  }

  mun_of_nbr <- integer(H)
  for (h in 1:H) {
    mun_of_nbr[h] <- mun_vect_idx[match(h, nbr_vect_idx)]
  }

  #---- confirm ordering for towers and subscribers ----#
  telco1 <- telco1 %>%
    left_join(
      towers1_voronoi %>% st_drop_geometry() %>% select(tower_id, tower_index)
    ) %>%
    arrange(tower_index)

  telco2 <- telco2 %>%
    left_join(
      towers2_voronoi %>% st_drop_geometry() %>% select(tower_id, tower_index)
    ) %>%
    arrange(tower_index)

  # model data
  md <- list(
    I = sum(mastergrid_idx[], na.rm = TRUE),
    G = G,
    M = M,
    H = H,
    J1 = J1,
    J2 = J2,
    K = ncol(X),
    I_g = I_g,
    I_m = I_m,
    I_h = I_h,
    I_j1 = I_j1,
    I_j2 = I_j2,
    grids_by_gov = grids_by_gov,
    grids_by_mun = grids_by_mun,
    grids_by_nbr = grids_by_nbr,
    grids_by_tower1 = grids_by_tower1,
    grids_by_tower2 = grids_by_tower2,
    gg = gov_vect_idx,
    mm = mun_vect_idx,
    hh = nbr_vect_idx,
    jj1 = tower1_vect,
    jj2 = tower2_vect,
    gov_of_mun = gov_of_mun,
    mun_of_nbr = mun_of_nbr,
    mun_ids = mun_ids,
    nbr_ids = nbr_ids,
    d1 = d1,
    d2 = d2,
    X = X,
    tents = tent_vect,
    housing = housing_vect,
    s_rho = s_rho,
    N_tot = 2.1e6,
    y1 = round(telco1$subscribers),
    y2 = round(telco2$subscribers),
    mastergrid_idx = mastergrid_idx_vect,
    seed = seed
  )

  attr(md, "covariate_names") <- covariate_names
  attr(md, "covariate_means") <- covariate_means
  attr(md, "covariate_sds") <- covariate_sds

  return(md)
}

init_generator <- function(md) {
  list(
    kappa1 = exp(rnorm(1, log(10), 0.1)),
    kappa2 = exp(rnorm(1, log(10), 0.1)),
    alpha_rho1 = rnorm(1, log(0.4), 0.1),
    alpha_rho2 = rnorm(1, log(0.2), 0.1),
    sigma_rho1 = exp(rnorm(1, log(0.05), 0.1)),
    sigma_rho2 = exp(rnorm(1, log(0.05), 0.1)),
    radius_rho1 = exp(rnorm(1, log(3000), 0.1)),
    radius_rho2 = exp(rnorm(1, log(3000), 0.1)),
    z_rho1 = rnorm(md$J1, 0, 0.1),
    z_rho2 = rnorm(md$J2, 0, 0.1),
    alpha_phi_tents = rnorm(1, log(10), 0.1),
    sigma_gov_phi_tents = exp(rnorm(1, log(0.05), 0.1)),
    sigma_mun_phi_tents = exp(rnorm(1, log(0.05), 0.1)),
    z_gov_phi_tents = rnorm(md$G, 0, 0.1),
    z_mun_phi_tents = rnorm(md$M, 0, 0.1),
    beta_tents = rnorm(md$K, 0, 0.1),
    alpha_phi_housing = rnorm(1, log(10), 0.1),
    sigma_gov_phi_housing = exp(rnorm(1, log(0.05), 0.1)),
    sigma_mun_phi_housing = exp(rnorm(1, log(0.05), 0.1)),
    z_gov_phi_housing = rnorm(md$G, 0, 0.1),
    z_mun_phi_housing = rnorm(md$M, 0, 0.1),
    beta_housing = rnorm(md$K, 0, 0.1)
  )
}
