library(terra)
library(sf)
library(dplyr)

# function to join tower attributes to voronoi polygons of coverage areas
join_towers_to_voronoi <- function(towers, voronoi) {
  # list of which polygon contains each point
  contains_list <- st_within(towers, voronoi)

  # index of polygon for each point
  poly_idx_for_point <- sapply(contains_list, function(x) {
    if (length(x) > 0) x[1] else NA_integer_
  })

  # assign first point if more than one in a polygon
  mapping_df <- data.frame(
    poly_index = poly_idx_for_point,
    tower_index = seq_len(nrow(towers)),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(poly_index))

  # handle polygons that remain unmatched (if any)
  all_poly_idx <- seq_len(length(voronoi))
  matched_poly_idx <- unique(mapping_df$poly_index)
  unmatched_poly_idx <- setdiff(all_poly_idx, matched_poly_idx)

  if (length(unmatched_poly_idx) > 0) {
    # find nearest tower for each unmatched polygon centroid
    poly_centroids <- st_centroid(voronoi[unmatched_poly_idx])
    nearest_tower_idx <- st_nearest_feature(poly_centroids, towers)
    # add to mapping_df
    add_df <- data.frame(
      poly_index = unmatched_poly_idx,
      tower_index = nearest_tower_idx,
      stringsAsFactors = FALSE
    )
    mapping_df <- bind_rows(mapping_df, add_df)
  }

  # create final voronoi sf with tower attributes with one tower per polygon
  mapping_df <- mapping_df %>% group_by(poly_index) %>% slice(1) %>% ungroup()

  # reorder/attach attributes: create an sf of polygons with an index
  vor_sf <- st_as_sf(
    data.frame(poly_index = seq_along(voronoi)),
    geometry = st_sfc(voronoi),
    crs = st_crs(voronoi)
  )

  # join mapping to vor_sf then bring tower attributes across
  voronoi_attr <- vor_sf %>%
    left_join(mapping_df, by = "poly_index") %>%
    left_join(
      st_drop_geometry(towers) %>% mutate(tower_index = row_number()),
      by = "tower_index",
      suffix = c("", "_tower")
    )

  return(voronoi_attr)
}

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

select_md_fields <- function(md, fields) {
  md_selected <- md[fields]

  for (attr_name in c("covariate_names", "covariate_means", "covariate_sds")) {
    attr(md_selected, attr_name) <- attr(md, attr_name)
  }

  md_selected
}

build_model_data <- function(
  reference_date = NULL,
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
  covariate_rasters = NULL,
  evac_buffer = rast(file.path(
    data_dir,
    "evacuation_buffers",
    "evac_buffer_2025-10-10.tif"
  )),
  mastergrid = rast(file.path(data_dir, "mastergrid.tif")),
  s_rho = NULL,
  N_tot = 2.1e6,
  compact_admin_ids = TRUE,
  write_outputs = FALSE,
  output_dir = NULL,
  enrich_tower_voronoi = FALSE,
  seed = round(runif(1, 0, 1e9))
) {
  set.seed(seed)

  telco1$date <- as.Date(telco1$date)
  telco2$date <- as.Date(telco2$date)

  if (is.null(reference_date)) {
    telco_date <- max(telco1$date)
  } else {
    telco_date <- as.Date(reference_date)
  }

  telco1 <- telco1 %>%
    filter(date == telco_date)

  telco2 <- telco2 %>%
    filter(date == telco_date)

  if (nrow(telco1) == 0 || nrow(telco2) == 0) {
    stop(
      paste0(
        "No telecom data found for reference_date = ",
        as.character(telco_date),
        "."
      )
    )
  }

  towers1_geo <- st_as_sf(towers1) %>%
    rename(tower_id = id) %>%
    filter(tower_id %in% telco1$tower_id)
  towers2_geo <- st_as_sf(towers2) %>%
    rename(tower_id = id) %>%
    filter(tower_id %in% telco2$tower_id)

  tents[mastergrid == 1 & is.na(tents)] <- 0
  housing[mastergrid == 1 & is.na(housing)] <- 0
  evac_buffer[mastergrid == 1 & is.na(evac_buffer)] <- 0

  mastergrid_idx <- mastergrid == 1 &
    evac_buffer == 0 &
    (tents > 0 | housing > 0)

  mastergrid_idx_vect <- which(as.vector(
    mastergrid[] == 1 &
      evac_buffer[] == 0 &
      (tents[] > 0 | housing[] > 0)
  ))

  tent_vect <- as.vector(tents[mastergrid_idx])
  housing_vect <- as.vector(housing[mastergrid_idx])

  if (is.null(covariate_rasters)) {
    covariate_names <- character(0)
    X <- matrix(0, nrow = length(tent_vect), ncol = 0)
    storage.mode(X) <- "double"
    covariate_means <- numeric(0)
    covariate_sds <- numeric(0)
  } else {
    covariate_names <- names(covariate_rasters)
    covariate_values <- lapply(covariate_rasters, function(x) {
      as.vector(x[mastergrid_idx])
    })
    covariate_scaled <- lapply(covariate_values, scale_covariate)

    X <- do.call(cbind, lapply(covariate_scaled, function(x) x$values))
    storage.mode(X) <- "double"

    covariate_means <- sapply(covariate_scaled, function(x) x$mean)
    covariate_sds <- sapply(covariate_scaled, function(x) x$sd)
  }

  grid_coords <- xyFromCell(mastergrid, mastergrid_idx_vect) %>%
    as.data.frame() %>%
    setNames(c("x", "y"))

  towers1_coords <- towers1_geo %>%
    st_transform(crs = st_crs(mastergrid)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    select(x = X, y = Y)

  towers2_coords <- towers2_geo %>%
    st_transform(crs = st_crs(mastergrid)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    select(x = X, y = Y)

  d1 <- distance(towers1_coords, grid_coords)
  d2 <- distance(towers2_coords, grid_coords)

  env <- st_union(st_as_sf(gov_geo)) %>%
    st_make_valid() %>%
    st_transform(crs = "EPSG: 32636")

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

  if (enrich_tower_voronoi) {
    towers1_voronoi <- towers1_voronoi %>%
      mutate(provider = 1L) %>%
      left_join(
        telco1 %>% select(tower_id, subscribers),
        by = "tower_id"
      ) %>%
      select(
        provider,
        tower_id,
        tower_index,
        site_name,
        subscribers,
        longitude,
        latitude,
        geometry
      )

    towers2_voronoi <- towers2_voronoi %>%
      mutate(provider = 2L) %>%
      left_join(
        telco2 %>% select(tower_id, subscribers),
        by = "tower_id"
      ) %>%
      select(
        provider,
        tower_id,
        tower_index,
        site_name,
        subscribers,
        longitude,
        latitude,
        geometry
      )
  }

  if (write_outputs) {
    if (is.null(output_dir)) {
      stop("`output_dir` must be supplied when `write_outputs = TRUE`.")
    }

    writeVector(
      vect(st_transform(towers1_voronoi, crs = crs(mastergrid))),
      file.path(output_dir, "towers1_voronoi.gpkg"),
      overwrite = TRUE
    )

    writeVector(
      vect(st_transform(towers2_voronoi, crs = crs(mastergrid))),
      file.path(output_dir, "towers2_voronoi.gpkg"),
      overwrite = TRUE
    )
  }

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

  if (write_outputs) {
    writeRaster(
      towers1_zones,
      file.path(output_dir, "towers1_zones.tif"),
      overwrite = TRUE
    )

    writeRaster(
      towers2_zones,
      file.path(output_dir, "towers2_zones.tif"),
      overwrite = TRUE
    )
  }

  tower1_vect <- as.vector(towers1_zones[mastergrid_idx])
  tower2_vect <- as.vector(towers2_zones[mastergrid_idx])

  J1 <- max(tower1_vect)
  J2 <- max(tower2_vect)

  I_j1 <- integer(J1)
  for (j in 1:J1) {
    I_j1[j] <- sum(tower1_vect == j)
  }

  I_j2 <- integer(J2)
  for (j in 1:J2) {
    I_j2[j] <- sum(tower2_vect == j)
  }

  grids_by_tower1 <- matrix(0, nrow = J1, ncol = max(I_j1))
  for (j in 1:J1) {
    grids_by_tower1[j, seq_len(I_j1[j])] <- which(tower1_vect == j)
  }

  grids_by_tower2 <- matrix(0, nrow = J2, ncol = max(I_j2))
  for (j in 1:J2) {
    grids_by_tower2[j, seq_len(I_j2[j])] <- which(tower2_vect == j)
  }

  gov_vect_raw <- as.vector(gov_grid[mastergrid_idx])
  mun_vect_raw <- as.vector(mun_grid[mastergrid_idx])
  nbr_vect_raw <- as.vector(nbr_grid[mastergrid_idx])

  if (compact_admin_ids) {
    gov_ids <- sort(unique(gov_vect_raw))
    mun_ids <- sort(unique(mun_vect_raw))
    nbr_ids <- sort(unique(nbr_vect_raw))

    gg <- match(gov_vect_raw, gov_ids)
    mm <- match(mun_vect_raw, mun_ids)
    hh <- match(nbr_vect_raw, nbr_ids)

    G <- length(gov_ids)
    M <- length(mun_ids)
    H <- length(nbr_ids)

    gov_of_mun <- integer(M)
    for (m in 1:M) {
      gov_of_mun[m] <- gg[match(m, mm)]
    }

    mun_of_nbr <- integer(H)
    for (h in 1:H) {
      mun_of_nbr[h] <- mm[match(h, hh)]
    }
  } else {
    gg <- gov_vect_raw
    mm <- mun_vect_raw
    hh <- nbr_vect_raw

    G <- max(gg)
    M <- max(mm)
    H <- max(hh)

    gov_ids <- sort(unique(gov_vect_raw))
    mun_ids <- sort(unique(mun_vect_raw))
    nbr_ids <- sort(unique(nbr_vect_raw))
    gov_of_mun <- NULL
    mun_of_nbr <- NULL
  }

  I_g <- integer(G)
  for (g in 1:G) {
    I_g[g] <- sum(gg == g)
  }

  grids_by_gov <- matrix(0, nrow = G, ncol = max(I_g))
  for (g in 1:G) {
    grids_by_gov[g, seq_len(I_g[g])] <- which(gg == g)
  }

  I_m <- integer(M)
  for (m in 1:M) {
    I_m[m] <- sum(mm == m)
  }

  grids_by_mun <- matrix(0, nrow = M, ncol = max(I_m))
  for (m in 1:M) {
    grids <- which(mm == m)
    if (length(grids) > 0) {
      grids_by_mun[m, seq_len(I_m[m])] <- grids
    }
  }

  I_h <- integer(H)
  for (h in 1:H) {
    I_h[h] <- sum(hh == h)
  }

  grids_by_nbr <- matrix(0, nrow = H, ncol = max(I_h))
  for (h in 1:H) {
    grids <- which(hh == h)
    if (length(grids) > 0) {
      grids_by_nbr[h, seq_len(I_h[h])] <- grids
    }
  }

  telco1 <- telco1 %>%
    left_join(
      towers1_voronoi %>% st_drop_geometry() %>% select(tower_id, tower_index),
      by = "tower_id"
    ) %>%
    arrange(tower_index)

  telco2 <- telco2 %>%
    left_join(
      towers2_voronoi %>% st_drop_geometry() %>% select(tower_id, tower_index),
      by = "tower_id"
    ) %>%
    arrange(tower_index)

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
    gg = gg,
    mm = mm,
    hh = hh,
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
    N_tot = N_tot,
    tower1_id = telco1$tower_id,
    tower2_id = telco2$tower_id,
    y1 = round(telco1$subscribers),
    y2 = round(telco2$subscribers),
    mastergrid_idx = mastergrid_idx_vect,
    seed = seed
  )

  attr(md, "reference_date") <- reference_date
  attr(md, "covariate_names") <- covariate_names
  attr(md, "covariate_means") <- covariate_means
  attr(md, "covariate_sds") <- covariate_sds

  return(md)
}
