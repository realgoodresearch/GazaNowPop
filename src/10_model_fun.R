# create mask
build_mask <- function(ref_date, evac, evac_buffers, bldg_mask, mastergrid) {
  blocks <- evac |>
    filter(date <= ref_date & date_end >= ref_date) |>
    select(COD_mod) |>
    pull()

  if (length(blocks) == 0) {
    evac_mat <- matrix(1, nrow = nrow(mastergrid), ncol = ncol(mastergrid))
  } else {
    evac_ras <- mastergrid
    evac_ras[mastergrid == 1 & evac_grid %in% blocks] <- 0
    evac_mat <- matrix(
      evac_ras[],
      nrow = nrow(mastergrid),
      ncol = ncol(mastergrid)
    )
  }

  buffer_dates <- names(evac_buffers)
  buffer_date <- max(buffer_dates[buffer_dates <= ref_date])
  if (is.na(buffer_date)) {
    buffer_mat <- matrix(1, nrow = nrow(mastergrid), ncol = ncol(mastergrid))
  } else {
    buffer_ras <- evac_buffers[[buffer_date]]
    buffer_ras[buffer_ras == 1] <- 0
    buffer_ras[is.na(buffer_ras) & mastergrid == 1] <- 1
    buffer_mat <- matrix(
      buffer_ras[],
      nrow = nrow(mastergrid),
      ncol = ncol(mastergrid)
    )
  }

  result <- rast(mastergrid)
  values(result) <- as.vector(bldg_mask * evac_mat * buffer_mat)
  result[result == 0] <- NA

  return(result)
}


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


# intersect tower voronoi polygons and max radius buffers
intersect_voronoi_buffer <- function(voronois, buffers) {
  # ensure same CRS
  t_vor <- st_transform(voronois, st_crs(buffers))
  t_buf <- st_transform(buffers, st_crs(t_vor))

  # make a copy of the buffer geometry as a list-column, then drop the sf geometry
  buf_tbl <- t_buf %>%
    mutate(buf_geom = geometry) %>%
    st_set_geometry(NULL) %>%
    select(tower_id, buf_geom)

  # drop geometry from voronoi to keep attributes for later join
  vor_tbl <- t_vor %>% st_set_geometry(NULL)

  # join attributes (now have buffer geometry as column buf_geom)
  joined <- left_join(vor_tbl, buf_tbl, by = "tower_id")

  # compute pairwise intersections (row-wise) using purrr::map2
  intersections <- purrr::map2(
    .x = st_geometry(t_vor),
    .y = joined$buf_geom,
    ~ {
      if (is.null(.y) || length(.y) == 0) {
        st_geometrycollection() # no match -> empty geometry
      } else {
        st_intersection(.x, .y) # polygon ∩ buffer
      }
    }
  )

  # build final sf (keep vor attributes + intersection geometry)
  result <- joined %>%
    select(-buf_geom) %>% # drop the list-column
    st_as_sf(geometry = st_sfc(intersections, crs = st_crs(t_vor)))

  # clean / validate
  result <- st_make_valid(result)
  result <- result[!st_is_empty(result), ] # drop empty geometries if desired

  return(result)
}
