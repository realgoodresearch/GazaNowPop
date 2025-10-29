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


# merge voronoi if masked out
merge_masked_voronoi <- function(towers, voronoi, mask) {
  # towers_geo           : sf POINT with column tower_id
  # towers_buff_voronoi  : sf POLYGON with column tower_id
  # mask                 : terra SpatRaster

  #---- sum raster cells per polygon and find polygons with sum == 0 ----#

  # convert polygons to terra vect for extract
  pol_v <- terra::vect(voronoi)
  ex <- terra::extract(mask, pol_v, fun = sum, na.rm = TRUE)
  pol_sums <- ex[, 2]

  zero_idx <- which(is.na(pol_sums) | pol_sums == 0)
  if (length(zero_idx) == 0) {
    message("No polygons have raster-cell sum == 0. Returning original object.")
    voronoi_new <- voronoi
  } else {
    # copy working sf
    work_polys <- voronoi %>% st_make_valid() %>% mutate(tower_id_merged = NA)
    work_points <- towers %>% st_make_valid()

    # process each zero-sum polygon, updating work_polys as we go to avoid double-merge conflicts
    for (i in seq_along(zero_idx)) {
      # index in original ordering -> get tower_id
      orig_index <- zero_idx[i]
      # find the tower_id value for that polygon (if polygons were reordered, safe to look up by matching)
      pid_zero <- voronoi$tower_id[orig_index]

      # check whether this pid_zero still exists in work_polys (it may have been removed by earlier merges)
      if (!pid_zero %in% work_polys$tower_id) {
        next
      }

      # point for the zero-sum tower
      pt_zero <- work_points %>% filter(tower_id == pid_zero)
      if (nrow(pt_zero) == 0) {
        warning(paste("No point found for tower_id", pid_zero, "- skipping"))
        next
      }

      # compute distances to all candidate points in work_points; exclude self
      dists <- as.numeric(st_distance(pt_zero, work_points)) # vector of distances
      self_idx <- which(work_points$tower_id == pid_zero)
      dists[self_idx] <- Inf
      nearest_idx <- which.min(dists)
      pid_nearest <- work_points$tower_id[nearest_idx]

      # if nearest is NA or Inf, skip
      if (!is.finite(dists[nearest_idx])) {
        warning(paste("No other towers available to merge with for", pid_zero))
        next
      }

      # get polygons for the two ids from the current working polygons
      poly_a <- work_polys %>% filter(tower_id == pid_zero)
      poly_b <- work_polys %>% filter(tower_id == pid_nearest)

      # If for some reason nearest polygon not found, skip
      if (nrow(poly_b) == 0) {
        warning(paste(
          "Nearest polygon not found for",
          pid_nearest,
          "- skipping"
        ))
        next
      }

      # 3) union the two polygons and build a merged row
      merged_geom <- st_union(poly_a$geometry, poly_b$geometry) %>%
        st_make_valid()

      # create new attributes: keep a merged id (adjust as you prefer)
      merged_row <- poly_b[1, ] # take attributes from nearest as base
      merged_row$geometry <- merged_geom
      merged_row$tower_id_merged <- ifelse(
        !is.finite(merged_row$tower_id_merged),
        pid_zero,
        paste(merged_row$tower_id_merged, pid_zero, sep = '+')
      )
      merged_row$subscribers <- poly_a$subscribers + poly_b$subscribers

      # remove original two polygons from work_polys and add merged_row
      work_polys <- work_polys %>%
        filter(!tower_id %in% c(pid_zero, pid_nearest)) %>%
        bind_rows(merged_row) %>%
        st_make_valid()
      # also remove the merged point (optional) to avoid merging it again; we keep points unchanged here
      # work_points <- work_points %>% filter(! tower_id %in% c(pid_zero, pid_nearest))
    }
    voronoi_new <- work_polys
  }
  return(voronoi_new)
}
