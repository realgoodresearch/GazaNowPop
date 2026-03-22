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
