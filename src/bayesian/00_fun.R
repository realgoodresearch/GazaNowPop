## Global helpers shared across Bayesian scripts.

timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

log_message <- function(message, model_name = NULL) {
  suffix <- if (is.null(model_name) || is.na(model_name) || !nzchar(model_name)) {
    ""
  } else {
    paste0(" for ", model_name)
  }

  cat("[", timestamp(), "] ", message, suffix, "\n", sep = "")
}

write_tower_voronoi_predictions <- function(
  pred_summary,
  model_name,
  model_out_dir,
  env_wd,
  output_stem,
  tower_summary = NULL
) {
  mcmc_dir <- file.path(dirname(model_out_dir), "mcmc")

  for (provider_id in c(1L, 2L)) {
    gpkg_path <- file.path(mcmc_dir, paste0("towers", provider_id, "_voronoi.gpkg"))

    towers_sf <- sf::st_read(gpkg_path, quiet = TRUE)

    if (!is.null(tower_summary)) {
      towers_sf <- towers_sf %>%
        dplyr::left_join(
          tower_summary %>%
            dplyr::filter(provider == provider_id) %>%
            dplyr::select(-provider),
          by = c("tower_id", "tower_index")
        )
    }

    towers_sf <- towers_sf %>%
      dplyr::left_join(
        pred_summary %>%
          dplyr::filter(provider == provider_id) %>%
          dplyr::select(-provider),
        by = c("tower_id", "tower_index")
      )

    sf::st_write(
      towers_sf,
      file.path(model_out_dir, paste0("towers", provider_id, "_", output_stem, ".gpkg")),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }
}

admin_metadata_sf <- function(admin_sf, level) {
  if (level == "gov") {
    admin_sf %>%
      rename(
        ADM2_EN = Name,
        ADM2_PCODE = PCODE
      ) %>%
      select(id, ADM2_PCODE, ADM2_EN)
  } else if (level == "mun") {
    admin_sf %>%
      rename(
        ADM2_EN = Governorat,
        ADM3_EN = Name,
        ADM3_PCODE = PCOE_Munic
      ) %>%
      mutate(ADM2_PCODE = substr(ADM3_PCODE, 1, 3)) %>%
      select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE)
  } else {
    admin_sf %>%
      rename(
        ADM2_EN = Governorat,
        ADM2_PCODE = PCODE_Gove,
        ADM3_EN = Name_Munic,
        ADM3_PCODE = PCOE_Munic,
        ADM4_EN = Neighbourh,
        ADM4_PCODE = PCODE_Neig
      ) %>%
      select(
        id,
        ADM2_EN,
        ADM2_PCODE,
        ADM3_EN,
        ADM3_PCODE,
        ADM4_EN,
        ADM4_PCODE
      )
  }
}

admin_key_columns <- function(level) {
  if (level == "gov") {
    c("id", "ADM2_PCODE", "ADM2_EN")
  } else if (level == "mun") {
    c("id", "ADM2_EN", "ADM2_PCODE", "ADM3_EN", "ADM3_PCODE")
  } else {
    c(
      "id", "ADM2_EN", "ADM2_PCODE", "ADM3_EN", "ADM3_PCODE",
      "ADM4_EN", "ADM4_PCODE"
    )
  }
}

admin_csv_columns <- function(level) {
  if (level == "gov") {
    c("ADM2_PCODE", "ADM2_EN")
  } else if (level == "mun") {
    c("ADM2_EN", "ADM2_PCODE", "ADM3_EN", "ADM3_PCODE")
  } else {
    c("ADM2_EN", "ADM2_PCODE", "ADM3_EN", "ADM3_PCODE", "ADM4_EN", "ADM4_PCODE")
  }
}

build_grid_admin_lookup <- function(md, gov_grid, mun_grid, nbr_grid) {
  tibble::tibble(
    grid_index = seq_len(md$I),
    gov_id = as.integer(terra::values(gov_grid, mat = FALSE)[md$mastergrid_idx]),
    mun_id = as.integer(terra::values(mun_grid, mat = FALSE)[md$mastergrid_idx]),
    nbr_id = as.integer(terra::values(nbr_grid, mat = FALSE)[md$mastergrid_idx])
  )
}

summarise_admin_draws <- function(
  draw_matrix,
  grid_lookup,
  admin_col,
  admin_sf,
  level,
  reference_date,
  prefix = "population"
) {
  admin_ids <- sort(unique(grid_lookup[[admin_col]][!is.na(grid_lookup[[admin_col]])]))
  col_groups <- split(grid_lookup$grid_index, grid_lookup[[admin_col]])

  summary_tbl <- lapply(admin_ids, function(admin_id) {
    cols <- col_groups[[as.character(admin_id)]]
    totals <- rowSums(draw_matrix[, cols, drop = FALSE])
    lower_name <- if (prefix == "population") "pop_lower" else paste0(prefix, "_lower")
    upper_name <- if (prefix == "population") "pop_upper" else paste0(prefix, "_upper")

    tibble::tibble(
      id = admin_id,
      !!prefix := round(mean(totals)),
      !!lower_name := round(stats::quantile(totals, 0.025)),
      !!upper_name := round(stats::quantile(totals, 0.975))
    )
  }) %>%
    dplyr::bind_rows()

  csv_keys <- admin_csv_columns(level)

  admin_metadata_sf(admin_sf, level) %>%
    dplyr::left_join(summary_tbl, by = "id") %>%
    dplyr::mutate(date = reference_date) %>%
    dplyr::relocate(date, .after = dplyr::all_of(csv_keys[length(csv_keys)])) %>%
    dplyr::arrange(.data[[csv_keys[length(csv_keys)]]])
}

summarise_admin_ratio_draws <- function(
  numerator_draw_matrix,
  denominator_draw_matrix,
  grid_lookup,
  admin_col,
  admin_sf,
  level,
  reference_date,
  prefix
) {
  admin_ids <- sort(unique(grid_lookup[[admin_col]][!is.na(grid_lookup[[admin_col]])]))
  col_groups <- split(grid_lookup$grid_index, grid_lookup[[admin_col]])

  summary_tbl <- lapply(admin_ids, function(admin_id) {
    cols <- col_groups[[as.character(admin_id)]]
    numerator_totals <- rowSums(numerator_draw_matrix[, cols, drop = FALSE])
    denominator_totals <- rowSums(denominator_draw_matrix[, cols, drop = FALSE])
    ratio <- ifelse(denominator_totals > 0, numerator_totals / denominator_totals, NA_real_)

    tibble::tibble(
      id = admin_id,
      !!prefix := round(mean(ratio, na.rm = TRUE), 2),
      !!paste0(prefix, "_lower") := round(stats::quantile(ratio, 0.025, na.rm = TRUE), 2),
      !!paste0(prefix, "_upper") := round(stats::quantile(ratio, 0.975, na.rm = TRUE), 2)
    )
  }) %>%
    dplyr::bind_rows()

  csv_keys <- admin_csv_columns(level)

  admin_metadata_sf(admin_sf, level) %>%
    dplyr::left_join(summary_tbl, by = "id") %>%
    dplyr::mutate(date = reference_date) %>%
    dplyr::relocate(date, .after = dplyr::all_of(csv_keys[length(csv_keys)])) %>%
    dplyr::arrange(.data[[csv_keys[length(csv_keys)]]])
}

write_draw_mean_raster <- function(mastergrid, mastergrid_idx, values_mean, out_path) {
  out_rast <- mastergrid
  out_rast[out_rast == 1] <- 0
  out_rast[mastergrid_idx] <- values_mean
  terra::writeRaster(out_rast, out_path, overwrite = TRUE)
  out_rast
}
