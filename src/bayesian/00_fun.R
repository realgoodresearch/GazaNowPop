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
  mcmc_dir <- file.path(env_wd, "out", "bayesian", model_name, "mcmc")

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
