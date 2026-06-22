set_utf8_locale <- function() {
  for (locale in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
    result <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (!is.na(result) && result != "") {
      return(invisible(result))
    }
  }

  warning("Could not set a UTF-8 locale; Arabic text may not render correctly.")
  invisible(NULL)
}

download_report_json_csv <- function(url, destfile = NULL) {
  tmpfile <- tempfile(fileext = ".json")
  on.exit(unlink(tmpfile), add = TRUE)

  utils::download.file(url = url, destfile = tmpfile, mode = "wb", quiet = TRUE)

  report_dat <- jsonlite::fromJSON(tmpfile, flatten = TRUE)

  names(report_dat) <- make.names(names(report_dat), unique = TRUE)

  if (!is.null(destfile)) {
    dir.create(dirname(destfile), showWarnings = FALSE, recursive = TRUE)
    write.csv(report_dat, destfile, row.names = FALSE)
  }

  report_dat
}

parse_site_polygon_geometry <- function(wkt, raw_points) {
  empty_geom <- st_as_sfc("POLYGON EMPTY", crs = 4326)[[1]]

  if (!is.na(wkt) && nzchar(wkt)) {
    wkt_geom <- tryCatch(
      suppressWarnings(
        suppressMessages(st_as_sfc(wkt, crs = 4326))
      )[[1]],
      error = function(e) NULL
    )

    if (!is.null(wkt_geom)) {
      return(wkt_geom)
    }
  }

  if (is.na(raw_points) || !nzchar(raw_points)) {
    return(empty_geom)
  }

  point_strings <- strsplit(raw_points, ";", fixed = TRUE)[[1]]
  coords_list <- lapply(point_strings, function(point_string) {
    point_string <- trimws(point_string)

    if (!nzchar(point_string)) {
      return(NULL)
    }

    point_values <- regmatches(
      point_string,
      gregexpr("-?\\d+(?:\\.\\d+)?", point_string, perl = TRUE)
    )[[1]]

    if (length(point_values) < 2) {
      return(NULL)
    }

    c(
      as.numeric(point_values[2]),
      as.numeric(point_values[1])
    )
  })

  coords_list <- coords_list[!vapply(coords_list, is.null, logical(1))]
  if (length(coords_list) < 4) {
    return(empty_geom)
  }

  coords <- do.call(rbind, coords_list)

  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords <- rbind(coords, coords[1, ])
  }

  st_polygon(list(coords))
}

standardize_open_value <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x <- tolower(x)
  x <- gsub("&", "and", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x[x == ""] <- NA_character_
  x
}

standardize_site_type <- function(x) {
  x <- trimws(x)
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x %in%
      c(
        "Collective Centre (UNRWA)",
        "UNRWA Collective centre"
      ) ~ "collective_centre_unrwa",
    x %in%
      c(
        "Collective Centre (non-UNRWA)",
        "Non-UNRWA Collective Center"
      ) ~ "collective_centre_non_unrwa",
    x == "Makeshift site" ~ "makeshift_site",
    x == "Scattered site" ~ "scattered_site",
    x == "Scattered site (less than 10 households)" ~ "scattered_site",
    x == "Planned Sites" ~ "planned_site",
    x == "Unknown" ~ "unknown",
    TRUE ~ standardize_open_value(x)
  )
}

standardize_interview_type <- function(x) {
  x <- trimws(x)
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "In person visit" ~ "in_person_visit",
    x == "Phone interview" ~ "phone_interview",
    TRUE ~ standardize_open_value(x)
  )
}

standardize_site_status_assessed <- function(x) {
  x <- trimws(x)
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "Active (people are living there)" ~ "active",
    x == "Inactive (site no longer has people)" ~ "inactive",
    x == "Site could not be found" ~ "site_not_found",
    TRUE ~ standardize_open_value(x)
  )
}

standardize_mobile_connectivity <- function(x) {
  x <- trimws(x)
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x ==
      "Yes, stable and usable with internet connectivity" ~ "stable_internet",
    x == "Yes, stable but only for calling and SMS" ~ "stable_calls_sms",
    x ==
      "Yes but intermittent / unstable even for calling and SMS" ~ "unstable_calls_sms",
    x == "Very limited / rarely available" ~ "very_limited",
    x == "No network coverage" ~ "no_network",
    TRUE ~ standardize_open_value(x)
  )
}

standardize_recent_movement_households <- function(x) {
  x <- trimws(x)
  has_arrivals <- grepl("arrived", x, ignore.case = TRUE)
  has_departures <- grepl("left", x, ignore.case = TRUE)

  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "No changes" ~ "no_change",
    has_arrivals & has_departures ~ "arrivals_and_departures",
    has_arrivals ~ "arrivals",
    has_departures ~ "departures",
    TRUE ~ standardize_open_value(x)
  )
}
