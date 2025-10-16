# tower coverage layers
tower_coverage <- function(dist, rad, mastergrid, kappa = 10) {
  # dist = distance1
  # rad = radius1
  # mastergrid = mastergrid
  # kappa = 2

  n_towers <- length(rad)
  result <- array(NA, dim = c(n_towers, dim(mastergrid)[1:2]))

  for (tower in 1:n_towers) {
    d <- dist[tower, ] / 1e3
    r <- rad[tower] / 1e3
    p <- (1 + exp(-kappa * r)) / (1 + exp(kappa * (d - r)))
    p[p < 0.01] <- 0
    p <- p / sum(p, na.rm = T)

    ras <- mastergrid
    values(ras) <- NA
    ras[mastergrid == 1] <- p

    result[tower, , ] <- as.matrix(ras)
  }
  dimnames(result) <- list(tower = rownames(dist), cellx = 1:nrow(mastergrid), celly = 1:ncol(mastergrid))
  return(result)
}

p_spot_check <- function(tower_id, p) {
  ras <- mastergrid
  values(ras) <- as.vector(p[tower_id, , ])
  plot(ras)
}

plot_rast <- function(x, ras=mastergrid, zero2NA=T){
  values(ras) <- as.vector(x)
  ras[ras==0] <- NA
  plot(ras)
}

distance_array <- function(distance, mastergrid) {
  n_tower <- nrow(distance)
  x <- array(NA, dim = c(n_tower, dim(mastergrid)[1:2]))
  for (tower in 1:n_tower) {
    d <- mastergrid
    d[!is.na(d)] <- distance[tower, ] / 1e3
    x[tower, , ] <- as.matrix(d)
  }
  ras <- mastergrid
  values(ras) <- as.vector(x[sample(1:n_tower, 1), , ])
  plot(ras)

  return(x)
}


subscriber_layers <- function(date, subscribers, p_tower, p_mask = NA) {
  # date = as.Date("2025-03-13")
  # subscribers = telco1
  # p_tower = p1

  s <- subscribers |>
    rename(telco_date = date) |>
    filter(telco_date == date) |>
    arrange(tower_id) |>
    select(tower_id, subscribers)
  p <- p_tower[s$tower_id, , ]

  if (is.matrix(p_mask)) {
    # p <- sweep(p, MARGIN = c(2, 3), STATS = p_mask, FUN = "*")

    p_scale <- array(NA, dim = dim(p))
    for (i in 1:dim(p)[1]) {
      slice <- p[i, , ] * p_mask
      p_scale[i, , ] <- slice / sum(slice, na.rm = T)
    }
  } else {
    p_scale <- p
  }

  x <- s$subscribers * p_scale
  dimnames(x) <- list(tower = s$tower_id, celly = 1:dim(p)[2], cellx = 1:dim(p)[3])
  return(x)
}

# subscriber_raster <- function(telco, p_tower, mastergrid, outpath) {
#   # telco = telco1[telco1$date == max(telco1$date),]
#   # p_tower = p1
#   # outpath = file.path(out_dir, "subscriber_rasters", "provider1.tif")

#   dir.create(dirname(outpath), showWarnings = F, recursive = T)

#   for (date in unique(telco$date)) {
#     # date <- max(telco$date)
#     s <- subscriber_layers(
#       date = date,
#       subscribers = telco,
#       p_tower = p_tower
#     )

#     s_adj <- s
#     s_adj[s == 0] <- NA
#     x <- apply(s_adj, MARGIN = c(2, 3), FUN = quantile, probs=c(q), na.rm = T)
#     # x <- apply(s_adj, MARGIN = c(2, 3), FUN = mean, na.rm = T)

#     ras <- mastergrid
#     values(ras) <- as.vector(x)
#     plot(ras)

#     # save to disk
#     writeRaster(
#       ras,
#       paste0(tools::file_path_sans_ext(outpath), "_", date, ".tif"),
#       overwrite = T
#     )
#   }
# }


subscriber_raster <- function(telco, p_tower, mastergrid, outpath, p_mask = 1) {
  # telco = telco1[telco1$date == max(telco1$date),]
  # p_tower = p1
  # outpath = file.path(out_dir, "subscriber_rasters", "provider1.tif")

  dir.create(dirname(outpath), showWarnings = F, recursive = T)

  for (date in unique(telco$date)) {
    # date <- max(telco$date)
    s <- subscriber_layers(
      date = date,
      subscribers = telco,
      p_tower = p_tower,
      p_mask = p_mask[[date]]
    )

    # weighted mean across towers
    x0 <- s * p_tower[dimnames(s)[[1]], , ]
    x <- apply(x0, MARGIN = c(2, 3), sum, na.rm = T) / apply(p_tower[dimnames(s)[[1]], , ], MARGIN = c(2, 3), sum, na.rm = T)

    # rasterise
    ras <- mastergrid
    values(ras) <- as.vector(x)
    ras[is.na(ras) & !is.na(mastergrid)] <- 0
    plot(ras)

    # save to disk
    writeRaster(
      ras,
      paste0(tools::file_path_sans_ext(outpath), "_", date, ".tif"),
      overwrite = T
    )
  }
}


# create mask
build_mask <- function(dates, evac, evac_buffers, bldg_mask, mastergrid){
  result <- list()
  for (d in dates) {
    blocks <- evac |>
      filter(date <= d & date_end >= d) |>
      select(COD_mod) |>
      pull()
    
    if (length(blocks) == 0) {
      evac_mat <- matrix(1, nrow = nrow(mastergrid), ncol = ncol(mastergrid))
    } else {
      evac_ras <- mastergrid
      evac_ras[mastergrid == 1 & evac_grid %in% blocks] <- 0
      evac_mat <- matrix(evac_ras[], nrow = nrow(mastergrid), ncol = ncol(mastergrid))
    }

    buffer_dates <- names(evac_buffers)
    buffer_date <- max(buffer_dates[buffer_dates <= d])
    if (is.na(buffer_date)) {
      buffer_mat <- matrix(1, nrow = nrow(mastergrid), ncol = ncol(mastergrid))
    } else {
      buffer_ras <- evac_buffers[[buffer_date]]
      buffer_ras[buffer_ras == 1] <- 0
      buffer_ras[is.na(buffer_ras) & mastergrid == 1] <- 1
      buffer_mat <- matrix(buffer_ras[], nrow = nrow(mastergrid), ncol = ncol(mastergrid))
    }

    result[[d]] <- bldg_mask * evac_mat * buffer_mat
  }
  return(result)
}
