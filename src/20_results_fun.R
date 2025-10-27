summarise_grid_per_governorate <- function(
  pop_ras,
  gov_poly,
  gov_ras,
  ref_date
) {
  result <- gov_poly |>
    rename(
      ADM2_EN = Name,
      ADM2_PCODE = PCODE
    ) |>
    select(id, ADM2_EN, ADM2_PCODE) |>
    mutate(
      population = NA,
      date = ref_date
    )

  for (i in 1:nrow(result)) {
    result$population[i] <- sum(
      pop_ras[gov_ras == result$id[i]],
      na.rm = T
    )
  }

  result <- result |>
    select(ADM2_PCODE, ADM2_EN, date, population) |>
    arrange(ADM2_PCODE)

  return(result)
}

summarise_grid_per_municipality <- function(
  pop_ras,
  mun_poly,
  mun_ras,
  ref_date
) {
  pop_mun <- mun_poly |>
    rename(
      ADM2_EN = Governorat,
      ADM3_EN = Name,
      ADM3_PCODE = PCOE_Munic,
    ) |>
    mutate(ADM2_PCODE = substr(ADM3_PCODE, 1, 3)) |>
    select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE) |>
    mutate(
      population = NA,
      date = ref_date
    )

  for (i in 1:nrow(pop_mun)) {
    pop_mun$population[i] <- sum(
      pop_ras[mun_ras == pop_mun$id[i]],
      na.rm = T
    )
  }

  pop_mun <- pop_mun |>
    select(ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE, date, population) |>
    arrange(ADM3_PCODE)

  return(pop_mun)
}

summarise_grid_per_neighbourhood <- function(
  pop_ras,
  nbr_poly,
  nbr_ras,
  ref_date
) {
  pop_nbr <- nbr_poly |>
    rename(
      ADM2_EN = Governorat,
      ADM2_PCODE = PCODE_Gove,
      ADM3_EN = Name_Munic,
      ADM3_PCODE = PCOE_Munic,
      ADM4_EN = Neighbourh,
      ADM4_PCODE = PCODE_Neig
    ) |>
    select(id, ADM2_EN, ADM2_PCODE, ADM3_EN, ADM3_PCODE, ADM4_EN, ADM4_PCODE) |>
    mutate(
      population = NA,
      date = ref_date
    )

  for (i in 1:nrow(pop_nbr)) {
    pop_nbr$population[i] <- sum(
      pop_ras[nbr_ras == pop_nbr$id[i]],
      na.rm = T
    )
  }

  pop_nbr <- pop_nbr |>
    select(
      ADM2_EN,
      ADM2_PCODE,
      ADM3_EN,
      ADM3_PCODE,
      ADM4_EN,
      ADM4_PCODE,
      date,
      population
    ) |>
    arrange(ADM4_PCODE)
}
