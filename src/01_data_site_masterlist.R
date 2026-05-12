# cleanup
rm(list = ls())
gc()

# load libraries
library(dplyr)
library(sf)
library(jsonlite)

# load functions
src_dir <- here::here("src")
source(file.path(src_dir, "01_data_site_masterlist_fun.R"))

# encoding for Arabic
set_utf8_locale()

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
if (dir.exists(env$wd)) {
  setwd(env$wd)
} else {
  stop("Working directory does not exist.")
}

out_dir <- file.path(getwd(), "out", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

site_column_map <- c(
  site_id = "Site.ID",
  site_name = "Site.Name",
  site_status = "Site.Status",
  site_type = "Displacement.Type",
  ADM2_EN = "Governorate",
  ADM2_PCODE = "Region.Information.First.Level.Region.ID",
  ADM4_EN = "Neighborhood",
  ADM4_PCODE = "Region.Information.Second.Level.Region.ID",
  managing_agency = "Managing.Agency",
  implementing_partner = "Implementing.Partner",
  assessing_agency = "Assessing.Agency",
  interview_date = "Interview.Date",
  interview_type = "Interview.Type",
  site_type_assessed = "Displacement.Type..Ass.",
  site_status_assessed = "Site.Status..Ass.",
  site_mobile_connectivity = "Site.Management.Is.there.stable.network.coverage.at.the.site..mobile.or.internet..",
  T_TOT = "Site.demographics.Estimated.number.of.individuals.currently.accommodated.in.the.site",
  M_00_05 = "Site.demographics.Number.of.MALES.of.age.between.0...5.years",
  M_06_17 = "Site.demographics.Number.of.MALES.of.age.between.6...17.years",
  M_18_60 = "Site.demographics.Number.of.MALES.of.age.between.18...60.years",
  M_61_plus = "Site.demographics.Number.of.MALES.of.age.above.60.years",
  F_00_05 = "Site.demographics.Number.of.FEMALES.of.age.between.0...5.years",
  F_06_17 = "Site.demographics.Number.of.FEMALES.of.age.between.6...17.years",
  F_18_60 = "Site.demographics.Number.of.FEMALES.of.age.between.18...60.years",
  F_61_plus = "Site.demographics.Number.of.FEMALES.of.age.above.60.years",
  households = "Site.demographics.Please.review.the.below.information.Estimated.number.of.households.currently.accommodated.in.the.site..total.population.divided.by.5.is...est_hh_existing..",
  recent_movement_households = "Site.demographics.Recent.movements.at.the.site.What.have.been.the.main.population.changes.at.the.site.in.the.past.month.",
  households_out = "Site.demographics.Recent.movements.at.the.site.How.many.households.do.you.estimate.have.left.the.site.in.the.last.30.days.",
  individuals_out = "Site.demographics.Recent.movements.at.the.site.How.many.individuals.do.you.estimate.have.left.the.site.in.the.last.30.days.",
  households_in = "Site.demographics.Recent.movements.at.the.site.How.many.households.do.you.estimate.have.arrived.at.the.site.in.the.last.30.days.",
  individuals_in = "Site.demographics.Recent.movements.at.the.site.How.many.individuals.do.you.estimate.have.arrived.at.the.site.in.the.last.30.days.",
  households_female_headed = "Vulnerable.Demographics.Approx..How.many.familes.are.female.headed..18.59.years..",
  households_female_headed_proportion = "Vulnerable.Demographics.fhh_proportion",
  households_child_headed = "Vulnerable.Demographics.Approx..How.many.families.are.child.headed..0.17..",
  households_child_headed_proportion = "Vulnerable.Demographics.chh_proportion",
  unattended_children = "Vulnerable.Demographics.Approx..How.many.persons.under.the.age.of.18.are.living.with.no.mother..no.father.or.any.other.adult.relative.in.the.site.",
  unattended_children_proportion = "Vulnerable.Demographics.sepc_proportion",
  individuals_disabled = "Vulnerable.Demographics.Approx..How.many.individuals.living.in.the.site.have.a.physical.disability.",
  individuals_chronic_illness = "Vulnerable.Demographics.Approx..How.many.individuals.living.in.the.site.suffer.from.a.chronic.illness.",
  individuals_pregnant_lactating = "Vulnerable.Demographics.Approx..Number.of.women.living.in.the.site.are.pregnant.or.lactating"
)

site_output_columns <- names(site_column_map)
site_output_columns <- append(
  site_output_columns,
  c("latitude", "longitude"),
  after = match("site_type", site_output_columns)
)
site_output_columns <- append(
  site_output_columns,
  c("M_TOT", "F_TOT"),
  after = match("F_61_plus", site_output_columns)
)

male_age_columns <- c("M_00_05", "M_06_17", "M_18_60", "M_61_plus")
female_age_columns <- c("F_00_05", "F_06_17", "F_18_60", "F_61_plus")

#---- download raw data ----#
zitemanager_base_url <- "https://app.zitemanager.org/api/v2/reports-file/"

required_env_vars <- c(
  "zitemanager_site_masterlist_report_id",
  "zitemanager_site_masterlist_key",
  "zitemanager_site_polygons_report_id",
  "zitemanager_site_polygons_key"
)

missing_env_vars <- required_env_vars[
  !nzchar(unlist(mget(
    required_env_vars,
    envir = env,
    ifnotfound = list("")
  )))
]

if (length(missing_env_vars) > 0) {
  stop(
    "Missing required .env values: ",
    paste(missing_env_vars, collapse = ", ")
  )
}

site_masterlist <- download_report_json_csv(
  url = paste0(
    zitemanager_base_url,
    "?report_id=",
    env$zitemanager_site_masterlist_report_id,
    "&key=",
    env$zitemanager_site_masterlist_key
  ),
  destfile = file.path(out_dir, "site_masterlist.csv")
)

site_polygons <- download_report_json_csv(
  url = paste0(
    zitemanager_base_url,
    "?report_id=",
    env$zitemanager_site_polygons_report_id,
    "&key=",
    env$zitemanager_site_polygons_key
  ),
  destfile = file.path(out_dir, "site_polygons.csv")
)

#---- clean site masterlist ----#
site_masterlist_clean <- site_masterlist %>%
  filter(Site.Status == "Active")

site_location_coords <- st_as_sfc(
  site_masterlist_clean$Site.Information.Location,
  crs = 4326
) %>%
  st_coordinates()

site_masterlist_clean <- site_masterlist_clean %>%
  rename(all_of(site_column_map)) %>%
  mutate(
    across(
      all_of(c(male_age_columns, female_age_columns)),
      ~ as.numeric(.)
    )
  ) %>%
  mutate(
    site_type = standardize_site_type(site_type),
    latitude = site_location_coords[, "Y"],
    longitude = site_location_coords[, "X"],
    interview_date = as.Date(interview_date, format = "%Y-%m-%d"),
    interview_type = standardize_interview_type(interview_type),
    site_type_assessed = standardize_site_type(site_type_assessed),
    site_status_assessed = standardize_site_status_assessed(
      site_status_assessed
    ),
    site_mobile_connectivity = standardize_mobile_connectivity(
      site_mobile_connectivity
    ),
    M_TOT = rowSums(across(all_of(male_age_columns)), na.rm = TRUE),
    M_TOT = ifelse(if_all(all_of(male_age_columns), is.na), NA, M_TOT),
    F_TOT = rowSums(across(all_of(female_age_columns)), na.rm = TRUE),
    F_TOT = ifelse(if_all(all_of(female_age_columns), is.na), NA, F_TOT),
    recent_movement_households = standardize_recent_movement_households(
      recent_movement_households
    )
  ) %>%
  select(any_of(site_output_columns))

write.csv(
  site_masterlist_clean,
  file.path(out_dir, "site_masterlist_clean.csv"),
  row.names = FALSE
)

site_masterlist_geo <- site_masterlist_clean %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

st_write(
  site_masterlist_geo,
  file.path(out_dir, "site_masterlist_clean.gpkg"),
  append = FALSE
)

#---- site polygons ----#
site_polygon_geometries <- mapply(
  FUN = parse_site_polygon_geometry,
  wkt = site_polygons$Site.Extent.WKT..Most.Recent.Value.,
  raw_points = site_polygons$Please.walk.along.the.limit.of.the.site.and.record.the.coordinates.of.each.corner.point..Please.be.as.precise.as.possible.and.count.only.the.area.of.the.site.that.you.are.responsible.for..Most.Recent.Value.,
  SIMPLIFY = FALSE
)

site_polygons_geo <- st_sf(
  site_polygons,
  geometry = st_sfc(site_polygon_geometries, crs = 4326)
) |>
  filter(!st_is_empty(geometry)) |>
  st_make_valid()

st_write(
  site_polygons_geo,
  file.path(out_dir, "site_polygons.gpkg"),
  append = FALSE
)

#---- join polygons to masterlist ----#
site_masterlist_with_polygons <- site_masterlist_clean %>%
  left_join(
    site_polygons_geo %>%
      filter(Site.Information.Site.Status == "Active") %>%
      select(Site.ID, geometry),
    by = c("site_id" = "Site.ID")
  ) %>%
  st_as_sf()

st_write(
  site_masterlist_with_polygons,
  file.path(out_dir, "site_masterlist_with_polygons.gpkg"),
  append = FALSE
)
