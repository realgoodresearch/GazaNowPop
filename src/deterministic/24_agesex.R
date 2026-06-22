# !! WARNING!!
# This script overwrites existing files:
# out/deterministic/results/[reference_date]/supplementary_data/pop_gov_[reference_date].csv
# out/deterministic/results/[reference_date]/supplementary_data/pop_gov_[reference_date].gpkg

# cleanup
rm(list = ls())
gc()

#---- USER OPTIONS ----#
reference_date <- "2026-05-04"
#----------------------#

# load libraries
library(dplyr)
library(sf)

# encoding for Arabic
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
set_utf8_locale()

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
dir.create(env$wd, showWarnings = F, recursive = T)
setwd(env$wd)

in_dir <- file.path(getwd(), "in")
src_dir <- file.path(here::here(), "src", "deterministic")
data_dir <- file.path(getwd(), "out", "data")
model_dir <- file.path(getwd(), "out", "deterministic", "model", reference_date)
results_dir <- file.path(
  getwd(),
  "out",
  "deterministic",
  "results",
  reference_date
)
out_dir <- file.path(
  results_dir,
  "supplementary_data"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


#---- load data ----#
gov_agesex <- read.csv(file.path(model_dir, "site_masterlist_gov_agesex.csv"))

pop_gov_geo <- st_read(file.path(
  results_dir,
  "supplementary_data",
  paste0("pop_gov_", reference_date, ".gpkg")
))

cols_pop_gov_geo <- names(pop_gov_geo)

pop_gov_geo_agesex <- pop_gov_geo %>%
  mutate(ADM2_PCODE = as.integer(ADM2_PCODE)) %>%
  left_join(gov_agesex %>% select(-ADM2_EN), by = "ADM2_PCODE") %>%
  mutate(
    girls = pop_raw * F_00_17p,
    boys = pop_raw * M_00_17p,
    children = pop_raw * T_00_17p,
    women = pop_raw * F_18_plusp,
    men = pop_raw * M_18_plusp,
    adults = pop_raw * T_18_plusp
  ) %>%
  mutate(
    girls = ifelse(
      girls < 1000,
      round(girls / 100) * 100,
      round(girls / 1000) * 1000
    ),
    boys = ifelse(
      boys < 1000,
      round(boys / 100) * 100,
      round(boys / 1000) * 1000
    ),
    children = ifelse(
      children < 1000,
      round(children / 100) * 100,
      round(children / 1000) * 1000
    ),
    women = ifelse(
      women < 1000,
      round(women / 100) * 100,
      round(women / 1000) * 1000
    ),
    men = ifelse(men < 1000, round(men / 100) * 100, round(men / 1000) * 1000),
    adults = ifelse(
      adults < 1000,
      round(adults / 100) * 100,
      round(adults / 1000) * 1000
    )
  ) %>%
  select(
    any_of(cols_pop_gov_geo),
    girls,
    boys,
    children,
    women,
    men,
    adults
  ) %>%
  relocate(geom, .after = last_col())

pop_gov_geo_agesex

write.csv(
  pop_gov_geo_agesex %>% st_drop_geometry(),
  file.path(out_dir, paste0("pop_gov_", reference_date, ".csv")),
  row.names = FALSE
)

st_write(
  pop_gov_geo_agesex,
  file.path(out_dir, paste0("pop_gov_", reference_date, ".gpkg")),
  append = FALSE
)
