# R package requirements
pkgs <- c(
  'dplyr',
  'terra',
  'sf',
  'here',
  'purrr',
  'cmdstanr',
  'posterior',
  'bayesplot',
  'loo',
  'here',
  'ggplot2'
)

# identify missing packages
missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

# install missing packages
if (length(missing_pkgs) > 0) {
  # Ensure R uses the system's cmake for the 's2' dependency
  Sys.setenv(CMAKE = "/usr/bin/cmake")

  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))

  if ("cmdstanr" %in% missing_pkgs) {
    install.packages(
      "cmdstanr",
      repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
    )
  }

  if (0 < length(missing_pkgs[missing_pkgs != "cmdstanr"])) {
    install.packages(missing_pkgs, dependencies = TRUE)
  }
} else {
  message("All packages are already installed!")
}

# cmdstan
library(cmdstanr)

check_cmdstan_toolchain()
cmdstanr::check_cmdstan_toolchain(fix = TRUE)

install_cmdstan(cores = 2)
