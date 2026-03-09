# R package requirements
pkgs <- c('dplyr', 'terra', 'sf', 'here', 'purrr')

# identify missing packages
missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

# install missing packages
if (length(missing_pkgs) > 0) {
  # Ensure R uses the system's cmake for the 's2' dependency
  Sys.setenv(CMAKE = "/usr/bin/cmake")

  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, dependencies = TRUE)
} else {
  message("All packages are already installed!")
}
