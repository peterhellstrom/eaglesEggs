# https://r-pkgs.org
# devtools::install_github("r-lib/devtools")
# devtools::install_github("r-lib/usethis")

# can be added to .Rprofile startup file
library(devtools)

# Create project ----
p <- "W:/projects/R/eaglesEggs"
# usethis::create_package(p, check_name = FALSE)

# License ----
usethis::use_mit_license()

# Creat GitHub repository ----
use_git_config(
  user.name = "peterhellstrom",
  user.email = "peter.hellstrom@nrm.se"
)

usethis::use_git()
usethis::use_github()

usethis::create_github_token()

# Load all ----
load_all()

# Documentation / NAMESPACE ----
document()

# Check ----
chk_pkg <- check()
dplyr::glimpse(chk_pkg)
names(chk_pkg)

chk_pkg$checkdir
utils::browseURL(chk_pkg$checkdir)

# Test ----
test()

# ReadMe ----
use_readme_rmd()
build_readme()

# Imports ----
# How to deal with this?
# Imports includes {n} non-default packages.
# Importing from so many packages makes the package vulnerable to any of
# them becoming unavailable.  Move as many as possible to Suggests and
# use conditionally.

# https://stackoverflow.com/questions/63345284/r-package-cran-note-for-package-dependencies-and-warnings-in-tests
# Bad practice to import entire package?
# Use some functions "conditionally"?

# 2) Ddd dependencies to non-CRAN packages?
# like my own; eagles
# 2) When should ImportFrom be used?
# 3) How to use requireNamespace
#    Use `requireNamespace("ggplot2", quietly = TRUE)` to test if package is installed
#    • Then directly refer to functions with `ggplot2::fun()`

# 'cellranger' 'dplyr' 'janitor' 'lubridate' 'purrr' 'readr' 'readxl'
# 'stringr' 'tibble' 'tidyr' 'tidyselect' 'tidyxl'

usethis::use_package("cellranger", min_version = TRUE)
usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("janitor", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)
usethis::use_package("readr", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("tibble", min_version = TRUE)
usethis::use_package("tidyr", min_version = TRUE)
usethis::use_package("tidyselect", min_version = TRUE)
usethis::use_package("tidyxl", min_version = TRUE)

# Suggests ----
usethis::use_package("ggplot2", "Suggests")
usethis::use_package("writexl", "Suggests")

requireNamespace("ggplot2", quietly = TRUE)

usethis::use_tidy_description()

# Install ----
install()

## Install from GitHub ----
# install_github("peterhellstrom/eaglesEggs")

# Ignore ----
usethis::use_build_ignore(
  c("backup", "data-raw", "development", "examples")
)

# Document data ----
# https://r-pkgs.org/data.html

## Load package ----
library(eaglesEggs)
utils::sessionInfo()
sessioninfo::session_info()

## Data sets ----
usethis::use_data_raw()
