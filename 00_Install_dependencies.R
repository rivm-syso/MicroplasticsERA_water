# The code in this repository was used with R version 4.5.2.

# The versions of the packages used are listed below. 

# Used packages
needed_packages <- c(
  "tidyverse",      # used with version 2.0.0
  "readxl",         # used with version 1.4.5
  "fuzzyjoin",      # used with version 0.1.6.1
  "readr",          # used with version 2.1.5
  "poweRlaw",       # used with version 1.0.0
  "viridis",        # used with version 0.6.5
  "ggplot2",        # used with version 3.5.2
  "rnaturalearth",  # used with version 1.2.0
  "sf",             # used with version 1.0-20
  "openxlsx",       # used with version 4.2.7.1
  "dplyr",          # used with version 1.1.4
  "tidyr",          # used with version 1.3.1
  "stringr",        # used with version 1.5.1
  "knitr",          # used with version 1.50
  "RColorBrewer"    # used with version 1.1.3
)

# Data frame with package versions, if you want to install them specifically:
# needed_versions <- c(
#   tidyverse          = "2.0.0",
#   readxl             = "1.4.5",
#   fuzzyjoin          = "0.1.6.1",
#   readr              = "2.1.5",
#   poweRlaw           = "1.0.0",
#   viridis            = "0.6.5",
#   ggplot2            = "3.5.2",
#   rnaturalearth      = "1.2.0",
#   sf                 = "1.0-20",
#   openxlsx           = "4.2.7.1",
#   dplyr              = "1.1.4",
#   tidyr              = "1.3.1",
#   stringr            = "1.5.1",
#   knitr              = "1.50",
#   RColorBrewer       = "1.1.3"
# )


# Install pak if not already installed
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Install packages (latest versions available)
for (pkg in needed_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    pak::pkg_install(pkg)
  }
}

# Install packages (specified versions)
# for (pkg in needed_packages) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     ver <- needed_versions[pkg]
#     if (!(pkg %in% rownames(installed.packages())) ||
#         packageVersion(pkg) != ver) {
#       pak::pkg_install(paste0(pkg, "@", ver))
#     }
#   }
# }

# Install rnaturalearthhires (no specific version; created with some GitHub version)
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("ropensci/rnaturalearthhires")
}

cat("All needed packages are installed.")