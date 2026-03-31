#Loads all necessary packages

# List of required packages
packages <- c("tidyverse", "fpp3", "readabs", "scales")

# Install any that are missing
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)