# ===============================
# LOAD PACKAGES
# ===============================

packages <- c("tidyverse", "fpp3", "readabs", "scales", "mFilter", "pracma", "Rlibeemd", "broom", "modelsummary")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

cat("✅ Packages loaded successfully\n")


# ===============================
# CHECK WORKING DIRECTORY
# ===============================

cat("📁 Working directory:", getwd(), "\n")

required_files <- c(
  "data/clean/gop.csv",
  "data/clean/gos.csv",
  "data/clean/gop_wide.csv",
  "data/clean/gos_wide.csv",
  "data/clean/thw.csv",
  "data/clean/gdp.csv",
  "data/clean/thw_aug.csv",
  "data/clean/gdp_aug.csv"
  
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste("❌ Missing files:", paste(missing_files, collapse = ", ")))
}

cat("✅ All data files found\n")


# ===============================
# LOAD CLEANED DATASETS
# ===============================

gop <- read_csv("data/clean/gop.csv") |>
  mutate(date = as.Date(date))

gos <- read_csv("data/clean/gos.csv") |>
  mutate(date = as.Date(date))

gop_wide_margin <- read_csv("data/clean/gop_wide.csv") |>
  mutate(date = as.Date(date))

gos_wide_margin <- read_csv("data/clean/gos_wide.csv") |>
  mutate(date = as.Date(date))

thw <- read_csv("data/clean/thw.csv") |>
  mutate(date = as.Date(date))

gdp <- read_csv("data/clean/gdp.csv") |>
  mutate(date = as.Date(date))

thw_ext <- read_csv("data/clean/thw_ext.csv") |>
  mutate(date = as.Date(date))

gdp_ext <- read_csv("data/clean/gdp_ext.csv") |>
  mutate(date = as.Date(date))

bn_thw <- read_csv("data/clean/thw_aug.csv") |>
  mutate(date = as.Date(date))

bn_gdp <- read_csv("data/clean/gdp_aug.csv") |>
  mutate(date = as.Date(date))

cat("✅ Data loaded successfully\n")


# ===============================
# OPTIONAL: QUICK SANITY CHECKS
# ===============================

cat("Rows loaded:\n")
cat("gop:", nrow(gop), "\n")
cat("gos:", nrow(gos), "\n")
cat("gdp:", nrow(gdp), "\n")
