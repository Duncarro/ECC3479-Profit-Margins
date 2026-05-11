# Packages
library(dplyr)
library(purrr)
library(tidyr)
library(tseries)
library(urca)

# ---------------------------------------------------
# Function to run a suite of unit root tests
# ---------------------------------------------------

run_unit_root_tests <- function(data, data_name) {
  
  # Select variables
  df <- data |>
    select(
      date,
      margin = `Total less mining_resid`,
      bn
    ) |>
    drop_na()
  
  vars <- c("margin", "bn")
  
  map_dfr(vars, function(v) {
    
    x <- df[[v]]
    
    # -------------------------
    # ADF Tests
    # -------------------------
    
    adf_drift <- ur.df(
      x,
      type = "drift",
      selectlags = "AIC"
    )
    
    adf_trend <- ur.df(
      x,
      type = "trend",
      selectlags = "AIC"
    )
    
    adf_none <- ur.df(
      x,
      type = "none",
      selectlags = "AIC"
    )
    
    # -------------------------
    # Phillips-Perron
    # -------------------------
    
    pp_test <- ur.pp(
      x,
      type = "Z-tau",
      model = "trend",
      lags = "short"
    )
    
    # -------------------------
    # KPSS
    # -------------------------
    
    kpss_level <- kpss.test(
      x,
      null = "Level"
    )
    
    kpss_trend <- kpss.test(
      x,
      null = "Trend"
    )
    
    tibble(
      dataset = data_name,
      variable = v,
      
      adf_drift_stat =
        adf_drift@teststat[1],
      
      adf_drift_5pct =
        adf_drift@cval[1, "5pct"],
      
      adf_trend_stat =
        adf_trend@teststat[1],
      
      adf_trend_5pct =
        adf_trend@cval[1, "5pct"],
      
      adf_none_stat =
        adf_trend@teststat[1],
      
      adf_none_5pct =
        adf_trend@cval[1, "5pct"],
      
      pp_stat =
        pp_test@teststat,
      
      pp_5pct =
        pp_test@cval["5pct"],
      
      kpss_level_stat =
        kpss_level$statistic,
      
      kpss_level_p =
        kpss_level$p.value,
      
      kpss_trend_stat =
        kpss_trend$statistic,
      
      kpss_trend_p =
        kpss_trend$p.value
    )
  })
}

# ---------------------------------------------------
# Run across all datasets
# ---------------------------------------------------

unit_root_results <- bind_rows(
  run_unit_root_tests(gop_gdp, "gop_gdp"),
  run_unit_root_tests(gop_thw, "gop_thw"),
  run_unit_root_tests(gos_gdp, "gos_gdp"),
  run_unit_root_tests(gos_thw, "gos_thw")
)

# View results
unit_root_results


# ---------------------------------------------------
# First differences
# ---------------------------------------------------

diff_df <- gos_gdp |>
  select(
    date,
    margin = `Total less mining_resid`,
    bn
  ) |>
  mutate(
    d_margin = difference(margin),
    d_bn = difference(bn)
  ) |>
  drop_na()

# ---------------------------------------------------
# Function to run unit root tests
# ---------------------------------------------------

run_diff_tests <- function(x, var_name) {
  
  # ADF
  adf_drift <- ur.df(
    x,
    type = "drift",
    selectlags = "AIC"
  )
  
  adf_trend <- ur.df(
    x,
    type = "trend",
    selectlags = "AIC"
  )
  
  # Phillips-Perron
  pp_test <- ur.pp(
    x,
    type = "Z-tau",
    model = "constant",
    lags = "short"
  )
  
  # KPSS
  kpss_level <- kpss.test(
    x,
    null = "Level"
  )
  
  tibble(
    variable = var_name,
    
    adf_drift_stat =
      adf_drift@teststat[1],
    
    adf_drift_5pct =
      adf_drift@cval[1, "5pct"],
    
    adf_trend_stat =
      adf_trend@teststat[1],
    
    adf_trend_5pct =
      adf_trend@cval[1, "5pct"],
    
    pp_stat =
      pp_test@teststat,
    
    pp_5pct =
      pp_test@cval["5pct"],
    
    kpss_level_stat =
      kpss_level$statistic,
    
    kpss_level_p =
      kpss_level$p.value
  )
}

# ---------------------------------------------------
# Run tests
# ---------------------------------------------------

diff_unit_root_results <- bind_rows(
  run_diff_tests(diff_df$d_margin, "d_margin"),
  run_diff_tests(diff_df$d_bn, "d_bn")
)

diff_unit_root_results