library(ARDL)


# ===================================================
# LIBRARIES
# ===================================================

library(dplyr)
library(tsibble)
library(purrr)
library(ARDL)
library(broom)
library(ggplot2)
library(patchwork)
library(lmtest)
library(tseries)
library(FinTS)
library(strucchange)

# ===================================================
# CREATE TSIBBLES
# ===================================================
make_tsibble <- function(data) {
  
  data |>
    
    rename(
      margin = `Total less mining_resid`
    ) |>
    
    mutate(
      date = yearquarter(date)
    ) |>
    
    as_tsibble(index = date) |>
    
    arrange(date) |>
    
    mutate(
      
      covid = if_else(
        date >= yearquarter("2020 Q1") &
          date <= yearquarter("2022 Q4"),
        1,
        0
      )
    ) |>
    
    drop_na()
}


# ===================================================
# PREPARE DATA
# ===================================================

gop_gdp_ts <- make_tsibble(gop_gdp)

gop_thw_ts <- make_tsibble(gop_thw)

gos_gdp_ts <- make_tsibble(gos_gdp)

gos_thw_ts <- make_tsibble(gos_thw)

# ===================================================
# DATASET LIST
# ===================================================

datasets <- list(
  gop_gdp = gop_gdp_ts,
  gop_thw = gop_thw_ts,
  gos_gdp = gos_gdp_ts,
  gos_thw = gos_thw_ts
)

# ===================================================
# FIT ARDL(2,1,0)
# ===================================================
#
# order = c(2,1,0)
#
# dependent variable:
#   2 lags
#
# bn:
#   1 lag
#
# covid:
#   contemporaneous only
#
# ===================================================

fit_ardl <- purrr::imap(
  datasets,
  
  ~ {
    
    ardl(
      
      formula =
        margin ~ bn + covid,
      
      data = as.data.frame(.x),
      
      order = c(2,1,0)
    )
  }
)

# ===================================================
# MODEL SUMMARIES
# ===================================================

purrr::iwalk(
  fit_ardl,
  
  ~ {
    
    cat("\n")
    cat("=================================================\n")
    cat("DATASET:", .y, "\n")
    cat("=================================================\n")
    
    print(summary(.x))
  }
)

# ===================================================
# COEFFICIENTS
# ===================================================

coef_results <- map_dfr(
  fit_ardl,
  
  ~ {
    
    s <- summary(.x)
    
    as_tibble(
      s$coefficients,
      rownames = "term"
    ) |>
      
      rename(
        estimate = Estimate,
        std.error = `Std. Error`,
        statistic = `t value`,
        p.value = `Pr(>|t|)`
      )
  },
  
  .id = "dataset"
)

coef_results

# ---------------------------------------------------
# SIGNIFICANCE TABLE
# ---------------------------------------------------

coef_sig <- coef_results |>
  
  mutate(
    
    significance =
      case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      )
  )

coef_sig
# ===================================================
# AIC / BIC COMPARISON
# ===================================================

model_stats <- map_dfr(
  fit_ardl,
  
  ~ {
    
    tibble(
      
      AIC =
        AIC(.x),
      
      BIC =
        BIC(.x),
      
      logLik =
        as.numeric(logLik(.x)),
      
      sigma2 =
        summary(.x)$sigma^2
    )
  },
  
  .id = "dataset"
)

model_stats |>
  arrange(AIC)

# ===================================================
# RESIDUAL DIAGNOSTICS
# ===================================================

diagnostics <- map_dfr(
  fit_ardl,
  
  ~ {
    
    resids <- residuals(.x)
    
    # Ljung-Box
    
    lb <- Box.test(
      resids,
      lag = 12,
      type = "Ljung-Box"
    )
    
    # Shapiro-Wilk
    
    shapiro <- shapiro.test(resids)
    
    # ARCH
    
    arch <- ArchTest(
      resids,
      lags = 12
    )
    
    tibble(
      
      lb_stat =
        as.numeric(lb$statistic),
      
      lb_p =
        lb$p.value,
      
      shapiro_stat =
        as.numeric(shapiro$statistic),
      
      shapiro_p =
        shapiro$p.value,
      
      arch_stat =
        as.numeric(arch$statistic),
      
      arch_p =
        arch$p.value
    )
  },
  
  .id = "dataset"
)

diagnostics

# ===================================================
# PERSISTENCE
# ===================================================

persistence_results <- map_dfr(
  fit_ardl,
  
  ~ {
    
    coefs <- coef(.x)
    
    phi1 <- coefs["L(margin, 1)"]
    
    phi2 <- coefs["L(margin, 2)"]
    
    tibble(
      
      ar1 = phi1,
      
      ar2 = phi2,
      
      persistence =
        phi1 + phi2
    )
  },
  
  .id = "dataset"
)

persistence_results

# ===================================================
# LONG-RUN MULTIPLIER
# ===================================================

long_run_results <- map_dfr(
  fit_ardl,
  
  ~ {
    
    coefs <- coef(.x)
    
    b0 <- coefs["bn"]
    
    b1 <- coefs["L(bn, 1)"]
    
    phi1 <- coefs["L(margin, 1)"]
    
    phi2 <- coefs["L(margin, 2)"]
    
    tibble(
      
      long_run_multiplier =
        (b0 + b1) /
        (1 - phi1 - phi2)
    )
  },
  
  .id = "dataset"
)

long_run_results


# ===================================================
# ECM REPRESENTATION
# ===================================================

ecm_models <- map(
  fit_ardl,
  uecm
)

# ---------------------------------------------------
# ECM summaries
# ---------------------------------------------------

purrr::iwalk(
  ecm_models,
  
  ~ {
    
    cat("\n")
    cat("=================================================\n")
    cat("ECM:", .y, "\n")
    cat("=================================================\n")
    
    print(summary(.x))
  }
)

# ===================================================
# STRUCTURAL BREAK TESTS
# ===================================================

break_results <- map_dfr(
  fit_ardl,
  
  ~ {
    
    resids <- residuals(.x)
    
    bp <- breakpoints(resids ~ 1)
    
    tibble(
      
      n_breaks =
        sum(!is.na(bp$breakpoints)),
      
      breakpoints =
        paste(
          bp$breakpoints,
          collapse = ", "
        )
    )
  },
  
  .id = "dataset"
)

break_results

# ===================================================
# RESIDUAL PLOTS
# ===================================================

plot_residuals <- function(model, title_name) {
  
  tibble(
    
    resid =
      residuals(model),
    
    fitted =
      fitted(model),
    
    time =
      seq_along(residuals(model))
  ) |>
    
    ggplot(
      aes(
        x = time,
        y = resid
      )
    ) +
    
    geom_line() +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    
    labs(
      title = paste(
        "Residuals:",
        title_name
      ),
      x = "",
      y = "Residuals"
    )
}

p1 <- plot_residuals(
  fit_ardl$gos_gdp,
  "GOS GDP"
)

p2 <- plot_residuals(
  fit_ardl$gos_thw,
  "GOS THW"
)

p3 <- plot_residuals(
  fit_ardl$gop_gdp,
  "GOP GDP"
)

p4 <- plot_residuals(
  fit_ardl$gop_thw,
  "GOP THW"
)

(p1 + p2) / (p3 + p4)

# ===================================================
# HISTOGRAMS
# ===================================================

plot_histogram <- function(model, title_name) {
  
  tibble(
    resid = residuals(model)
  ) |>
    
    ggplot(
      aes(x = resid)
    ) +
    
    geom_histogram(
      bins = 30
    ) +
    
    labs(
      title = paste(
        "Histogram:",
        title_name
      ),
      x = "Residuals",
      y = "Frequency"
    )
}

plot_histogram(
  fit_ardl$gos_gdp,
  "GOS GDP"
)

# ===================================================
# RESIDUAL ACF
# ===================================================

acf(
  residuals(fit_ardl$gos_gdp),
  main = "Residual ACF"
)

# ===================================================
# RESIDUAL PACF
# ===================================================

pacf(
  residuals(fit_ardl$gos_gdp),
  main = "Residual PACF"
)

# ===================================================
# FITTED VS ACTUAL
# ===================================================

plot_fitted <- function(model, data, title_name) {
  
  tibble(
    
    actual =
      data$margin[
        seq_along(fitted(model))
      ],
    
    fitted =
      fitted(model),
    
    time =
      seq_along(fitted(model))
  ) |>
    
    ggplot(
      aes(x = time)
    ) +
    
    geom_line(
      aes(
        y = actual,
        colour = "Actual"
      )
    ) +
    
    geom_line(
      aes(
        y = fitted,
        colour = "Fitted"
      )
    ) +
    
    labs(
      title = paste(
        "Actual vs Fitted:",
        title_name
      ),
      y = "Margins",
      x = ""
    )
}

plot_fitted(
  fit_ardl$gos_gdp,
  gos_gdp_ts,
  "GOS GDP"
)

# ===================================================
# MODEL COMPARISON:
# WITH vs WITHOUT COVID
# ===================================================

fit_compare <- purrr::imap(
  datasets,
  
  ~ {
    
    list(
      
      no_covid =
        ardl(
          
          margin ~ bn,
          
          data =
            as.data.frame(.x),
          
          order =
            c(2,1)
        ),
      
      with_covid =
        ardl(
          
          margin ~ bn + covid,
          
          data =
            as.data.frame(.x),
          
          order =
            c(2,1,0)
        )
    )
  }
)

# ===================================================
# COVID MODEL COMPARISON
# ===================================================

covid_compare <- map_dfr(
  fit_compare,
  
  ~ {
    
    tibble(
      
      model =
        c(
          "no_covid",
          "with_covid"
        ),
      
      AIC =
        c(
          AIC(.x$no_covid),
          AIC(.x$with_covid)
        ),
      
      BIC =
        c(
          BIC(.x$no_covid),
          BIC(.x$with_covid)
        )
    )
  },
  
  .id = "dataset"
)

covid_compare

# ===================================================
# HYPOTHESIS TEST:
# COVID EFFECT
# ===================================================

covid_hypothesis <- map_dfr(
  fit_ardl,
  
  ~ {
    
    s <- summary(.x)
    
    coef_table <- as.data.frame(s$coefficients)
    
    # Extract covid row
    
    covid_row <- coef_table["covid", ]
    
    tibble(
      
      estimate =
        covid_row$Estimate,
      
      std_error =
        covid_row$`Std. Error`,
      
      t_stat =
        covid_row$`t value`,
      
      p_value =
        covid_row$`Pr(>|t|)`,
      
      decision =
        if_else(
          covid_row$`Pr(>|t|)` < 0.05,
          "Reject H0",
          "Fail to Reject H0"
        ),
      
      interpretation =
        if_else(
          covid_row$`Pr(>|t|)` < 0.05,
          "COVID significantly affected margins",
          "COVID effect not statistically significant"
        )
    )
  },
  
  .id = "dataset"
)

covid_hypothesis

# ===================================================
# RESIDUAL DIAGNOSTIC PANELS
# (ARDL equivalent of gg_tsresiduals)
# ===================================================

library(feasts)

plot_ts_residuals <- function(model, title_name) {
  
  resid_tbl <- tibble(
    
    time =
      seq_along(residuals(model)),
    
    resid =
      as.numeric(residuals(model))
  )
  
  # -----------------------------------------------
  # Residual time plot
  # -----------------------------------------------
  
  p1 <- resid_tbl |>
    
    ggplot(
      aes(
        x = time,
        y = resid
      )
    ) +
    
    geom_line() +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    
    labs(
      title = paste(
        title_name,
        "- Residuals"
      ),
      x = "",
      y = ""
    )
  
  # -----------------------------------------------
  # ACF
  # -----------------------------------------------
  
  acf_tbl <- acf(
    resid_tbl$resid,
    plot = FALSE
  )
  
  acf_df <- tibble(
    lag = acf_tbl$lag[-1],
    acf = acf_tbl$acf[-1]
  )
  
  p2 <- acf_df |>
    
    ggplot(
      aes(
        x = lag,
        y = acf
      )
    ) +
    
    geom_col() +
    
    geom_hline(
      yintercept = c(
        -1.96 / sqrt(nrow(resid_tbl)),
        1.96 / sqrt(nrow(resid_tbl))
      ),
      linetype = "dashed"
    ) +
    
    labs(
      title = "Residual ACF",
      x = "Lag",
      y = "ACF"
    )
  
  # -----------------------------------------------
  # Histogram
  # -----------------------------------------------
  
  p3 <- resid_tbl |>
    
    ggplot(
      aes(x = resid)
    ) +
    
    geom_histogram(
      bins = 30
    ) +
    
    labs(
      title = "Residual Histogram",
      x = "",
      y = ""
    )
  
  # -----------------------------------------------
  # Combine
  # -----------------------------------------------
  
  (p1 / p2 / p3)
}

# ===================================================
# SHOW FOR ALL MODELS
# ===================================================

purrr::iwalk(
  fit_ardl,
  
  ~ print(
    
    plot_ts_residuals(
      .x,
      .y
    )
  )
)
