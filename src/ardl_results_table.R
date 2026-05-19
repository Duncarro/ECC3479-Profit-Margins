##############################################################################
# Packages
##############################################################################

library(ARDL)
library(purrr)
library(modelsummary)




##############################
# GOS Merged Series          #
##############################

gos_gdp <- gos_resid %>%
  left_join(cf_gdp  %>% select(date, cf  = cycle),  by = "date") %>%
  left_join(hp_gdp  %>% select(date, hp  = cycle),  by = "date") %>%
  left_join(bn_gdp  %>% select(date, bn  = bn),  by = "date") 


gos_thw <- gos_resid %>%
  left_join(cf_thw  %>% select(date, cf  = cycle),  by = "date") %>%
  left_join(hp_thw  %>% select(date, hp  = cycle),  by = "date") %>%
  left_join(bn_thw  %>% select(date, bn  = bn),  by = "date") 

##############################
# GOP Merged Series          #
##############################

gop_gdp <- gop_resid %>%
  left_join(cf_gdp  %>% select(date, cf  = cycle),  by = "date") %>%
  left_join(hp_gdp  %>% select(date, hp  = cycle),  by = "date") %>%
  left_join(bn_gdp  %>% select(date, bn  = bn),  by = "date") 


gop_thw <- gop_resid %>%
  left_join(cf_thw  %>% select(date, cf  = cycle),  by = "date") %>%
  left_join(hp_thw  %>% select(date, hp  = cycle),  by = "date") %>%
  left_join(bn_thw  %>% select(date, bn  = bn),  by = "date") 


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


gop_gdp_ts <- make_tsibble(gop_gdp)

gop_thw_ts <- make_tsibble(gop_thw)

gos_gdp_ts <- make_tsibble(gos_gdp)

gos_thw_ts <- make_tsibble(gos_thw)


datasets <- list(
  gop_gdp = gop_gdp_ts,
  gop_thw = gop_thw_ts,
  gos_gdp = gos_gdp_ts,
  gos_thw = gos_thw_ts
)

##############################################################################
# ARDL Models
##############################################################################

fit_ardl <- purrr::imap(
  datasets,
  
  ~ ardl(
    
    formula =
      margin ~ cf + covid,
    
    data =
      as.data.frame(.x),
    
    order =
      c(2,1,0)
  )
)

##############################################################################
# Convert ARDL Objects to lm Objects
##############################################################################

ardl_models <- lapply(
  fit_ardl,
  to_lm
)

##############################################################################
# Model Names
##############################################################################

names(ardl_models) <- c(
  "GOS GDP",
  "GOP GDP",
  "GOS THW",
  "GOP THW"
)

##############################################################################
# Generate LaTeX Table
##############################################################################

latex_code <- modelsummary(
  
  ardl_models,
  
  stars = c(
    "*" = .05,
    "**" = .01,
    "***" = .001
  ),
  
  statistic = "std.error",
  
  title = "ARDL(2,1,0) Estimation Results",
  
  gof_omit = "IC|Log|Adj|F",
  
  coef_map = c(
    
    "L(margin, 1)" = "Margin (t-1)",
    
    "L(margin, 2)" = "Margin (t-2)",
    
    "cf" = "Business Cycle",
    
    "L(cf, 1)" = "Business Cycle (t-1)",
    
    "covid" = "COVID Dummy"
  ),
  
  output = "latex"
)

##############################################################################
# Print LaTeX Code
##############################################################################

cat(as.character(latex_code))