#Runs the regressions for the conditional cyclicality analysis
source("src/conditional_cyclicality.R")


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


##############################
# Adding COVID Dummy         #
##############################

gos_gdp <- gos_gdp |> 
mutate(covid = if_else(
  date >= as.Date("2020-01-01") & date <= as.Date("2023-03-31"),
  1, 0
))

gos_thw <- gos_thw |> 
  mutate(covid = if_else(
    date >= as.Date("2020-01-01") & date <= as.Date("2023-03-31"),
    1, 0
  ))


gop_gdp <- gop_gdp |> 
  mutate(covid = if_else(
    date >= as.Date("2020-01-01") & date <= as.Date("2023-03-31"),
    1, 0
  ))

gop_thw <- gop_thw |> 
  mutate(covid = if_else(
    date >= as.Date("2020-01-01") & date <= as.Date("2023-03-31"),
    1, 0
  ))

#########################################
# Base Elasticity Model Regressions     #
#########################################
#Selected margin: Total less mining


###  GOP-GDP  ###
spec <- gop_gdp |> 
  select(date,
         margin = 'Total less mining_resid',
         cf, bn, hp) |> 
  arrange(date)

filters <- c("cf", "hp", "bn")

models <- lapply(filters, function(f) {
  lm(as.formula(paste0("margin ~ ", f)), data = spec)
})

names(models) <- toupper(filters)


modelsummary(
  models,
  vcov = sandwich::NeweyWest,
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj",
  coef_map = c(
    "cf"  = "Cycle",
    "hp"  = "Cycle",
    "bn"  = "Cycle"
  ),
  title = "Gross Operating Profit and GDP Business Cycle (Across Filters)"
)




###  GOP-THW  ###


spec <- gop_thw |> 
  select(date,
         margin = 'Total less mining_resid',
         cf,  bn, hp) |> 
  arrange(date)

filters <- c("cf", "hp", "bn")

models <- lapply(filters, function(f) {
  lm(as.formula(paste0("margin ~ ", f)), data = spec)
})

names(models) <- toupper(filters)


modelsummary(
  models,
  vcov = sandwich::NeweyWest,
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj",
  coef_map = c(
    "cf"  = "Cycle",
    "hp"  = "Cycle",
    "bn"  = "Cycle"
  ),
  title = "Gross Operating Profit and THW Business Cycle (Across Filters)"
)




###  GOS-GDP  ###
spec <- gos_gdp |> 
  select(date,
         margin = 'Total less mining_resid',
         cf,  bn, hp) |> 
  arrange(date)

filters <- c("cf", "hp", "bn")

models <- lapply(filters, function(f) {
  lm(as.formula(paste0("margin ~ ", f)), data = spec)
})

names(models) <- toupper(filters)


modelsummary(
  models,
  vcov = sandwich::NeweyWest,
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj",
  coef_map = c(
    "cf"  = "Cycle",
    "hp"  = "Cycle",
    "bn"  = "Cycle"
  ),
  title = "Gross Operating Surplus and GDP Business Cycle (Across Filters)"
)




###  GOS-THW  ###
spec <- gos_thw |> 
  select(date,
         margin = 'Total less mining_resid',
         cf,  bn, hp) |> 
  arrange(date)

filters <- c("cf", "hp", "bn")

models <- lapply(filters, function(f) {
  lm(as.formula(paste0("margin ~ ", f)), data = spec)
})

names(models) <- toupper(filters)


modelsummary(
  models,
  vcov = sandwich::NeweyWest,
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj",
  coef_map = c(
    "cf"  = "Cycle",
    "hp"  = "Cycle",
    "bn"  = "Cycle"
  ),
  title = "Gross Operating Surplus and THW Business Cycle (Across Filters)"
)


#EOF




##############################
# GOP-GDP Models
##############################

spec_gop <- gop_gdp |>
  select(
    date,
    margin = `Total less mining_resid`,
    cf, hp, bn
  ) |>
  arrange(date)

gop_models <- lapply(filters, function(f) {
  lm(as.formula(paste0("margin ~ ", f)), data = spec_gop)
})

names(gop_models) <- c(
  "GOP: CF",
  "GOP: HP",
  "GOP: BN"
)

##############################
# GOS-GDP Models
##############################

spec_gos <- gos_gdp |>
  select(
    date,
    margin = `Total less mining_resid`,
    cf, hp, bn
  ) |>
  arrange(date)

gos_models <- lapply(filters, function(f) {
  lm(as.formula(paste0("margin ~ ", f)), data = spec_gos)
})

names(gos_models) <- c(
  "GOS: CF",
  "GOS: HP",
  "GOS: BN"
)

##############################
# Combined Table
##############################

combined_models <- c(
  gop_models,
  gos_models
)

modelsummary(
  combined_models,
  vcov = sandwich::NeweyWest,
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj",
  
  coef_map = c(
    "cf" = "Cycle",
    "hp" = "Cycle",
    "bn" = "Cycle"
  ),
  
  title = "Conditional Cyclicality of Profit Margins with GDP"
)

############################
# Extended Covid Models
############################
library(modelsummary)
library(kableExtra)
library(dplyr)
library(sandwich)

##############################
# GOP-GDP Models
##############################

spec_gop <- gop_gdp |>
  select(
    date,
    margin = `Total less mining_resid`,
    cf, hp, bn
  ) |>
  arrange(date) |>
  mutate(
    covid = ifelse(date >= as.Date("2020-03-01"), 1, 0)
  )

gop_models <- lapply(filters, function(f) {
  lm(
    as.formula(
      paste0("margin ~ ", f, " + covid + ", f, ":covid")
    ),
    data = spec_gop
  )
})

names(gop_models) <- c(
  "GOP: CF",
  "GOP: HP",
  "GOP: BN"
)

##############################
# GOS-GDP Models
##############################

spec_gos <- gos_gdp |>
  select(
    date,
    margin = `Total less mining_resid`,
    cf, hp, bn
  ) |>
  arrange(date) |>
  mutate(
    covid = ifelse(date >= as.Date("2020-03-01"), 1, 0)
  )

gos_models <- lapply(filters, function(f) {
  lm(
    as.formula(
      paste0("margin ~ ", f, " + covid + ", f, ":covid")
    ),
    data = spec_gos
  )
})

names(gos_models) <- c(
  "GOS: CF",
  "GOS: HP",
  "GOS: BN"
)

##############################
# Combined Table
##############################

combined_models <- c(
  gop_models,
  gos_models
)

##############################
# Create LaTeX Table
##############################

latex_table <- modelsummary(
  combined_models,
  vcov = sandwich::NeweyWest,
  stars = c('*' = .05, '**' = .01, '***' = .001),
  statistic = "({std.error})",
  gof_omit = "IC|Log|Adj|F",
  
  coef_map = c(
    "cf" = "CF",
    "hp" = "HP",
    "bn" = "BN",
    "covid" = "COVID",
    "cf:covid" = "CF $\\times$ COVID",
    "hp:covid" = "HP $\\times$ COVID",
    "bn:covid" = "BN $\\times$ COVID"
  ),
  
  title = "Conditional Cyclicality of Profit Margins with GDP",
  
  output = "latex"
)

##############################
# Export .tex File
##############################

writeLines(
  latex_table,
  "outputs/conditional_cyclicality_gdp.tex"
)







library(modelsummary)
library(sandwich)

##############################
# GOP - GDP
##############################

gop_gdp_bn <- lm(
  `Total less mining_resid` ~ bn + covid + bn:covid,
  data = gop_gdp
)

gop_gdp_hp <- lm(
  `Total less mining_resid` ~ hp + covid + hp:covid,
  data = gop_gdp
)

gop_gdp_cf <- lm(
  `Total less mining_resid` ~ cf + covid + cf:covid,
  data = gop_gdp
)

##############################
# GOP - THW
##############################

gop_thw_bn <- lm(
  `Total less mining_resid` ~ bn + covid + bn:covid,
  data = gop_thw
)

gop_thw_hp <- lm(
  `Total less mining_resid` ~ hp + covid + hp:covid,
  data = gop_thw
)

gop_thw_cf <- lm(
  `Total less mining_resid` ~ cf + covid + cf:covid,
  data = gop_thw
)

##############################
# GOS - GDP
##############################

gos_gdp_bn <- lm(
  `Total less mining_resid` ~ bn + covid + bn:covid,
  data = gos_gdp
)

gos_gdp_hp <- lm(
  `Total less mining_resid` ~ hp + covid + hp:covid,
  data = gos_gdp
)

gos_gdp_cf <- lm(
  `Total less mining_resid` ~ cf + covid + cf:covid,
  data = gos_gdp
)

##############################
# GOS - THW
##############################

gos_thw_bn <- lm(
  `Total less mining_resid` ~ bn + covid + bn:covid,
  data = gos_thw
)

gos_thw_hp <- lm(
  `Total less mining_resid` ~ hp + covid + hp:covid,
  data = gos_thw
)

gos_thw_cf <- lm(
  `Total less mining_resid` ~ cf + covid + cf:covid,
  data = gos_thw
)

##############################
# Combine Models
##############################

models <- list(
  "GOP GDP: BN" = gop_gdp_bn,
  "GOP GDP: HP" = gop_gdp_hp,
  "GOP GDP: CF" = gop_gdp_cf,
  
  "GOP THW: BN" = gop_thw_bn,
  "GOP THW: HP" = gop_thw_hp,
  "GOP THW: CF" = gop_thw_cf,
  
  "GOS GDP: BN" = gos_gdp_bn,
  "GOS GDP: HP" = gos_gdp_hp,
  "GOS GDP: CF" = gos_gdp_cf,
  
  "GOS THW: BN" = gos_thw_bn,
  "GOS THW: HP" = gos_thw_hp,
  "GOS THW: CF" = gos_thw_cf
)

##############################
# Export LaTeX Table
##############################
latex_table <- modelsummary(
  models,
  
  vcov = sandwich::NeweyWest,
  
  statistic = "({std.error})",
  
  stars = c(
    '*' = .05,
    '**' = .01,
    '***' = .001
  ),
  
  coef_map = c(
    "bn" = "BN",
    "hp" = "HP",
    "cf" = "CF",
    "covid" = "COVID",
    "bn:covid" = "BN $\\times$ COVID",
    "hp:covid" = "HP $\\times$ COVID",
    "cf:covid" = "CF $\\times$ COVID"
  ),
  
  gof_omit = "AIC|BIC|Log.Lik|Adj|F",
  
  title = "Conditional Cyclicality of Profit Margins",
  
  output = "latex"
)

latex_code <- tinytable::to_latex(latex_table)

writeLines(
  latex_code,
  "outputs/conditional_cyclicality.tex"
)
writeLines(
  latex_table,
  "outputs/conditional_cyclicality.tex"
)
