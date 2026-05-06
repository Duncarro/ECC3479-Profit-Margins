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
  left_join(bn_thw  %>% select(date, bn  = bn),  by = "date") 


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
