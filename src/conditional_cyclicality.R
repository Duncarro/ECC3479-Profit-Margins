#Conditional Cylicality

source(here::here("src", "library.R"))


####################################### 
# Obtaining Cyclical Margin Component #
#######################################

#Creating deterministic trend component 
t <- seq_len(nrow(gos_wide_margin)) #gos and gop have same # of obs (n = 94)

gos_wide_margin <- gos_wide_margin |> 
  mutate(t = t)

gop_wide_margin <- gop_wide_margin |> 
  mutate(t = t)

#Run regressions on deterministic trend over every industry (column)
#Model: margin = a0 + a1*t + e

models_gop <- gop_wide_margin %>%
  select(-date) %>%
  map(~ lm(.x ~ t))

models_gos <- gos_wide_margin %>%
  select(-date) %>%
  map(~ lm(.x ~ t))


lapply(models_gop, summary )

lapply(models_gos, summary)
#majority of the trend components are statistically significant
#positive sign that there were trend across the margin series


#Extracting residuals for each industry (column)

#GOP
gop_resid <- map_dfc(models_gop, resid)
names(gop_resid) <- paste0(names(models_gop), "_resid")

gop_resid <- dplyr::bind_cols(
  date = gop_wide_margin$date,
  gop_resid
)


#GOS
gos_resid <- map_dfc(models_gos, resid)
names(gos_resid) <- paste0(names(models_gos), "_resid")

gos_resid <- dplyr::bind_cols(
  date = gos_wide_margin$date,
  gos_resid
)





###############################################
# Obtaining Cyclical Business Cycle Component #
###############################################

# HP FILTER #

#GDP
hp_gdp <- hpfilter(gdp_ext$log, freq = 1600)

hp_gdp <- gdp_ext |> 
  mutate(
    trend = hp_gdp$trend*100,
    cycle = hp_gdp$cycle*100,
    date = as.Date(gdp_ext$date)
  ) |> 
  select(date, cycle)

#THW
hp_thw <- hpfilter(thw_ext$log, freq = 1600)

hp_thw <- thw_ext |> 
  mutate(
    trend = hp_thw$trend*100,
    cycle = hp_thw$cycle*100,
    date = as.Date(thw_ext$date)
  ) |> 
  select(date, cycle)




# CF FILTER #
cf_gdp <- gdp_ext |>
  transmute(
    date = as.Date(date),
    cycle = cffilter(log, pl = 6, pu = 32, root = TRUE)$cycle * 100
  )

#THW
cf_thw <- thw_ext |>
  transmute(
    date = as.Date(date),
    cycle = cffilter(log, pl = 6, pu = 32, root = TRUE)$cycle * 100
  )


###############################################
# BN FILTER:            bn_run_aug.R Required #
###############################################

bn_gdp <- bn_gdp |> 
  filter(date > as.Date("2002-06-01")) |> 
  select(date, bn)

bn_thw <- bn_thw |> 
  filter(date > as.Date("2002-06-01")) |> 
  select(date, bn)



#EOF
