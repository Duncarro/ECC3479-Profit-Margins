#-------------------------------
# Robustness Checks
#-------------------------------
#1. controlling for COVID
#2. exploring time dynamics




#-----------------------
# Controlling for COVID
#-----------------------
gos_gdp_ts <- gos_gdp |> 
  as_tsibble(index = date) |> 
  mutate(date = yearquarter(date))

gos_thw_ts <- gos_thw |> 
  as_tsibble(index = date)  |> 
  mutate(date = yearquarter(date))

gop_gdp_ts <- gop_gdp |> 
  as_tsibble(index = date) |> 
  mutate(date = yearquarter(date))

gop_thw_ts <- gop_thw |> 
  as_tsibble(index = date) |> 
  mutate(date = yearquarter(date))



#-----
# Identifying significant lags
#-----

gos_gdp_ts <- gos_gdp_ts |>
  mutate(
    covid = if_else(
      date >= yearquarter("2020 Q1") &
        date <= yearquarter("2022 Q4"),
      1, 0
    )
  )

gos_gdp_ts |>
  gg_tsdisplay(
    `Total less mining_resid`,
    plot_type = "partial",
    lag_max = 20
  )
#ar2 sig

gop_gdp_ts |>
  gg_tsdisplay(
    `Total less mining_resid`,
    plot_type = "partial",
    lag_max = 20
  )
#ar2 sig

gos_gdp_ts |>
  gg_tsdisplay(
    `bn`,
    plot_type = "partial",
    lag_max = 20
  )
#ar1 sig

gop_thw_ts |>
  gg_tsdisplay(
    `bn`,
    plot_type = "partial",
    lag_max = 20
  )
#ar1 sig




#-----
# Lag creation
#-----

gos_gdp_ts <- gos_gdp_ts |>
  mutate(
    bn_lag1 = lag(bn, 1)
  )

gos_thw_ts <- gos_thw_ts |>
  mutate(
    bn_lag1 = lag(bn, 1)
  )

gop_gdp_ts <- gop_gdp_ts |>
  mutate(
    bn_lag1 = lag(bn, 1)
  )

gop_thw_ts <- gop_thw_ts |>
  mutate(
    bn_lag1 = lag(bn, 1)
  )





#-------
# Model Selection
#-------


fit <- gos_gdp_ts |>
  model(
    ar1 = ARIMA(`Total less mining_resid` ~ pdq(2,0,0))
  )

fit2 <- gos_gdp_ts |>
  model(
    ar1 = ARIMA(
      `Total less mining_resid` ~ covid + pdq(1,0,0)
    )
  )

fit22 <- gos_gdp_ts |>
  model(
    ar1 = ARIMA(
      `Total less mining_resid` ~ covid + pdq(2,0,0)
    )
  )


#################################
fit_ardl <- gos_gdp_ts |>
  model(
    ardl21 = ARIMA(
      `Total less mining_resid` ~ 
        bn + bn_lag1 + pdq(2,0,0)
    )
  )
#################################


fits <- gos_gdp_ts |>
  model(
    ar2          = ARIMA(`Total less mining_resid` ~ pdq(2,0,0)),
    
    covid_ar1    = ARIMA(
      `Total less mining_resid` ~ covid + pdq(1,0,0)
    ),
    
    covid_ar2    = ARIMA(
      `Total less mining_resid` ~ covid + pdq(2,0,0)
    )
  )


glance(fits)
tidy(fits)

augment(fits) |>
  features(.resid, ljung_box, lag = 8)

augment(fits) |>
  autoplot(`Total less mining_resid`) +
  geom_line(aes(y = .fitted, colour = .model))


fits |>
  select(ar2) |> 
  gg_tsresiduals()


fits |>
  select(covid_ar1) |> 
  gg_tsresiduals()

fits |>
  select(covid_ar2) |> 
  gg_tsresiduals()

fit |>
  gg_tsresiduals()



