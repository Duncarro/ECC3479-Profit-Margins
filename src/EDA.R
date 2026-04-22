#EDA SCRIPT

source("src/library.R")
source("src/functions.R")
source("src/output.R")


#Descriptive statistics
desc_stats_ts(gop_wide, value_col = Total)
desc_stats_ts(gos_wide, value_col = Total)

plot_highlight(gop, "Total") #fix
#The total fits roughtly in the middle of the industry distribution

plot_highlight(gos, "Total") #fix 
#The total sits just above the median


#Variance change plots
c1 #GOS
#Displays that mining plays a major contribution to the aggregate margin
#Shows that the total value is reduced when they are subtracted

c2 #GOP 
#Displays that the subtraction of mining makes the aggregate margin less volatile


### FURTHER TIME SERIES EDA ###
###############################


# formatting tsibble
gop_ts <- gop |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(key = industry, index = date)

gos_ts <- gos |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(key = industry, index = date)


# Seasonal plots
gop_ts |> 
  filter(industry == "Total less mining") |> 
  gg_season()
# Shows that margins have expanded across time (trend component)

gos_ts |> 
  filter(industry == "Total less mining") |> 
  gg_season()
# The change in GOS/GVA across time has been much more stable and constant

# Subseries plots
gop_ts |> 
  filter(industry == "Total less mining") |> 
  gg_subseries()
#Shows a level shift at 2015. Q2 and Q3 have drastic expanses in response to COVID-19

gos_ts |> 
  filter(industry == "Total less mining") |> 
  gg_subseries()
# The change has been much more steady, with Q2 and Q3 having less of a response to COVID-19
#!! shows gos is less volatile than GOP, possible connection to GOP being more responsive to business cycle (conjecture)


# STL Decomposition
gop_ts |>
  filter(industry == "Total less mining") |>
  model(STL(margin)) |>
  components() |>
  autoplot()
#Trend: wavy trend (cyclical pattern), relatively constant until COVID which caused massive spike. Has reduced to pre-pandemic trend (link to AR MODELS?)
#Seasonality: severe heteroskedastic seasonality, variance increases even prior to covid, with maxima occuring prior to pandemic (counterfactual to covid causing expanse in margins)
#Remainder: still patterns, inferring the decomp didnt capture all the variation in the original series

gos_ts |>
  filter(industry == "Total less mining") |>
  model(STL(margin)) |>
  components() |>
  autoplot()
#Overall similar dynamics to GOP decomposition
#Seasonality: tapers in middle of series


# Seasonality deepdive 
fit_gos <- gos_ts |>
  filter(industry == "Total less mining") |>
  model(STL(margin))

components_gos <- fit_gos |> 
  components()

components_gos |>
  ggplot(aes(x = date, y = season_year)) +
  geom_line()


#WIP - Naive estimates of Cyclical component

components_gos <- components_gos |>
  mutate(cycle = margin - trend)

components_gos |> 
  ggplot(aes(x = date, y = cycle)) +
  geom_line()

