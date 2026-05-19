#################################################################################
# This script conducts analysis on the components of the profit margin measures #
#################################################################################

# WIP # # WIP # # WIP # # WIP # # WIP # # WIP # # WIP # # WIP # # WIP # # WIP # # WIP # # WIP # 

source("src/library.R")
thw_ext

gop_comp_wide <- gop_comp %>%
  filter(industry == "Total less mining") |> 
  pivot_wider(
    names_from = industry,
    values_from = c(gop, sales, margin)
  ) |> 
  select(date, gop = `gop_Total less mining`, sales = `sales_Total less mining`, margin = `margin_Total less mining`) |> 
  mutate(trend = 1:n(),
         hp = gop_gdp$hp)

#detrending components

fit_sales <- lm(sales ~ trend, data = gop_comp_wide)
sales_resid <- fit_sales$residuals 

fit_gop <- lm(gop ~ trend, data = gop_comp_wide)
gop_resid <- fit_gop$residuals 



####since macro series, can decompose??
library("mFilter")

hp_sales <- hpfilter(log(gop_comp_wide$sales), freq = 1600)

gop_comp_wide <- gop_comp_wide %>%
  mutate(
    hp_sales_cycle = 100 * hp_sales$cycle
  )

hp_gop <- hpfilter(log(gop_comp_wide$gop), freq = 1600)

gop_comp_wide <- gop_comp_wide %>%
  mutate(
    hp_gop_cycle = 100 * hp_gop$cycle
  )





#visual comp to hp gdp

plot_data <- gop_comp_wide %>%
  mutate(
    hp            = scale(hp)[,1],
    hp_gop_cycle  = scale(hp_gop_cycle)[,1],
    hp_sales_cycle = scale(hp_sales_cycle)[,1]
  ) %>%
  select(date, hp, hp_gop_cycle, hp_sales_cycle) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "value"
  )
ggplot(plot_data, aes(date, value, colour = series)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Standardised Cyclical Components",
    x = NULL,
    y = "Standard Deviations",
    colour = NULL
  )




#cointegration
library(urca)

gop_comp_wide <- gop_comp %>%
  filter(industry == "Total less mining") |> 
  pivot_wider(
    names_from = industry,
    values_from = c(gop, sales, margin)
  ) |> 
  select(date, gop = `gop_Total less mining`, sales = `sales_Total less mining`, margin = `margin_Total less mining`) |> 
  mutate(trend = 1:n(),
         hp = gop_gdp$hp)

summary(ur.df(log(gop_comp_wide$gop), type = "trend", lags = 4))
summary(ur.df(log(gop_comp_wide$sales), type = "trend", lags = 4))


#johnannsen test

library(urca)
gdp_ext <- gdp_ext |> 
  filter(date > "2002-06-01")

gop_comp_wide <- gop_comp_wide |> 
  left_join(gdp_ext, by = "date")

data_ci <- gop_comp_wide %>%
  select(sales, gop) %>%
  mutate(across(everything(), log)) %>%
  na.omit()

colnames(data_ci) <- c("sales", "gop")

johansen <- ca.jo(
  data_ci,
  type = "trace",
  ecdet = "trend",
  K = 2
)

summary(johansen)
