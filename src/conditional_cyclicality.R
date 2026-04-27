#Conditional Cylicality

if (!exists("gop")) {
  source("src/library.R")
}

####################################### 
# Obtaining Cyclical Margin Component #
#######################################

#Creating deterministic trend component 
t <- seq_len(nrow(gos_wide)) #gos and gop have same # of obs (n = 94)

gos_wide <- gos_wide |> 
  mutate(t = t)

gop_wide <- gop_wide |> 
  mutate(t = t)

#Run regressions on deterministic trend over every industry (column)
#Model: margin = a0 + a1*t + e

models_gop <- gop_wide %>%
  select(-date) %>%
  map(~ lm(.x ~ t))

models_gos <- gos_wide %>%
  select(-date) %>%
  map(~ lm(.x ~ t))


lapply(models_gop, summary)

lapply(models_gos, summary)
#majority of the trend components are statistically significant
#positive sign that there were trend across the margin series


#Extracting residuals for each industry (column)

#GOP
gop_resid <- map_dfc(models_gop, resid)
names(gop_resid) <- paste0(names(models_gop), "_resid")

gop_resid <- dplyr::bind_cols(
  date = gop_wide$date,
  gop_resid
)


#GOS
gos_resid <- map_dfc(models_gos, resid)
names(gos_resid) <- paste0(names(models_gos), "_resid")

gos_resid <- dplyr::bind_cols(
  date = gos_wide$date,
  gos_resid
)





###############################################
# Obtaining Cyclical Business Cycle Component #
###############################################

#FILTERS NEEDED: HP, B



