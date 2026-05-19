library(texreg)
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
# Robust SE Function
##############################

nw <- function(x) {
  sqrt(diag(NeweyWest(x)))
}

##############################
# GOP TABLE
##############################

texreg(
  l = list(
    
    gop_gdp_cf,
    gop_gdp_hp,
    gop_gdp_bn,
    
    gop_thw_cf,
    gop_thw_hp,
    gop_thw_bn
  ),
  
  custom.model.names = c(
    "GDP: CF",
    "GDP: HP",
    "GDP: BN",
    "THW: CF",
    "THW: HP",
    "THW: BN"
  ),
  
  custom.coef.map = list(
    "cf" = "Cycle",
    "hp" = "Cycle",
    "bn" = "Cycle",
    
    "covid" = "COVID",
    
    "cf:covid" = "Cycle $\\times$ COVID",
    "hp:covid" = "Cycle $\\times$ COVID",
    "bn:covid" = "Cycle $\\times$ COVID"
  ),
  
  override.se = list(
    
    nw(gop_gdp_cf),
    nw(gop_gdp_hp),
    nw(gop_gdp_bn),
    
    nw(gop_thw_cf),
    nw(gop_thw_hp),
    nw(gop_thw_bn)
  ),
  
  stars = c(0.001, 0.01, 0.05),
  
  digits = 3,
  
  booktabs = TRUE,
  
  caption = "Conditional Cyclicality with COVID Interaction: GOP",
  
  label = "tab:gop_covid",
  
  file = "output/gop_covid.tex"
)

##############################
# GOS TABLE
##############################

texreg(
  l = list(
    
    gos_gdp_cf,
    gos_gdp_hp,
    gos_gdp_bn,
    
    gos_thw_cf,
    gos_thw_hp,
    gos_thw_bn
  ),
  
  custom.model.names = c(
    "GDP: CF",
    "GDP: HP",
    "GDP: BN",
    "THW: CF",
    "THW: HP",
    "THW: BN"
  ),
  
  custom.coef.map = list(
    "cf" = "Cycle",
    "hp" = "Cycle",
    "bn" = "Cycle",
    
    "covid" = "COVID",
    
    "cf:covid" = "Cycle $\\times$ COVID",
    "hp:covid" = "Cycle $\\times$ COVID",
    "bn:covid" = "Cycle $\\times$ COVID"
  ),
  
  override.se = list(
    
    nw(gos_gdp_cf),
    nw(gos_gdp_hp),
    nw(gos_gdp_bn),
    
    nw(gos_thw_cf),
    nw(gos_thw_hp),
    nw(gos_thw_bn)
  ),
  
  stars = c(0.001, 0.01, 0.05),
  
  digits = 3,
  
  booktabs = TRUE,
  
  caption = "Conditional Cyclicality with COVID Interaction: GOS",
  
  label = "tab:gos_covid",
  
  file = "output/gos_covid.tex"
)
