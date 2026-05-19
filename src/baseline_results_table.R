library(texreg)
library(sandwich)

##############################
# GOP - GDP
##############################

gop_gdp_bn <- lm(
  `Total less mining_resid` ~ bn,
  data = gop_gdp
)

gop_gdp_hp <- lm(
  `Total less mining_resid` ~ hp,
  data = gop_gdp
)

gop_gdp_cf <- lm(
  `Total less mining_resid` ~ cf,
  data = gop_gdp
)

##############################
# GOP - THW
##############################

gop_thw_bn <- lm(
  `Total less mining_resid` ~ bn,
  data = gop_thw
)

gop_thw_hp <- lm(
  `Total less mining_resid` ~ hp,
  data = gop_thw
)

gop_thw_cf <- lm(
  `Total less mining_resid` ~ cf,
  data = gop_thw
)

##############################
# GOS - GDP
##############################

gos_gdp_bn <- lm(
  `Total less mining_resid` ~ bn,
  data = gos_gdp
)

gos_gdp_hp <- lm(
  `Total less mining_resid` ~ hp,
  data = gos_gdp
)

gos_gdp_cf <- lm(
  `Total less mining_resid` ~ cf,
  data = gos_gdp
)

##############################
# GOS - THW
##############################

gos_thw_bn <- lm(
  `Total less mining_resid` ~ bn,
  data = gos_thw
)

gos_thw_hp <- lm(
  `Total less mining_resid` ~ hp,
  data = gos_thw
)

gos_thw_cf <- lm(
  `Total less mining_resid` ~ cf,
  data = gos_thw
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
    "bn" = "Cycle"
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
  
  caption = "Conditional Cyclicality: GOP",
  
  label = "tab:gop_cycle",
  
  file = "output/gop_cycle.tex"
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
    "bn" = "Cycle"
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
  
  caption = "Conditional Cyclicality: GOS",
  
  label = "tab:gos_cycle",
  
  file = "output/gos_cycle.tex"
)