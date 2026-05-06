##################################################################################
# This code was created by Güneş Kamber, James Morley, and Benjamin Wong,        #
# Article: Trend-cycle decomposition in the presence of large shocks,            #
# Journal: Journal of Economic Dynamics and Control,                             #
##################################################################################


# Clear the workspace -- comment out if you have loaded other packages already
#rm(list = ls(all = T))
#gc()

# Source required functions
source(here::here("src", "bnf_fcns.R"))
source(here::here("src", "library.R"))
source(here::here("src", "data_cleaning.R"))

# Read in US centric data to use for the demonstration
#usdata <- read.csv(file = 'us_data.csv', header = T, stringsAsFactors = F)
gdp_prep <-
  read.csv(file = "data/clean/gdp_ext.csv",
           header = T,
           stringsAsFactors = F)

gdp_prep <- gdp_prep |> 
  select(date, value) |>
  slice(1:266)

# Make the series GDP a 'ts' object
# type 'help(ts)' in the R console for more information
#gdp <- ts(data = usdata$GDPC1, end = c(2016, 2), frequency = 4)
gdp <- ts(data = gdp_prep$value,
          end = c(2025, 4),
          frequency = 4)

#Take logs and multiply by 100
y <-
  transform_series(y = gdp,
                   take_log = T,
                   pcode = "p1") # same as: log(raw_y) * 100.0

print(y)

# Example: Automatically determined delta and full sample mean demeaning method
cat("Example: log US real GDP\n\n")

bnfOutput <- bnf(
  as.vector(y),
  delta_select = 2,
  demean = "dm",
  iterative = 100,
  dynamic_bands = T,
  adjusted_bands = T,
  outliers = c(200:210, 243:258),
  window = 40,
  ib = T
)
# @delta_select: set to 0 if use fixed delta, 1 if max amp-to-noise, 2 (default) if min var(trend shocks)
# @demean = "nd", "sm", "dm", or "pm", where "nd" = no drift, "sm" = sample mean, "dm" = dynamic demeaning, "pm" = structural breaks
# @iterative: set to >1 for max number of iterations for iterative dynamic demeaning (e.g., 100 (default))
# Note: for demean = "pm", enter in e.g. 'breaks = c(100, 237)' to allow for breaks at set dates
# @dynamic_bands: set to T (default) for dynamic error bands, F for fixed standard error bands
# Note: enter in e.g. 'adjust_bands = T' and 'outliers = c(293, 294)' (obs. #'s in terms first differences) to adjusts for outlier observations when calculating error bands, F (default) for no adjustment, or remove argument to automatically adjust for outlier observations falling between 1 March 2020 and 30 September 2020 regardless of data frequency
# @window: rolling window length for dynamic demeaning and/or dynamic error bands (e.g., 40 (default) is 10 years for quarterly data)
# @ib: set to F if no iterative backcasting as in KMW2018 (just unconditional mean), set to T (default) if iterative backcasting
# defaults for other inputs are 'fixed_delta = 0.02' (set delta_select = 0),'p = 12', 'd0 = 0.005' (min delta), 'dt = 0.0005' (grid increment)

write.csv(bnfOutput$cycle, "gdp_aug.csv")

plot(
  bnfOutput,
  main = "AUS Output Gap",
  col = "red",
  secondary_col = "blue"
)
cat("\nPrinting out cycle data...\n")
print(bnfOutput) # comment this command to stop the cycle data being printed to the console
cat('\n')

gdp_prep <- gdp_prep |> 
  mutate(bn = bnfOutput$cycle)

gdp_prep <- gdp_prep  |> 
  as_tibble() |> 
  mutate(date = as.Date(date))

gdp_prep |> 
  filter(date > as.Date("2002-09-01")) |> 
  ggplot(aes(x=date,y=bn)) + geom_line()

write.csv(gdp_prep, "data/clean/gdp_aug.csv")





###############################
# THW                         #
###############################

thw_prep <-
  read.csv(file = "data/clean/thw_ext.csv",
           header = T,
           stringsAsFactors = F)

thw_prep <- thw_prep |> 
  select(date, value) |>
  slice(1:266)

# Make the series GDP a 'ts' object
# type 'help(ts)' in the R console for more information
#gdp <- ts(data = usdata$GDPC1, end = c(2016, 2), frequency = 4)
thw <- ts(data = thw_prep$value,
          end = c(2025, 4),
          frequency = 4)

#Take logs and multiply by 100
y <-
  transform_series(y = thw,
                   take_log = T,
                   pcode = "p1") # same as: log(raw_y) * 100.0

print(y)


bnfOutput <- bnf(
  as.vector(y),
  delta_select = 2,
  demean = "dm",
  iterative = 100,
  dynamic_bands = T,
  adjusted_bands = T,
  outliers = c(200:210, 243:258),
  window = 40,
  ib = T
)
# @delta_select: set to 0 if use fixed delta, 1 if max amp-to-noise, 2 (default) if min var(trend shocks)
# @demean = "nd", "sm", "dm", or "pm", where "nd" = no drift, "sm" = sample mean, "dm" = dynamic demeaning, "pm" = structural breaks
# @iterative: set to >1 for max number of iterations for iterative dynamic demeaning (e.g., 100 (default))
# Note: for demean = "pm", enter in e.g. 'breaks = c(100, 237)' to allow for breaks at set dates
# @dynamic_bands: set to T (default) for dynamic error bands, F for fixed standard error bands
# Note: enter in e.g. 'adjust_bands = T' and 'outliers = c(293, 294)' (obs. #'s in terms first differences) to adjusts for outlier observations when calculating error bands, F (default) for no adjustment, or remove argument to automatically adjust for outlier observations falling between 1 March 2020 and 30 September 2020 regardless of data frequency
# @window: rolling window length for dynamic demeaning and/or dynamic error bands (e.g., 40 (default) is 10 years for quarterly data)
# @ib: set to F if no iterative backcasting as in KMW2018 (just unconditional mean), set to T (default) if iterative backcasting
# defaults for other inputs are 'fixed_delta = 0.02' (set delta_select = 0),'p = 12', 'd0 = 0.005' (min delta), 'dt = 0.0005' (grid increment)

write.csv(bnfOutput$cycle, "thw_bn.csv")

plot(
  bnfOutput,
  main = "THW Output Gap",
  col = "red",
  secondary_col = "blue"
)
cat("\nPrinting out cycle data...\n")
print(bnfOutput) # comment this command to stop the cycle data being printed to the console
cat('\n')

thw_prep <- thw_prep |> 
  mutate(bn = bnfOutput$cycle)

thw_prep <- thw_prep  |> 
  as_tibble() |> 
  mutate(date = as.Date(date))

thw_prep |> 
  filter(date > as.Date("2002-09-01")) |> 
  ggplot(aes(x=date,y=bn)) + geom_line()

write.csv(thw_prep, "data/clean/thw_aug.csv")


# EOF
