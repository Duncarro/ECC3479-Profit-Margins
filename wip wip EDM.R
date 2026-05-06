# Install if needed
install.packages(c("Rlibeemd", "tidyverse", "zoo", "mFilter"))

library(Rlibeemd)
library(tidyverse)
library(zoo)
library(mFilter)

set.seed(123)

imfs <- ceemdan(
  gdp$log,
  ensemble_size = 250,   # number of noise realizations
  noise_strength = 0.2,  # typical choice
  S_number = 4,
  num_siftings = 50
)

imf_df <- as.data.frame(imfs)

# Name columns IMF1, IMF2, ...
colnames(imf_df) <- paste0("IMF", 1:ncol(imf_df))

# Add date
imf_df$date <- as.Date(gdp$date)

# Convert to long format for plotting
imf_long <- imf_df %>%
  pivot_longer(-date, names_to = "imf", values_to = "value")

ggplot(imf_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ imf, scales = "free_y") +
  theme_minimal() +
  labs(title = "CEEMDAN Decomposition of Log GDP")




library(pracma)

dominant_period <- function(x) {
  spec <- spectrum(x, plot = FALSE)
  freq <- spec$freq[which.max(spec$spec)]
  return(1 / freq)
}

periods <- apply(imfs, 2, dominant_period)

data.frame(
  IMF = paste0("IMF", 1:length(periods)),
  Period = periods
)


test <- hp_gdp |> 
  mutate(imf = imf_df$IMF4,
         cf = cf_gdp$cycle,
         avg_cycle = rowMeans(across(c(cycle, imf, cf)), na.rm = TRUE),
         gop = gop_resid$`Total less mining_resid`,
         gos = gos_resid$`Total less mining`)

test <- test |>
  select(date, cycle, imf, cf, avg_cycle, gop, gos)

test <- test |> 
  pivot_longer(-date, names_to = "model", values_to = "value")

ggplot(test, aes(x = date, y = value, colour = model)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    title = "Cycle vs IMF",
    x = NULL,
    y = "Value",
    colour = "Series"
  )




######
##Regs!
test_wide <- test |>
  pivot_wider(
    names_from = model,
    values_from = `value`
  )
test_clean <- test_wide |>
  mutate(
    cycle = cycle[,1],
    imf   = imf[,1],
    cf    = cf[,1],
    avg_cycle = avg_cycle[,1],
    gop   = gop[,1],
    gos   = gos[,1]
  ) |>
  select(date, cycle, imf, cf, avg_cycle, gop, gos)



fit <- lm(gop ~ cycle, data = test_clean)
summary(fit)

fit <- lm(gos ~ cf, data = test_clean)
summary(fit)

fit <- lm(gos ~ imf, data = test_clean)
summary(fit)

fit <- lm(gos ~ avg_cycle, data = test_clean)
summary(fit)

#######

fit <- lm(gop ~ cycle, data = test_clean)
summary(fit)

fit <- lm(gop ~ cf, data = test_clean)
summary(fit)

fit <- lm(gop ~ imf, data = test_clean)
summary(fit)

fit <- lm(gop ~ avg_cycle, data = test_clean)
summary(fit)




###lag testing
test_lag <- test_clean |>
  arrange(date) |>
  mutate(
    gop_lag1 = lag(gop, 1),
    gop_lag2 = lag(gop, 2),
    gop_lag3 = lag(gop, 3)) |>
  drop_na()

fit <- lm(gop ~ avg_cycle + gop_lag1 + gop_lag2 + gop_lag3, data = test_lag)
summary(fit)



test_lag <- test_clean |>
  arrange(date) |>
  mutate(
    cycle_lag1 = lag(cycle, 1),
    cycle_lag2 = lag(cycle, 2),
    cycle_lag3 = lag(cycle, 3)) |>
  drop_na()

fit <- lm(gop ~ avg_cycle + cycle_lag1 + cycle_lag2 + cycle_lag3, data = test_lag)
summary(fit)

fit <- lm(gos ~ avg_cycle + cycle_lag1 + cycle_lag2 + cycle_lag3, data = test_lag)
summary(fit)
