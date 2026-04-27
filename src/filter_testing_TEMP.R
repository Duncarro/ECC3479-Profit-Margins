library(KFAS)
library(KFAS)
library(KFAS)

library(KFAS)
library(ggplot2)
library(zoo)

# --- 1. Data ---
y_raw <- gdp$log
y <- as.numeric(na.omit(y_raw))

# Scale for stability (IMPORTANT)
y <- as.numeric(scale(y))

# Dates
dates <- as.Date(as.yearqtr(time(y_raw)))
dates <- dates[!is.na(y_raw)]

# --- 2. Model: Trend + Cycle ---
model <- SSModel(
  y ~ SSMtrend(2, Q = list(NA, NA)) +
    SSMcycle(period = 20, Q = NA),
  H = NA
)

# --- 3. Update function (FIXED) ---
update_fn <- function(pars, model) {
  pars <- pmin(pmax(pars, -10), 10)
  
  # Observation variance
  model$H[,,1] <- exp(pars[1])
  
  # Trend variances
  model$Q[1,1,1] <- exp(pars[2])   # level
  model$Q[2,2,1] <- exp(pars[3])   # slope
  
  # Cycle variance (2 states!)
  model$Q[3:4,3:4,1] <- diag(exp(pars[4]), 2)
  
  model
}

# --- 4. Fit ---
init_vals <- log(c(0.01, 0.01, 0.01, 0.01))

fit <- fitSSM(
  model,
  inits = init_vals,
  updatefn = update_fn,
  method = "BFGS"
)

kfs <- KFS(fit$model)

# --- 5. Extract components (CORRECT) ---
trend <- kfs$alphahat[, 1]

# Proper cycle (amplitude from 2D state)
cycle <- sqrt(kfs$alphahat[,3]^2 + kfs$alphahat[,4]^2)

# --- 6. Dataframe ---
df_uc <- data.frame(
  date = dates,
  trend = trend,
  cycle = cycle
)

df_uc$cycle_pct <- 100 * df_uc$cycle

cycle <- kfs$alphahat[,3]
# --- 7. Plot ---
ggplot(df_uc, aes(x = date, y = cycle)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_line(colour = "steelblue", linewidth = 0.7) +
  labs(
    title = "GDP Output Gap",
    subtitle = "UC Model (Trend + Stochastic Cycle)",
    x = NULL,
    y = "Percent Deviation from Trend"
  ) +
  theme_classic()
































###Augmented AR(2) Model
library(KFAS)
library(ggplot2)
library(zoo)

# --- 1. Data ---
y_raw <- gdp$log
y <- as.numeric(na.omit(y_raw))

# Scale (important for stability)
y <- as.numeric(scale(y))

dates <- as.Date(as.yearqtr(time(y_raw)))
dates <- dates[!is.na(y_raw)]

# --- 2. Build AR(2) state-space matrices ---
build_ar2 <- function(phi1, phi2, sigma2) {
  list(
    T = matrix(c(phi1, phi2,
                 1,    0), 2, 2),
    R = matrix(c(1, 0), 2, 1),
    Q = matrix(sigma2)
  )
}

# --- 3. Model ---
model <- SSModel(
  y ~ SSMtrend(2, Q = list(NA, NA)) +
    SSMcustom(
      Z = matrix(c(1, 0), 1, 2),   # observe first AR state
      T = diag(2),
      R = matrix(0, 2, 1),
      Q = matrix(NA),
      P1 = diag(2)
    ),
  H = NA
)

# --- 4. Update function ---
update_fn <- function(pars, model) {
  pars <- pmin(pmax(pars, -10), 10)
  
  # Variances
  model$H[,,1] <- exp(pars[1])
  model$Q[1,1,1] <- exp(pars[2])   # level
  model$Q[2,2,1] <- exp(pars[3])   # slope
  
  sigma2_c <- exp(pars[4])
  
  # AR(2) parameters (constrained for stationarity-ish)
  phi1 <- 1.5 * tanh(pars[5])
  phi2 <- -0.75 * tanh(pars[6])
  
  ar2 <- build_ar2(phi1, phi2, sigma2_c)
  
  model$T[3:4,3:4,1] <- ar2$T
  model$R[3:4,,1]    <- ar2$R
  model$Q[3,3,1]     <- ar2$Q
  
  model
}

# --- 5. Fit ---
init_vals <- log(c(0.01, 0.01, 0.01, 0.01, 0.1, 0.1))

fit <- fitSSM(
  model,
  inits = init_vals,
  updatefn = update_fn,
  method = "BFGS"
)

kfs <- KFS(fit$model)

# --- 6. Extract components ---
trend <- kfs$alphahat[,1]
cycle <- kfs$alphahat[,3]   # AR(2) cycle (signed!)

# Convert to % deviation
cycle_pct <- 100 * cycle

df_uc <- data.frame(
  date = dates,
  cycle_pct = cycle_pct
)

# --- 7. Plot ---
ggplot(df_uc, aes(x = date, y = cycle_pct)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_line(colour = "steelblue", linewidth = 0.7) +
  labs(
    title = "GDP Output Gap",
    subtitle = "UC Model with AR(2) Cycle",
    x = NULL,
    y = "Percent Deviation from Trend"
  ) +
  theme_classic(base_size = 12)



#########
library(mFilter)

# HP filter (quarterly lambda = 1600)
hp <- hpfilter(gdp$log, freq = 1600, type = "lambda")

hp_cycle <- hp$cycle

df_compare <- data.frame(
  date = dates,
  uc_cycle = cycle,
  hp_cycle = hp_cycle
)

# Convert to %
df_compare$uc_cycle_pct <- 100 * df_compare$uc_cycle
df_compare$hp_cycle_pct <- 100 * df_compare$hp_cycle

library(ggplot2)

ggplot(df_compare, aes(x = date)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  
  geom_line(aes(y = uc_cycle_pct, colour = "UC (AR2)"), linewidth = 0.7) +
  geom_line(aes(y = hp_cycle_pct, colour = "HP Filter"), 
            linewidth = 0.7, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "UC (AR2)" = "steelblue",
    "HP Filter" = "darkorange"
  )) +
  
  labs(
    title = "Output Gap: UC Model vs HP Filter",
    subtitle = "Quarterly GDP",
    x = NULL,
    y = "Percent Deviation from Trend",
    color = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(legend.position = "top")


# CF filter (quarterly business cycle: 6–32 quarters)
cf <- cffilter(gdp$log, pl = 6, pu = 32, root = TRUE)

cf_cycle <- cf$cycle

df_compare <- data.frame(
  date = dates,
  uc = cycle,
  hp = hp_cycle,
  cf = cf_cycle
)

# Convert to %
df_compare$uc_pct <- 100 * df_compare$uc
df_compare$hp_pct <- 100 * df_compare$hp
df_compare$cf_pct <- 100 * df_compare$cf

ggplot(df_compare, aes(x = date)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  
  geom_line(aes(y = uc_pct, colour = "UC (AR2)"), linewidth = 0.7) +
  geom_line(aes(y = hp_pct, colour = "HP"), linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = cf_pct, colour = "CF"), linewidth = 0.7, linetype = "dotted") +
  
  scale_color_manual(values = c(
    "UC (AR2)" = "steelblue",
    "HP" = "darkorange",
    "CF" = "darkgreen"
  )) +
  
  labs(
    title = "Output Gap Comparison",
    subtitle = "UC vs HP vs Christiano–Fitzgerald",
    x = NULL,
    y = "Percent Deviation from Trend",
    color = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(legend.position = "top")



#ham
# Hamilton filter parameters
h <- 8   # forecast horizon
p <- 4   # number of lags

# Build lag matrix
library(dplyr)

df_ham <- data.frame(y = y) %>%
  mutate(
    y_lead = lead(y, h),
    lag1 = lag(y, 1),
    lag2 = lag(y, 2),
    lag3 = lag(y, 3),
    lag4 = lag(y, 4)
  ) %>%
  na.omit()

# Regression
fit_ham <- lm(y_lead ~ lag1 + lag2 + lag3 + lag4, data = df_ham)

# Fitted values
y_hat <- fitted(fit_ham)

# Align cycle with original time
ham_cycle <- rep(NA, length(y))
ham_cycle[(p + h + 1):length(y)] <- df_ham$y_lead - y_hat



df_compare <- data.frame(
  date = dates,
  uc = cycle,
  hp = hp_cycle,
  cf = cf_cycle,
  ham = ham_cycle
)

# Convert to %
df_compare <- df_compare %>%
  mutate(across(-date, ~ 100 * .))


ggplot(df_compare, aes(x = date)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  
  geom_line(aes(y = uc, colour = "UC (AR2)"), linewidth = 0.7) +
  geom_line(aes(y = hp, colour = "HP"), linetype = "dashed") +
  geom_line(aes(y = cf, colour = "CF"), linetype = "dotted") +
  geom_line(aes(y = ham, colour = "Hamilton"), linetype = "dotdash") +
  
  scale_color_manual(values = c(
    "UC (AR2)" = "steelblue",
    "HP" = "darkorange",
    "CF" = "darkgreen",
    "Hamilton" = "purple"
  )) +
  
  labs(
    title = "Output Gap Comparison",
    subtitle = "UC vs HP vs CF vs Hamilton",
    x = NULL,
    y = "Percent Deviation from Trend",
    color = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(legend.position = "top")











library(dplyr)
df_compare <- df_compare |> 
  mutate(date = gdp$date)

library(dplyr)

df_plot <- df_compare %>%
  mutate(
    uc = as.numeric(uc),
    hp = as.numeric(hp),
    cf = as.numeric(cf),
    ham = as.numeric(ham)
  ) %>%
  filter(!is.na(date)) %>%                     # remove bad dates
  filter(date < as.Date("2020-03-31")) %>%     # pre-COVID
  arrange(date)

library(tidyr)

df_long <- df_plot %>%
  pivot_longer(
    cols = c(uc, hp, cf, ham),
    names_to = "method",
    values_to = "value"
  )

ggplot(df_long, aes(x = date, y = value, colour = method, group = method)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  
  scale_color_manual(values = c(
    uc = "steelblue",
    hp = "darkorange",
    cf = "darkgreen",
    ham = "purple"
  ),
  labels = c(
    uc = "UC (AR2)",
    hp = "HP",
    cf = "CF",
    ham = "Hamilton"
  )) +
  
  labs(
    title = "Output Gap Comparison (Pre-COVID)",
    subtitle = "UC vs HP vs CF vs Hamilton",
    x = NULL,
    y = "Percent Deviation from Trend",
    colour = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(legend.position = "top")
