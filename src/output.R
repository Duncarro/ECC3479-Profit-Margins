#EDA OUTPUT SCRIPT

if (!exists("gop")) {
  source("src/library.R")
}

source("src/conditional_cyclicality.R")
source("src/conditional_cyclicality_regressions.R")

# TOTAL TIME SERIES PLOTS

p1 <- gop |>
  filter(industry == "Total less mining") |> 
  ggplot(aes(x = date, y = margin)) +
  geom_line(linewidth = 0.9, colour = "darkorange") +
  labs(
    title = "Gross Operating Profit Margin",
    subtitle = "All industries",
    x = NULL,
    y = "Per cent"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("output/gop_margin.png", plot = p1, width = 8, height = 5, dpi = 300)



p2<-gos |>
  filter(industry == "Total less mining") |>
  ggplot(aes(x = date, y = margin)) +
  geom_line(linewidth = 0.9, colour = "coral2") +
  labs(
    title = "Gross Operating Surplus Margin",
    subtitle = "All industries",
    x = NULL,
    y = "Per cent"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

p2

ggsave("output/gos_margin.png", plot = p2, width = 8, height = 5, dpi = 300)
library(scales)

p3 <- thw |>
  ggplot(aes(x = date, y = value / 1e6)) +
  geom_line(linewidth = 0.9, colour = "cyan3") +
  labs(
    title = "Total Hours Worked",
    x = NULL,
    y = "Hours worked (millions)"
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.1)   # 1 decimal place
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

p3

ggsave("output/thw_level.png", plot = p3, width = 8, height = 5, dpi = 300)



p4 <- gdp |>
  ggplot(aes(x = date, y = value / 1e6)) +  
  geom_line(linewidth = 0.9, colour = "darkolivegreen4") +
  labs(
    title = "Gross Domestic Product",
    x = NULL,
    y = "Millions ($)"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

p4

ggsave("output/gdp_level.png", plot = p4, width = 8, height = 5, dpi = 300)



##series highlight plots 
h1<-plot_highlight(gos, "Total")
ggsave("output/gos_highlight.png", plot = h1, width = 8, height = 5, dpi = 300) ##AXIS LABEL ISSUE + TITLE
h2<-plot_highlight(gop, "Total")
ggsave("output/gop_highlight.png", plot = h2, width = 8, height = 5, dpi = 300) 



c1 <- gos |>
  filter(industry %in% c("Total", "Total less mining", "Total less mi and ag")) |>
  ggplot(aes(x = date, y = margin, colour = industry)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Gross Operating Surplus Margin Variance",
    x = NULL,
    y = "Margin",
    colour = NULL
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("output/gos_Total_changes.png", plot = c1, width = 8, height = 5, dpi = 300) 


c2 <- gop |>
  filter(industry %in% c("Total", "Total less mining")) |>
  ggplot(aes(x = date, y = margin, colour = industry)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Gross Operating Profit Margin Variance",
    x = NULL,
    y = "Margin",
    colour = NULL
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("output/gop_Total_changes.png", plot = c2, width = 8, height = 5, dpi = 300) 


#CONDITIONAL CYCLICALITY SENSE CHECK PLOT

plot_check <- gos_resid %>%
  left_join(
    gos_wide %>% select(date, `Total less mi and ag`),
    by = "date"
  )

d1 <- plot_check %>%
  ggplot(aes(x = date)) +
  
  # Original (semi-transparent)
  geom_line(
    aes(y = `Total less mi and ag`, color = "Original"),
    linewidth = 0.6,
    alpha = 0.5
  ) +
  
  # Detrended
  geom_line(
    aes(y = `Total less mi and ag_resid`, color = "Detrended"),
    linewidth = 0.6
  ) +
  
  # Trend lines
  geom_smooth(
    aes(y = `Total less mi and ag`, color = "Original"),
    method = "lm", se = FALSE,
    linewidth = 0.7
  ) +
  geom_smooth(
    aes(y = `Total less mi and ag_resid`, color = "Detrended"),
    method = "lm", se = FALSE,
    linewidth = 0.7
  ) +
  
  scale_color_manual(values = c(
    "Original" = "darkorange",
    "Detrended" = "steelblue"
  )) +
  
  labs(
    title = "Original vs Detrended Series",
    subtitle = "GOS: Total less mining and agriculture",
    x = NULL,
    y = "Value (%)",
    color = NULL
  ) +
  
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
    
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "top"
  )

ggsave("output/gos_resid_og_comparison_TMA.png", plot = d1, width = 8, height = 5, dpi = 300) 




#### CYCLICAL CORRELATION BAR GRAPHS
#FULLSAMPLE
# benchmark business cycle series (BN filter)
cycle_series <- gos_gdp %>%
  select(date, cycle = bn)

# reshape residual series to long format
resid_long <- gos_gdp %>%
  select(date, ends_with("_resid")) %>%
  pivot_longer(
    cols = -date,
    names_to = "sector",
    values_to = "resid"
  )

# merge benchmark cycle and compute correlations
cor_results <- resid_long %>%
  left_join(cycle_series, by = "date") %>%
  group_by(sector) %>%
  summarise(
    corr = cor(resid, cycle, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(desc(corr))

# clean labels
cor_results_clean <- cor_results %>%
  mutate(
    sector = str_remove(sector, "_resid$"),
    
    sector = recode(
      sector,
      "Total" = "Total Economy",
      "Total less mining" = "Total less Mining",
      "Total less mi and ag" = "Total less Mining & Agriculture"
    )
  ) %>%
  filter(sector != "t")

# symmetric axis scaling
max_abs <- max(abs(cor_results_clean$corr), na.rm = TRUE)

# plot
cor1 <- ggplot(
  cor_results_clean,
  aes(x = reorder(sector, corr), y = corr)
) +
  
  geom_col(
    aes(fill = corr > 0),
    width = 0.7,
    show.legend = FALSE
  ) +
  
  coord_flip() +
  
  geom_hline(
    yintercept = 0,
    colour = "grey30",
    linewidth = 0.4
  ) +
  
  scale_fill_manual(values = c(
    "TRUE"  = "#2C5C85",
    "FALSE" = "#B04A4A"
  )) +
  
  scale_y_continuous(
    limits = c(-max_abs * 1.1, max_abs * 1.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Correlation with Benchmark Business Cycle",
    subtitle = "Sectoral profit margin correlations with BN-filtered GDP cycle",
    x = NULL,
    y = "Correlation coefficient",
    caption = "Source: ABS National Accounts | Author calculations"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 12,
      colour = "grey30"
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


ggsave(
  "output/Correl_full_sample.png",
  plot = cor1,
  width = 12,     # was 8 → increase to 11–13
  height = 6,     # slightly taller helps readability
  dpi = 300,
  bg = "white"
)


#pre-covid
# define cutoff
cutoff <- as.Date("2020-01-01")

# benchmark business cycle series (BN filter)
cycle_series <- gos_gdp %>%
  filter(date < cutoff) %>%
  select(date, cycle = bn)

# reshape residual series to long format
resid_long <- gos_gdp %>%
  filter(date < cutoff) %>%
  select(date, ends_with("_resid")) %>%
  pivot_longer(
    cols = -date,
    names_to = "sector",
    values_to = "resid"
  )

# merge benchmark cycle and compute correlations
cor_results <- resid_long %>%
  left_join(cycle_series, by = "date") %>%
  group_by(sector) %>%
  summarise(
    corr = cor(resid, cycle, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(desc(corr))

# clean labels
cor_results_clean <- cor_results %>%
  mutate(
    sector = str_remove(sector, "_resid$"),
    
    sector = recode(
      sector,
      "Total" = "Total Economy",
      "Total less mining" = "Total less Mining",
      "Total less mi and ag" = "Total less Mining & Agriculture"
    )
  ) %>%
  filter(sector != "t")

# symmetric axis scaling
max_abs <- max(abs(cor_results_clean$corr), na.rm = TRUE)

# plot
cor2 <- ggplot(
  cor_results_clean,
  aes(x = reorder(sector, corr), y = corr)
) +
  
  geom_col(
    aes(fill = corr > 0),
    width = 0.7,
    show.legend = FALSE
  ) +
  
  coord_flip() +
  
  geom_hline(
    yintercept = 0,
    colour = "grey30",
    linewidth = 0.4
  ) +
  
  scale_fill_manual(values = c(
    "TRUE"  = "#2C5C85",
    "FALSE" = "#B04A4A"
  )) +
  
  scale_y_continuous(
    limits = c(-max_abs * 1.1, max_abs * 1.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Correlation with Benchmark Business Cycle",
    subtitle = "Pre-COVID sectoral profit margin correlations with BN-filtered GDP cycle",
    x = NULL,
    y = "Correlation coefficient",
    caption = "Sample: 2002Q3–2019Q4 | Source: ABS National Accounts | Author calculations"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 12,
      colour = "grey30"
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "output/Correl_pre_covid.png",
  plot = cor2,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)



###########################

spec <- gos_gdp |> 
  select(date,
         margin = 'Total less mining_resid',
         cf,  bn, hp) |> 
  arrange(date)

spec_plot <- spec |>
  pivot_longer(
    cols = c(cf, hp, bn),
    names_to = "filter",
    values_to = "cycle"
  ) |>
  mutate(
    filter = factor(
      filter,
      levels = c("cf", "hp", "bn"),
      labels = c("CF Filter", "HP Filter", "BN Filter")
    )
  )

spec_p1<-spec_plot |> 
  ggplot(
    aes(date, cycle, colour = filter)
  ) +
  
  # COVID shading
  annotate(
    "rect",
    xmin = as.Date("2020-03-01"),
    xmax = as.Date("2021-12-01"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.06,
    fill = "grey70"
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "grey45"
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  scale_colour_manual(
    values = c(
      "CF Filter" = "#0072B2",   # blue
      "HP Filter" = "#D55E00",   # orange
      "BN Filter" = "#009E73"    # green
    )
  ) +
  
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    labels = label_number(accuracy = 0.1)
  ) +
  
  labs(
    title = "Comparison of Business Cycle Filters",
    subtitle = "Quarterly GDP cycle estimates",
    x = NULL,
    y = "Cycle component (%)",
    colour = NULL,
    caption = "Source: ABS National Accounts | Author calculations"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "top",
    
    legend.text = element_text(size = 11),
    
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 11,
      colour = "grey30",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    
    axis.line = element_line(
      linewidth = 0.4
    ),
    
    axis.ticks = element_line(
      linewidth = 0.4
    ),
    
    plot.caption = element_text(
      size = 9,
      colour = "grey40"
    )
  )


ggsave(
  "output/GDP_filter_comp.png",
  plot = spec_p1,
  width = 12,    
  height = 6,   
  dpi = 300
)





#######STL DECOMPS
gop_ts <- gop |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(key = industry, index = date)

gos_ts <- gos |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(key = industry, index = date)

stl1 <- gop_ts |>
  filter(industry == "Total less mining") |>
  model(STL(margin)) |>
  components() |>
  autoplot() + labs(title = "GOP - STL Decomposition")

ggsave(
  "output/stl_gop.png",
  plot = stl1,
  width = 12,    
  height = 6,   
  dpi = 300
)

stl2 <- gos_ts |>
  filter(industry == "Total less mining") |>
  model(STL(margin)) |>
  components() |>
  autoplot() + labs(title = "GOS - STL Decomposition")

ggsave(
  "output/stl_gos.png",
  plot = stl2,
  width = 12,    
  height = 6,   
  dpi = 300
)

