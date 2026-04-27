#EDA OUTPUT SCRIPT

if (!exists("gop")) {
  source("src/library.R")
}

source("src/conditional_cyclicality.R")

# TOTAL TIME SERIES PLOTS

p1 <- gop |>
  filter(industry == "Total") |> 
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
  filter(industry == "Total") |>
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

ggsave("output/gdp_level.png", plot = p2, width = 8, height = 5, dpi = 300)



##series highlight plots 
h1<-plot_highlight(gos, "Total")
ggsave("output/gos_highlight.png", plot = h1, width = 8, height = 5, dpi = 300) ##AXIS LABEL ISSUE + TITLE
h2<-plot_highlight(gop, "Total")
ggsave("output/gop_highlight.png", plot = h2, width = 8, height = 5, dpi = 300) ##AXIS LABEL ISSUE + TITLEE



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

ggsave("output/gos_Total_changes.png", plot = c1, width = 8, height = 5, dpi = 300) ##AXIS LABEL ISSUE + TITLEE


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

ggsave("output/gop_Total_changes.png", plot = c2, width = 8, height = 5, dpi = 300) ##AXIS LABEL ISSUE + TITLEE


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

ggsave("output/gos_resid_og_comparison_TMA.png", plot = d1, width = 8, height = 5, dpi = 300) ##AXIS LABEL ISSUE + TITLEE


