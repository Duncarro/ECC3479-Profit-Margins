#EDA SCRIPT

source("src/library.R")

#Time series
gop_margin |>
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

ggsave("output/gos_margin.png", plot = p2, width = 8, height = 5, dpi = 300)




p2<-thw |>
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 0.9, colour = "cyan3") +
  labs(
    title = "Total Hours Worked",
    x = NULL,
    y = "UNITTTTTTTT"
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

ggsave("output/thw_level.png", plot = p2, width = 8, height = 5, dpi = 300)




p2<-gdp |>
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 0.9, colour = "darkolivegreen1") +
  labs(
    title = "Gross Domestic Product",
    x = NULL,
    y = "Millions ($)"
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

ggsave("output/gdp_level.png", plot = p2, width = 8, height = 5, dpi = 300)
