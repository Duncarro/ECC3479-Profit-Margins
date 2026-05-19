uncon_spec <- gos_gdp |> 
  select(date, 
    margin = `Total less mining_resid`,
    bn, hp, cf
  ) |> arrange(date)


margin_bn_plot1 <- uncon_spec |>
  pivot_longer(
    cols = c(margin, bn),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = factor(
      series,
      levels = c("margin", "bn"),
      labels = c(
        "GOS Margin",
        "BN Cycle"
      )
    )
  )

gos_gdpp1 < -margin_bn_plot |>
  ggplot(
    aes(date, value, colour = series)
  ) +
  
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
    linewidth = 0.9
  ) +
  
  scale_colour_manual(
    values = c(
      "GOS Margin" = "#1A1A1A",   # charcoal black
      "BN Cycle"   = "#3B5B92"    # muted professional blue
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
    title = "Unconditional Cyclicality of GOS Margin and GDP",
    subtitle = "Comparison of GOS and GDP Cyclical Compoents",
    x = NULL,
    y = "Percentage Deviation",
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
  "output/Unconditional_cyclicality_gos.png",
  plot = gos_gdpp1,
  width = 12,    
  height = 6,   
  dpi = 300
)


uncon_spec <- gos_gdp |> 
  select(date, 
         margin = `Total less mining_resid`,
         bn, hp, cf
  ) |> arrange(date)


margin_bn_plot1 <- uncon_spec |>
  pivot_longer(
    cols = c(margin, bn),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = factor(
      series,
      levels = c("margin", "bn"),
      labels = c(
        "GOS Margin",
        "BN Cycle"
      )
    )
  )

gos_gdpp1 < -margin_bn_plot |>
  ggplot(
    aes(date, value, colour = series)
  ) +
  
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
    linewidth = 0.9
  ) +
  
  scale_colour_manual(
    values = c(
      "GOS Margin" = "#1A1A1A",   # charcoal black
      "BN Cycle"   = "#3B5B92"    # muted professional blue
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
    title = "Unconditional Cyclicality of GOS Margin and GDP",
    subtitle = "Comparison of GOS and GDP Cyclical Compoents",
    x = NULL,
    y = "Percentage Deviation",
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
  "output/Unconditional_cyclicality_gos.png",
  plot = gos_gdpp1,
  width = 12,    
  height = 6,   
  dpi = 300
)

