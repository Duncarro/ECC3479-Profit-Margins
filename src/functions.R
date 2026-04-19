# FUNCTIONS CODE

#simple descritive stats function
desc_stats_ts <- function(data, value_col, group_col = NULL) {
  
  value_col <- rlang::ensym(value_col)
  
  if (!is.null(group_col)) {
    group_col <- rlang::ensym(group_col)
    
    data %>%
      dplyr::group_by(!!group_col) %>%
      dplyr::summarise(
        mean   = mean(!!value_col, na.rm = TRUE),
        sd     = sd(!!value_col, na.rm = TRUE),
        min    = min(!!value_col, na.rm = TRUE),
        p25    = quantile(!!value_col, 0.25, na.rm = TRUE),
        median = median(!!value_col, na.rm = TRUE),
        p75    = quantile(!!value_col, 0.75, na.rm = TRUE),
        max    = max(!!value_col, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    
    data %>%
      dplyr::summarise(
        mean   = mean(!!value_col, na.rm = TRUE),
        sd     = sd(!!value_col, na.rm = TRUE),
        min    = min(!!value_col, na.rm = TRUE),
        p25    = quantile(!!value_col, 0.25, na.rm = TRUE),
        median = median(!!value_col, na.rm = TRUE),
        p75    = quantile(!!value_col, 0.75, na.rm = TRUE),
        max    = max(!!value_col, na.rm = TRUE)
      )
  }
}


#highlight industry plot
plot_highlight <- function(data, highlight = "Total") {
  
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(ggrepel)
  
  # Ensure Date format
  if (!inherits(data$date, "Date")) {
    data <- data %>% mutate(date = as.Date(date))
  }
  
  # Data for label (last available point of highlighted series)
  label_data <- data %>%
    filter(industry == highlight) %>%
    arrange(date) %>%
    slice_tail(n = 1)
  
  ggplot(data, aes(x = date, y = margin, group = industry)) +
    
    # Background industries
    geom_line(
      data = subset(data, industry != highlight),
      aes(colour = industry),
      linewidth = 0.7,
      alpha = 0.3
    ) +
    
    # Highlighted series
    geom_line(
      data = subset(data, industry == highlight),
      colour = "coral2",
      linewidth = 1.2
    ) +
    
    # Floating label
    geom_text_repel(
      data = label_data,
      aes(label = highlight),
      colour = "coral2",
      size = 4,
      fontface = "bold",
      nudge_x = -120,     # push LEFT (in days)
      direction = "y",
      hjust = 1,          # right-align text
      segment.alpha = 0,  # clean (no line)
      box.padding = 0.3
    ) +
    
    # Palette for background
    scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.85, guide = "none") +
    
    labs(
      title = "Gross Operating Profit Margins",
      subtitle = "Per cent, by industry",
      x = NULL,
      y = "Per cent"
    ) +
    
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))    
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
}
