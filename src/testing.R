#ROBUSTNESS CHECKS

#APPROXIMATE TOTAL IS CLOSELY EQUIVALENT TO ABS REPORTED TOTAL
gop_wide <- gop %>%
  pivot_wider(
    names_from = industry,
    values_from = value
  ) |> 
  rename(Total = `Total (Industry)`) |> 
  mutate(total = rowSums(across(-c(date, Total)), na.rm = TRUE),
         total_ex_mining = Total - Mining,
         diff = Total - total
  )

gop_wide |> 
  ggplot(aes(x=date,y=diff)) + geom_line()



##############
test <- gos |> 
  filter(series_id %in% c(
    "A85231868W",
    "A85231872L",
    "A85231876W",
    "A85231880L",
    "A85231884W",
    "A85231888F",
    "A85231892W",
    "A85231896F",
    "A85231900K",
    "A85231904V",
    "A85231908C",
    "A85231912V",
    "A85231916C",
    "A85231920V",
    "A85231924C",
    "A85231928L",
    "A85231932C",
    "A85231936L",
    "A85231940C",
    "A85231943K",
    "A85231947V"
  ))

library(dplyr)
library(stringr)

industry_list <- c(
  "Agriculture, forestry and fishing",
  "Mining",
  "Manufacturing",
  "Electricity, gas, water and waste services",
  "Construction",
  "Wholesale trade",
  "Retail trade",
  "Accommodation and food services",
  "Transport, postal and warehousing",
  "Information media and telecommunications",
  "Financial and insurance services",
  "Rental, hiring and real estate services",
  "Professional, scientific and technical services",
  "Administrative and support services",
  "Public administration and safety",
  "Education and training",
  "Health care and social assistance",
  "Arts and recreation services",
  "Other services",
  "Ownership of dwellings",
  "Total all industries"
)

gos_clean <- gos |>
  filter(
    grepl("Gross operating surplus", series),
    series_type == "Seasonally Adjusted"
  ) |>
  mutate(
    industry = str_trim(str_split(series, " ; ") %>% sapply(`[`, 1)),
    industry = str_remove(industry, " \\(.*\\)")   # removes (A), (B), etc.
  ) |>
  filter(industry %in% industry_list) |>
  select(date, industry, value)




#####GVA  

series_ids <- c(
  "A85231870J",
  "A85231874T",
  "A85231878A",
  "A85231882T",
  "A85231886A",
  "A85231890T",
  "A85231894A",
  "A85231898K",
  "A85231902R",
  "A85231906X",
  "A85231910R",
  "A85231914X",
  "A85231918J",
  "A85231922X",
  "A85231926J",
  "A85231930X",
  "A85231934J",
  "A85231938T",
  "A85231942J",
  "A85231945R",
  "A85231949X"
)

gva <- gva |>
  filter(series_id %in% series_ids)


industries <- c(
  "Agriculture, forestry and fishing",
  "Mining",
  "Manufacturing",
  "Electricity, gas, water and waste services",
  "Construction",
  "Wholesale trade",
  "Retail trade",
  "Accommodation and food services",
  "Transport, postal and warehousing",
  "Information media and telecommunications",
  "Financial and insurance services",
  "Rental, hiring and real estate services",
  "Professional, scientific and technical services",
  "Administrative and support services",
  "Public administration and safety",
  "Education and training",
  "Health care and social assistance",
  "Arts and recreation services",
  "Other services",
  "Ownership of dwellings",
  "Total all industries"
)


gva_clean <- gva |>
  filter(series_id %in% series_ids) |>
  filter(
    grepl("Gross value added", series),
    series_type == "Seasonally Adjusted"
  ) |>
  mutate(
    industry = str_trim(str_split(series, " ; ") %>% sapply(`[`, 1)),
    industry = str_remove(industry, " \\(.*\\)")
  ) |>
  filter(industry %in% industries) |>
  select(date, industry, value)


gva_margin <- gos |>
  left_join(gva, by = c("date", "industry"), suffix = c("_gos", "_gva")) |>
  mutate(margin = (value_gos / value_gva) * 100) |>
  select(date, industry, margin)

gva_margin_wide <- gva_margin |>
  pivot_wider(names_from = industry, values_from = margin)

gva_margin_wide <- gva_margin_wide |>
  rename(Total = `Total all industries`)

gva_margin_wide |>
  ggplot(aes(x = date, y = Total)) +
  geom_line()

str(gva_margin_wide)


gva_margin_wide |>
  ggplot(aes(x = date, y = Total)) +
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


######TOTAL NEW SUM IS OFF FROM OLD VERSION!!!


gva_wide <- gva |>
  pivot_wider(
    names_from = industry,
    values_from = value_gva
  )|> 
  select(date, Total_gos = "Total all industries" #wrong name
  )

gos_wide <- gos |> 
  pivot_wider(
    names_from = industry,
    values_from = value_gos
  ) |> 
  select(date, Total ="Total all industries"
)


test <- left_join(gos_wide, gva_wide,
          by = "date") |> 
  select(date, Total, Total_gos) |> 
  mutate(margin = Total / Total_gos*100) |>  
  filter(date > as.Date("2002-09-01")) 

test |>
  ggplot(aes(x=date,y=margin)) + geom_line()
 
 
  
