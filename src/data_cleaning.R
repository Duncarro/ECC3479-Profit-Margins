#This file imports the necessary series and tidies them

source("src/library.R")

#############################################
# Profit Margin Measures                    #
#############################################

gop <- read_abs("5676.0")

gop <- gop |>
  filter(table_no == "56760011", series_type == "Seasonally Adjusted") |>
  separate(
    series,
    into = c("drop1", "drop2", "industry", "drop3", "drop4", "drop5"),
    sep = " ; "
  ) |>
  mutate(industry = str_trim(industry)) |>   # <- removes leading space
  select(date, industry, value)

gop_wide <- gop %>%
  pivot_wider(
    names_from = industry,
    values_from = value
  ) |> 
  rename(Total = "Total (Industry)") |> 
  mutate("Total less mining" = Total - Mining)


#SALES
sales <- read_abs("5676.0")

sales <- sales |> 
  filter(table_no == "5676006", series_type == "Seasonally Adjusted") |> 
  select(date, series, value)

sales_wide <- sales |> 
  pivot_wider(
    names_from = series,
    values_from = value
  ) |> 
  filter(date > as.Date("2002-06-01")) 

sales_wide <- sales_wide |> 
  rename_with(~ str_trim(str_split(.x, " ; ") %>% sapply(`[`, 3)), -date)


#adding total variable
sales_wide <- sales_wide |> 
  mutate(
    Total = rowSums(across(-date), na.rm = TRUE),
    "Total less mining" = Total - Mining)


##MERGING GOP AND SALES TO MAKE MARGIN
#GOP MARGIN = GOP / SALES

gop_margin <- left_join(gop_wide, sales_wide, by = "date", suffix = c("_gop", "_sales")) |>
  filter(date > as.Date("2002-06-01")) |>
  pivot_longer(-date, names_to = c("industry", ".value"), names_sep = "_") |>
  mutate(margin = (gop / sales) * 100) |> 
  select(date, industry, value = margin)



##IMPORTING GOS AND GVA
gos <- read_abs("5206.0")

gos <- gos |> 
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

gos <- gos |>
  filter(
    grepl("Gross operating surplus", series),
    series_type == "Seasonally Adjusted"
  ) |>
  mutate(
    industry = str_trim(str_split(series, " ; ") %>% sapply(`[`, 1)),
    industry = str_remove(industry, " \\(.*\\)")   # removes (A), (B), etc.
  ) |>
  filter(industry %in% industry_list) |>
  select(date, industry, gos = value) #renaming value_gos


#making 'Total less' variables
gos <- gos |> 
  pivot_wider(names_from = industry, values_from = gos) |> 
  mutate(
    `Total less mining` = `Total all industries` - Mining,
    `Total less mi and ag` = `Total all industries` - Mining - `Agriculture, forestry and fishing`
  )

#returning to long format
gos <- gos %>%
  pivot_longer(
    cols = -date,
    names_to = "industry",
    values_to = "gos"
  )

##GVA
 gva <- read_abs("5206.0")
 
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
 
 gva <- gva |>
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
   select(date, industry, gva = value)
 
#making 'Total less' variables
gva <- gva |> 
   pivot_wider(names_from = industry, values_from = gva) |> 
   mutate(
     `Total less mining` = `Total all industries` - Mining,
     `Total less mi and ag` = `Total all industries` - Mining - `Agriculture, forestry and fishing`
   )
 

#returning to long format
gva <- gva %>%
  pivot_longer(
    cols = -date,
    names_to = "industry",
    values_to = "gva"
  )

##MERGING GOS AND GVA TO MAKE MARGIN
#GOP MARGIN = GOS / GVA             

gos_margin <- gva |> 
   inner_join(gos, by = c("date", "industry")) |> 
   mutate(margin = (gos / gva)*100,
          industry = str_replace(industry, "Total all industries", "Total"))

gos_margin |>  filter(industry == "Total")

#############################################
# Business Cycle Indictators                #
#############################################
  
##THW
thw <- read_abs("6202.0")

thw <- thw |> 
  filter(series_id == "A84426298K", date > as.Date("2002-06-01")) |> 
  select(date, value) |>  
  mutate(log = log(value))

##GDP
gdp <- read_abs("5206.0")

gdp <- gdp |> 
  filter(series_id == "A2304402X", date > as.Date("2002-06-01")) |> 
  select(date, value) |> 
  mutate(log = log(value))
  




# Wide formatting for export

gop_wide_margin <- gop_margin %>%
  select(date, industry, margin) %>%
  pivot_wider(
    names_from = industry,
    values_from = margin
  )


gos_wide_margin <- gos_margin %>%
  select(date, industry, margin) %>%
  pivot_wider(
    names_from = industry,
    values_from = margin
  )



###EXPORTING CLEANED DATA
write_csv(gop_wide_margin, "data/clean/gop_wide.csv")
write_csv(gos_wide_margin, "data/clean/gos_wide.csv")
write_csv(gop_margin, "data/clean/gop.csv")
write_csv(gos_margin, "data/clean/gos.csv")
write_csv(thw, "data/clean/thw.csv")
write_csv(gdp, "data/clean/gdp.csv")








####################################
gva_margin |>
  ggplot(aes(x = date, y = margin, colour = industry)) +
  geom_line() +
  guides(colour = "none")

gop_margin_ts <- gop_margin |>
  as_tsibble(index = date, key = industry)

gop_margin_ts <- gop_margin |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(index = date, key = industry) |>
  fill_gaps()

gop_margin_ts |>
  gg_subseries(margin) +
  theme(legend.position = "none")

gop_margin_ts |>
  filter(industry == "Total") |>
  model(STL(margin)) |>
  components() |>
  autoplot()


library(tsibble)
library(feasts)
library(fable)
library(dplyr)

gva_margin_ts <- gva_margin |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(index = date, key = industry) |>
  fill_gaps()


gva_margin_ts |>
  filter(industry == "Total all industries
") |>
  count()

gva_margin_ts |> 
  filter(industry == "Total all industries")

gva_margin_ts |>
  filter(industry == "Total all industries") |> 
  gg_season(margin)
