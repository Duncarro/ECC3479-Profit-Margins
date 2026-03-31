#This file imports the necessary series and tidies them
#ALL SERIES ARE SEASONALLY ADJUSTED

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
  select(date, industry, value)

gop_wide <- gop %>%
  pivot_wider(
    names_from = industry,
    values_from = value
  ) |> 
  rename(Total = ` Total (Industry)`)

########### SALES DATA
sales <- read_abs("5676.0")

sales <- sales |> 
  filter(table_no == "5676006", series_type == "Seasonally Adjusted") |> 
  select(date, series, value)

sales_wide <- sales |> 
  pivot_wider(
    names_from = series,
    values_from = value
  ) |> 
  filter(date > as.Date("2002-09-01")) 


sales_wide <- sales_wide |> 
  mutate(
    total = rowSums(across(-date), na.rm = TRUE),
    total_ex_mining = rowSums(
      across(-c(
        date,
        `Sales ;  Total (State) ;  Mining ;  Current Price ;  TOTAL (SCP_SCOPE) ;`
      )),
      na.rm = TRUE
    )) #|> select(date, total, total_ex_mining)


##MERGING GOP AND SALES TO MAKE MARGIN
#GOP MARGIN = GOP / SALES

gop <- left_join(gop_wide, sales_wide,
          by = "date") |> 
  select(date, gop = "Total", sales = "total") |>  
  mutate(margin = gop / sales*100) |> 
  filter(date > as.Date("2002-09-01")) 




##IMPORTING GOS AND GVA
gos <- read_abs("5206.0")

 gos <- gos |> 
  filter(series == "All sectors ;  Gross operating surplus ;",
         series_type == "Seasonally Adjusted") |> 
   select(date, gos = value) |> 
   filter(date > as.Date("2002-09-01"))
 

 ##GVA
 gva <- read_abs("5206.0")
 
 gva <- gva |> 
   filter(series == "Total all industries ;  Gross value added at basic prices ;",
          series_type == "Seasonally Adjusted") |> 
   select(date, gva = value) |> 
   filter(date > as.Date("2002-09-01"))
 
 
 ##MERGING GOS AND GVA TO MAKE MARGIN
 #GOP MARGIN = GOS / GVA
 
gos <- left_join(gos, gva,
                  by = "date") |> 
   select(date, gos, gva) |> 
   mutate(margin = gos / gva*100) |>  
   filter(date > as.Date("2002-09-01")) 
  

#############################################
# Business Cycle Indictators                #
#############################################
  
##THW
thw <- read_abs("6202.0")

thw <- thw |> 
  filter(series_id == "A84426298K", date > as.Date("2002-09-01")) |> 
  select(date, value) |>  
  mutate(log = log(value))


##GDP (chain volume)
gdp <- read_abs("5206.0")

gdp <- gdp |> 
  filter(series_id == "A2304402X", date > as.Date("2002-09-01")) |> 
  select(date, value) |> 
  mutate(log = log(value))
  

###EXPORTING CLEANED DATA
write_csv(gop, "data/clean/gop.csv")
write_csv(gos, "data/clean/gos.csv")
write_csv(thw, "data/clean/thw.csv")
write_csv(gdp, "data/clean/gdp.csv")
