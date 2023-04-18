# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)

library(readxl)

# 2.0 Importing Files ----

# A good convention is to use the file name and suffix it with tbl for the data structure tibble
bikes_tbl      <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# Not necessary for this analysis, but for the sake of completeness
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")



# 3.0 Examining Data ----

orderlines_tbl

glimpse(orderlines_tbl)



# 4.0 Joining Data ----

#left_join(orderlines_tbl, bikes_tbl)         Funktioniert nicht

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()



bikes_tbl %>%
  select(bike.id, model, model.year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))