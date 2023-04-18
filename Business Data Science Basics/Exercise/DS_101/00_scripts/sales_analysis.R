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



# 5.0 Wrangling Data ----

bike_orderlines_joined_tbl$category

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))




# 6.0 Business Insights ----
# 6.1 Sales by Year ----

library(lubridate)
# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(order_date, total_price) %>%
  
  # Add year column
  mutate(year = year(order_date)) %>%
  
  # Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_tbl
## # A tibble: 5 x 3
##    year    sales sales_text  
##   <dbl>    <dbl> <chr>       
## 1  2015  9930282 9.930.282 € 
## 2  2016 10730507 10.730.507 €
## 3  2017 14510291 14.510.291 €
## 4  2018 12241853 12.241.853 €
## 5  2019 15017875 15.017.875 €


# Step 2 - Visualize
sales_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )


# 6.2 Sales by Year and Category  ----

# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " ???"))

sales_by_year_cat_1_tbl  
## # A tibble: 25 x 4
##     year category_1      sales sales_text 
##    <dbl> <chr>           <dbl> <chr>      
##  1  2015 E-Bikes       1599048 1.599.048 €
##  2  2015 Gravel         663025 663.025 €  
##  3  2015 Hybrid / City  502512 502.512 €  
##  4  2015 Mountain      3254289 3.254.289 €
##  5  2015 Road          3911408 3.911.408 €
##  6  2016 E-Bikes       1916469 1.916.469 €
##  7  2016 Gravel         768794 768.794 €  
##  8  2016 Hybrid / City  512346 512.346 €  
##  9  2016 Mountain      3288733 3.288.733 €
## 10  2016 Road          4244165 4.244.165 €
## # … with 15 more rows


# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )



# # 7.0 Writing Files ----
# 
# # 7.1 Excel ----
# install.packages("writexl")
# library("writexl")
# bike_orderlines_wrangled_tbl %>%
#   write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 
# # 7.2 CSV ----
# bike_orderlines_wrangled_tbl %>% 
#   write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 
# # 7.3 RDS ----
# bike_orderlines_wrangled_tbl %>% 
#   write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
# 


