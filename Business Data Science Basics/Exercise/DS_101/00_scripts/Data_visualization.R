pkgs_cran <- c()

install.packages("fs")       
install.packages("pkgs_cran")
install.packages("rstudioapi")
install.packages("RSQLite")
install.packages("purrr")
install.packages("jsonlite")
install.packages("readxl")
install.packages("data.table")
install.packages("vroom")
install.packages("tictoc")
install.packages("scales") 
install.packages("lubridate") 
install.packages("ggthemes")
install.packages("maps")


library(RSQLite)
library(glue)
library(httr)
library(rstudioapi)
library(jsonlite)
library(tidyverse)
library(rvest)
library(purrr)
library(xopen)     
library(stringi)
library(furrr)
library(readxl)
library(data.table)
library(magrittr)
library(vroom)
library(tictoc)
library(scales)
library(lubridate)
library(ggthemes)
library(maps)


bike_orderlines_tbl <- read_rds("02_data_wrangling/bike_orderlines.rds")

# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----

sales_by_year_tbl <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # ??? Format Text
  mutate(sales_text = scales::dollar(sales, 
                                     big.mark     = ".", 
                                     decimal.mark = ",", 
                                     prefix       = "", 
                                     suffix       = " ???"))

sales_by_year_tbl
## # A tibble: 5 x 3
##    year    sales sales_text  
##   <dbl>    <dbl> <chr>       
## 1  2015  9930282 9.930.282 ??? 
## 2  2016 10730507 10.730.507 ???
## 3  2017 14510291 14.510.291 ???
## 4  2018 12241853 12.241.853 ???
## 5  2019 15017875 15.017.875 ???



# Step 2: Plot ----
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales))

# Without piping 
ggplot(data = sales_by_year_tbl, 
       aes(x     = year, 
           y     = sales, 
           color = sales))



sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) + 

# not what you want because 2 is not a variable
geom_point(aes(size = 2),
           
           # this is fine -- turns all points red
           color = "red")






# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity) %>%
  
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>%
  ungroup()

# Scatter Plot
order_value_tbl %>%
  
  ggplot(aes(x = total_quantity, y = total_price)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)






# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%
  
  ggplot(aes(year_month, revenue)) +
  
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)





# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()





# Histogram

bike_orderlines_tbl %>%
  
  distinct(model, price) %>%
  
  ggplot(aes(price)) +
  
  geom_histogram(bins = 25, fill = "blue", color = "white")





# Histogram
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_histogram() +
  
  facet_wrap(~ frame_material, ncol = 1)



# Density
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5) +
  # facet_wrap(~ frame_material, ncol = 1) +
  
  theme(legend.position = "bottom")





# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, model, price) %>%
  distinct() %>%
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_boxplot() +
  coord_flip()




# Violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_jitter(width = 0.15, color = "#2c3e50") +
  geom_violin(alpha = 0.5) +
  
  coord_flip()





# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "#2c3e50") +
  geom_smooth(method = "lm", se = FALSE) +
  
  geom_text(aes(label =  scales::dollar(revenue, 
                                        scale  = 1e-6, 
                                        prefix = "",
                                        suffix = "M")), 
            vjust = 1.5, color = "white") +
  
  geom_label(label =  "Major Demand This Year",
             vjust = -0.5, 
             size  = 5,
             fill  = "#1f78b4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>%
               filter(year %in% c(2019))) + 
  
  expand_limits(y = 2e7)





# Wie oben nur explizit
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # same as above, with explicit scales
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_continuous()






bike_orderlines_tbl <- read_rds("02_data_wrangling/bike_orderlines.rds")

# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----

sales_by_year_tbl <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # ??? Format Text
  mutate(sales_text = scales::dollar(sales, 
                                     big.mark     = ".", 
                                     decimal.mark = ",", 
                                     prefix       = "", 
                                     suffix       = " ???"))

sales_by_year_tbl
## # A tibble: 5 x 3
##    year    sales sales_text  
##   <dbl>    <dbl> <chr>       
## 1  2015  9930282 9.930.282 ??? 
## 2  2016 10730507 10.730.507 ???
## 3  2017 14510291 14.510.291 ???
## 4  2018 12241853 12.241.853 ???
## 5  2019 15017875 15.017.875 ???



# Step 2: Plot ----
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales))

# Without piping 
ggplot(data = sales_by_year_tbl, 
       aes(x     = year, 
           y     = sales, 
           color = sales))



sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) + 
  
  # not what you want because 2 is not a variable
  geom_point(aes(size = 2),
             
             # this is fine -- turns all points red
             color = "red")






# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity) %>%
  
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>%
  ungroup()

# Scatter Plot
order_value_tbl %>%
  
  ggplot(aes(x = total_quantity, y = total_price)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)






# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%
  
  ggplot(aes(year_month, revenue)) +
  
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)





# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()





# Histogram

bike_orderlines_tbl %>%
  
  distinct(model, price) %>%
  
  ggplot(aes(price)) +
  
  geom_histogram(bins = 25, fill = "blue", color = "white")





# Histogram
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_histogram() +
  
  facet_wrap(~ frame_material, ncol = 1)



# Density
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5) +
  # facet_wrap(~ frame_material, ncol = 1) +
  
  theme(legend.position = "bottom")





# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, model, price) %>%
  distinct() %>%
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_boxplot() +
  coord_flip()




# Violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_jitter(width = 0.15, color = "#2c3e50") +
  geom_violin(alpha = 0.5) +
  
  coord_flip()





# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "#2c3e50") +
  geom_smooth(method = "lm", se = FALSE) +
  
  geom_text(aes(label =  scales::dollar(revenue, 
                                        scale  = 1e-6, 
                                        prefix = "",
                                        suffix = "M")), 
            vjust = 1.5, color = "white") +
  
  geom_label(label =  "Major Demand This Year",
             vjust = -0.5, 
             size  = 5,
             fill  = "#1f78b4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>%
               filter(year %in% c(2019))) + 
  
  expand_limits(y = 2e7)





# Wie oben nur explizit
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # same as above, with explicit scales
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_continuous()




sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +
  
  # Formatting
  expand_limits(y = 0) +
  # You can also type "red", "black" etc. for the colors
  scale_color_continuous(low    = "#95E1EA", high = "#2097A3", 
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) +
  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )






library(ggthemes)
## DATA PREPARATION
sales_by_month_2015 <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a month column
  select(order_date, total_price) %>%
  mutate(year  = year(order_date)) %>% 
  mutate(month = month(order_date)) %>%
  
  filter(year == "2015") %>%
  
  # Grouping by month, and summarizing sales
  group_by(month) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # $ Format Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",  
                                     suffix          = " €"))

## PLOTTING
# Canvas
sales_by_month_2015 %>% 
  ggplot(aes(x = month, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # Formatting
  expand_limits(y = 0) +
  scale_color_continuous(low = "red", high = "black",
                         labels = scales::dollar_format(scale = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month, 
                     labels = month(sales_by_month_2015$month, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M")) +
  labs(
    title = "Monthly sales (2015)",
    subtitle = "April is the strongest month!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers are dropping towards the end of the year."
  )  +  
  theme_economist() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))













# Data Manipulation

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

# Uncover the factor levels (just for demonstration)
# sorted by years and the highest revenues
sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)




# Named Colors. This returns a long list of colors that can be used by name
colors()

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "slateblue")

# To RGB
col2rgb("slateblue")

col2rgb("#2C3E50")

# To HEX (this function should be provided to a geom)
rgb(44, 62, 80, maxColorValue = 255)






### Brewer. Comes with basic R.
#Primarly for discrete data.

# We can use those palletes by just calling their names (e.g. "Blues")
# Display the colors
RColorBrewer::display.brewer.all() 
# Get information
RColorBrewer::brewer.pal.info
# Get the HEX codes
RColorBrewer::brewer.pal(n = 8, name = "Blues")[1]

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])


### Viridis
viridisLite::viridis(n = 20)
# The last two characters indicate the transparency (e.g. FF makes it 100% transparent)

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = viridisLite::viridis(n = 20)[10])





sales_by_year_category_1_tbl %>%
  
  # Put the aes color mapping here, to apply it to geom_line and geom_point
  ggplot(aes(year, revenue, color = category_1)) +
  
  # Or you could do it locally in each geom 
  # (aes mapping only necessary if you map it to a column)
  geom_line(size = 1) + # geom_line(aes(color = category_1))
  geom_point(color = "dodgerblue", size = 5)




sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  geom_col(aes(fill = category_1)) 
# You could use color = ... to color the outlines




sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, size = revenue)) +
  
  # The local size overrides the global size
  geom_line(aes(color = category_1), size = 1) + 
  geom_point()




sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color = category_1)) +
  geom_line(color = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  
  # Break out stacked plot
  facet_wrap(~ category_1, ncol = 3, scales = "free_y") +
  
  expand_limits(y = 0)




sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  # geom_col(position = "stack") # default
  # geom_col(position = "dodge")
  geom_col(position = position_dodge(width = 0.9), color = "white")




# Stacked Area

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black")




g_facet_continuous <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color = revenue)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +
  
  theme_minimal()

g_facet_continuous





g_facet_discrete <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color = category_1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +
  
  theme_minimal()

g_facet_discrete




g_area_discrete <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black") +
  
  theme_minimal()

g_area_discrete




g_facet_continuous +
  
  # scale_color_continuous(
  #     low   = "black",
  #     high  = "cornflowerblue"
  # )
  # This is basically like adding a theme
  scale_color_viridis_c(option = "E", direction = -1)



RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n = 8, name = "Blues")

g_facet_discrete +
  scale_color_brewer(palette = "Set3") +
  theme_dark()

g_facet_discrete +
  scale_color_viridis_d(option = "D") +
  theme_dark()




g_area_discrete +
  scale_fill_brewer(palette = "Set3")

g_area_discrete +
  scale_fill_viridis_d()



g_facet_continuous +
  scale_x_continuous(breaks = seq(2015, 2019, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    preix = "",
                                                    suffix = "M"))




g_facet_continuous +
  
  scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    suffix = "M")) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  scale_color_viridis_c() +
  theme_dark() +
  
  labs(
    title = "Bike Sales",
    subtitle = "Sales are trending up",
    caption = "5-year sales trends\ncomes from our ERP Database",
    x = "Year",
    y = "Revenue (M €)",
    color = "Revenue" # Legend text
  )




g_facet_continuous +
  
  theme_light() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.background = element_rect(
      color = "black",
      fill  = "cornflowerblue",
      size  = 1
    ),
    strip.text = element_text(
      face  = "bold",
      color = "white"
    )
  )




sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  
  geom_area(color = "black") +
  
  # Scales
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " €")) +
  
  # Labels
  labs(
    title = "Sales Over Year by Category 1",
    subtitle = "Sales Trending Upward",
    x = "",
    y = "Revenue (M €)",
    fill = "2nd Category",
    caption = "Bike sales trends look strong heading into 2020"
  ) +
  
  # Theme
  theme_light() +
  theme(
    title = element_text(face = "bold", color = "#08306B")
    
  )
















# 1.0 Lollipop Chart: Top N Customers ----
library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")




n <- 10
# Data Manipulation
top_customers_tbl <- bike_orderlines_tbl %>%
  
  # Select relevant columns
  select(bikeshop, total_price) %>%
  
  # Collapse the least frequent values into “other”
  mutate(bikeshop = as_factor(bikeshop) %>% fct_lump(n = n, w = total_price)) %>%
  
  # Group and summarize
  group_by(bikeshop) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Reorder the column customer_city by revenue
  mutate(bikeshop = bikeshop %>% fct_reorder(revenue)) %>%
  # Place "Other" at the beginning
  mutate(bikeshop = bikeshop %>% fct_relevel("Other", after = 0)) %>%
  # Sort by this column
  arrange(desc(bikeshop)) %>%
  
  # Add Revenue Text
  mutate(revenue_text = scales::dollar(revenue, 
                                       scale  = 1e-6, 
                                       prefix = "", 
                                       suffix = "M €")) %>%
  
  # Add Cumulative Percent
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>%
  mutate(cum_pct_text = scales::percent(cum_pct)) %>%
  
  # Add Rank
  mutate(rank = row_number()) %>%
  mutate(rank = case_when(
    rank == max(rank) ~ NA_integer_,
    TRUE ~ rank
  )) %>%
  
  # Add Label text
  mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))






# Data Visualization
top_customers_tbl %>%
  
  # Canvas
  ggplot(aes(revenue, bikeshop)) +
  
  # Geometries
  geom_segment(aes(xend = 0, yend = bikeshop), 
               color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
               size  = 1) +
  
  geom_point(aes(size = revenue),
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  geom_label(aes(label = label_text), 
             hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  # Formatting
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M €")) +
  labs(
    title = str_glue("Top {n} Customers"),
    subtitle = str_glue(
      "Start: {year(min(bike_orderlines_tbl$order_date))}
               End:  {year(max(bike_orderlines_tbl$order_date))}"),
    x = "Revenue (M €)",
    y = "Customer",
    caption = str_glue("Top 6 customers contribute
                           52% of purchasing power.")
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )









# Select columns and filter categories
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain","Road")) %>% 
  
  # Group by category and summarize
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%
  
  # Add missing groups (not necessarily mandatory, but we'd get holes in the plot)
  # complete() creates NAs. We need to set those to 0.
  complete(bikeshop, nesting(category_1, category_2)) %>% 
  mutate(across(total_qty, ~replace_na(., 0))) %>%  
  
  # Group by bikeshop and calculate revenue ratio
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%
  
  # Reverse order of bikeshops
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  # Just to verify
  mutate(bikeshop_num = as.numeric(bikeshop))




# Data Visualization
pct_sales_by_customer_tbl %>%
  
  ggplot(aes(category_2, bikeshop)) +
  
  # Geometries
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)), 
            size = 3) +
  facet_wrap(~ category_1, scales = "free_x") +
  
  # Formatting
  scale_fill_gradient(low = "white", high = "#2C3E50") +
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road: 
        To be discussed ...
        
        Customers that prefer Mountain: 
        To be discussed ...")
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )




library(tidyverse)
covid_data_tbl <- read_csv("00_data/covid")




# Data Manipulation
covid_data_selected_tbl <- covid_data_tbl %>%
  
  select(dateRep, day, month, year, cases, countriesAndTerritories) %>%
  filter(countriesAndTerritories == "Germany" | countriesAndTerritories == "France" | countriesAndTerritories == "Spain"  | countriesAndTerritories == "United_States_of_America" | 
           countriesAndTerritories == "United_Kingdom" )  %>%
  filter(year == "2020") %>%
  mutate(date = lubridate::dmy(dateRep))

# 
  covid_data_selected_x_tbl <- covid_data_selected_tbl %>%
  arrange(date) %>%
group_by(countriesAndTerritories) %>%
    arrange(date) %>%
    mutate(cumsum_country = cumsum(cases)) %>%
  select(cumsum_country, countriesAndTerritories, date)

    
    
    
    
  
  # Graph Challenge 1
  covid_data_selected_x_tbl %>%
    
    ggplot(aes(x = date, y = cumsum_country, color = countriesAndTerritories)) +
    geom_line(size = 1.0) +
    labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/02/2020, Europe had more cases than the USA",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Country:"
  ) +
  theme(legend.position = "bottom") +
    
    theme_dark() +
    theme(legend.position = "bottom") +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      strip.background = element_rect(
        color = "black",
        fill  = "cornflowerblue",
        size  = 1
          )) +
    scale_x_date(date_labels = "%B", breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 months")) +
    scale_y_continuous(breaks = seq(0, 10000000, by = 2500000), labels = scales::dollar_format(scale = 1e-6, 
                                                      prefix = "",
                                                      suffix = "M")) +
    
  geom_label(aes(x = date,
                 y     = cumsum_country,
                 label = scales::comma(cumsum_country,
                                       big.mark     = ".",
                                       decimal.mark = ",")),
             data = covid_data_selected_x_tbl %>% filter(countriesAndTerritories == "United_States_of_America") %>% slice_max(cumsum_country),
             nudge_x = -35,
             size    = 4,
             fill    = "#E6AB02",
             color   = "white") 
  
  
  
  
  
# Challenge 2
  # Data Manipulation
  covid_data_selected_ch2_tbl <- covid_data_tbl %>%
    
    select(deaths, countriesAndTerritories, popData2019)
  
  # 
  covid_data_selected_ch2x_tbl <- covid_data_selected_ch2_tbl %>%
    mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
    mutate(countriesAndTerritories = case_when(
      
      countriesAndTerritories == "United Kingdom" ~ "UK",
      countriesAndTerritories == "United States of America" ~ "USA",
      countriesAndTerritories == "Czechia" ~ "Czech Republic",
      TRUE ~ countriesAndTerritories
       
    )) %>%
    group_by(countriesAndTerritories) %>%
    mutate(deaths_country = cumsum(deaths)) %>%
    ungroup()
    
    
    covid_data_selected_ch2xx_tbl <- covid_data_selected_ch2x_tbl %>%
    select(countriesAndTerritories, deaths_country) %>%
  group_by(countriesAndTerritories) %>%
  summarise(
    deaths_country   = max(deaths_country)
    )
  
  
  

  
  
  
  
  covid_data_selected_ch3_tbl <- covid_data_selected_ch2_tbl %>%
    mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
    mutate(countriesAndTerritories = case_when(
      
      countriesAndTerritories == "United Kingdom" ~ "UK",
      countriesAndTerritories == "United States of America" ~ "USA",
      countriesAndTerritories == "Czechia" ~ "Czech Republic",
      TRUE ~ countriesAndTerritories
    )) %>%
    select(countriesAndTerritories, popData2019) %>%
  unique()
  
  
  map_covid_tbl <- covid_data_selected_ch2xx_tbl %>%
    right_join(covid_data_selected_ch3_tbl, by = "countriesAndTerritories") %>%
  mutate(percentage_death_rate = (deaths_country / popData2019)) %>%
  rename(region = countriesAndTerritories) %>%
  mutate(Mortality_Rate = percentage_death_rate * 100)

  
  world <- map_data("world")  

    
     plain <- theme(
       axis.text = element_blank(),
       axis.line = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       axis.title = element_blank(),
       panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                       colour = "gray"),
       panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                       colour = "gray"),
       panel.background = element_rect(fill = "#282a36"),
       plot.title = element_text(hjust = 0.5)
     )  
    
    
  map_covid_final_tbl <- world %>%
    right_join(map_covid_tbl, by = "region") %>%
   select(long, lat, group, region, Mortality_Rate) 

    

    map_covid_final_tbl %>% 
      ggplot(aes(map_id = region)) +
      geom_map(aes(fill = Mortality_Rate), map = world, color = "grey", size = 0.1) +
      expand_limits(x = map_covid_final_tbl$long, y = map_covid_final_tbl$lat) +
      scale_fill_continuous(limits = c(0, 0.15)) +
      scale_fill_gradient( low = "red",
                                  high = "black")+
  
      plain
