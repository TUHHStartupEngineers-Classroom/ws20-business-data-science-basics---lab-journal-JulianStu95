# data science at NIT -----------------------------------------------------
# installation of packages

# CRAN Packages ----
pkgs_cran <- c(
    # File System
    "fs",         # working with the file system

    # Import
    "readxl",     # reading excel files
    "writexl",    # saving data as excel files

    # Tidy, Transform, & Visualize
    "tidyverse",  # dplyr, ggplot2, tibble, tidyr, readr, purrr, stringr, forcats
    "lubridate",  # working with dates and times

    # Other
    "devtools"    # used to install non-CRAN packages
)

install.packages("fs")       # Install single package
install.packages(pkgs_cran)  # Install many packages

library("tidyverse")

tibble(
    x = 1:50,
    y = runif(50), 
    z = x + y^2,
    outcome = rnorm(50)
)


class(cars)
## "data.frame"

cars_tbl <- as_tibble(cars)
class(cars_tbl)
## "tbl_df"     "tbl"        "data.frame"

# This way applies to dataframes and tibbles
vehicles <- as_tibble(cars[1:5,])
vehicles[['speed']]
vehicles[[1]]
vehicles$speed

# Using placeholders with the pipe
vehicles %>% .$dist
vehicles %>% .[['dist']]
vehicles %>% .[[2]]




library("tidyverse")
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)





diamonds2 %>% head(n = 5)
## # A tibble: 5 x 3
##   cut     `2008` `2009`
##   <chr>    <dbl>  <dbl>
## 1 Ideal      326    332
## 2 Premium    326    332
## 3 Good       237    333
## 4 Premium    334    340
## 5 Good       335    341


#diamonds2 %>% 
#    pivot_longer(cols      = c("2008", "2009"), 
#                 names_to  = 'year', 
#                 values_to = 'price') %>% 
#    head(n = 5)

#⁠#⁠ # A tibble: 5 x 3
#⁠#⁠   cut     year  price
#⁠#⁠   <chr>   <chr> <dbl>
#⁠#⁠ 1 Ideal   2008    326
#⁠#⁠ 2 Ideal   2009    332
#⁠#⁠ 3 Premium 2008    326
#⁠#⁠ 4 Premium 2009    332
#⁠#⁠ 5 Good    2008    237


#Diamonds2 und Diamonds3 mit dem Fenster unten rechts mit reload und dann sind die Daten geladen. Nicht nötig nochmal readRDS zu benutzen""


diamonds3 %>% head(n = 5)
## # A tibble: 5 x 5
##   cut     price clarity dimension measurement
##   <ord>   <dbl> <ord>   <chr>           <dbl>
## 1 Ideal     326 SI2     x                3.95
## 2 Premium   326 SI1     x                3.89
## 3 Good      327 VS1     x                4.05
## 4 Ideal     326 SI2     y                3.98
## 5 Premium   326 SI1     y                3.84



#diamonds3 %>% 
#    pivot_wider(cols      = c("2008", "2009"), 
#                 names_to  = 'year', 
#                 values_to = 'price') %>% 
#    head(n = 5)

