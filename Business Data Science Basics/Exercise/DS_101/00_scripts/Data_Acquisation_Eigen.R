
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("fs")       # Install single package
install.packages(pkgs_cran)  # Install many packages
install.packages("RSQLite")
install.packages("rstudioapi")
install.packages("purrr")
install.packages("jsonlite")
install.packages("furrr")

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(tidyverse)
library(RSQLite)
library(glue)
library(httr)
library(jsonlite)
library(rstudioapi)
library(rvest)
library(purrr)
library(xopen)     # Quickly opening URLs
library(stringi)   # character string/text processing
library(furrr)



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




url_rose <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"

html_rose <- read_html(url_rose)

title <- html_rose %>%
  html_nodes(css = ".catalog-category-bikes__title-text") %>%
  html_text()

price <- html_rose %>% 
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text() %>%
  stringr::str_replace_all(pattern = "\ab ", replacement = "") %>%
  stringr::str_replace_all(pattern = "\n", replacement = "") %>%
  stringr::str_replace_all(pattern = "\200", replacement = "") %>%
  stringr::str_replace_all(pattern = ",00", replacement = "") %>%
  stringr::str_replace_all(pattern = " ", replacement = "") %>%
  stringr::str_replace_all(pattern = "\\.", replacement = "") %>%
  readr::parse_number()

rose_tbl <- tibble(title,price)
view(rose_tbl)






#### Eigene Daten

url_tt <- "https://wirbauendeinenpc.shop/gaming/high-end/"

html_tt <- read_html(url_tt)


title <- html_tt %>%
  html_nodes(css = ".product--title") %>%
  html_text() %>%
  stringr::str_replace_all(pattern = " ", replacement = "")


price <- html_tt %>% 
  html_nodes(css = ".product--price-info") %>%
  html_text() %>%
  stringr::str_extract("(?<=ab             ).*(?=\200)" ) %>% 
  readr::parse_number()


tt_tbl <- tibble(title,price)




#NEU weil Link geändert wurde

url_ttx <- "https://wirbauendeinenpc.shop/collections/mal-schnell-einpacken"

html_ttx <- read_html(url_ttx)


titlex <- html_ttx %>%
  html_nodes(css = ".product-card__title") %>%
  html_text() %>%
  stringr::str_replace_all(pattern = " ", replacement = "")


pricex <- html_ttx %>% 
  html_nodes(css = ".price--listing") %>%
  html_text() #%>%
# stringr::str_extract("(?<=ab             ).*(?=\200)" ) %>% 
# readr::parse_number()


ttx_tbl <- tibble(titlex,pricex)
view(ttx_tbl)
