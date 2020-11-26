## Data Acquisition
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
## ----

library("tidyverse")

install.packages("RSQLite")

library(RSQLite)

con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "00_data/02_chinook/Chinook_Sqlite.sqlite")

dbListTables(con)
##  [1] "Album"         "Artist"        "Customer"      "Employee"     
##  [5] "Genre"         "Invoice"       "InvoiceLine"   "MediaType"    
##  [9] "Playlist"      "PlaylistTrack" "Track"


tbl(con, "Album")
## # Source:   table [?? x 3]
## # Database: sqlite 3.30.1 [~/Chinook_Sqlite.sqlite]
##    AlbumId Title                                 ArtistId
##      <int> <chr>                                    <int>
##  1       1 For Those About To Rock We Salute You        1
##  2       2 Balls to the Wall                            2
##  3       3 Restless and Wild                            2
##  4       4 Let There Be Rock                            1
##  5       5 Big Ones                                     3
##  6       6 Jagged Little Pill                           4
##  7       7 Facelift                                     5
##  8       8 Warner 25 Anos                               6
##  9       9 Plays Metallica By Four Cellos               7
## 10      10 Audioslave                                   8
## # … with more rows

album_tbl <- tbl(con, "Album") %>% collect()

dbGetQuery(con, 'SELECT * FROM Artist')

dbDisconnect(con)
con
## <SQLiteConnection>
##   DISCONNECTED






library(glue)
name <- "Fred"
glue('My name is {name}.')
## My name is Fred.

library(httr)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp
## Response [https://swapi.dev/api/people/1/]
##   Date: 2020-06-22 11:54
##   Status: 200
##   Content-Type: application/json
##   Size: 637 B




rawToChar(resp$content)
## "{\"name\":\"Luke Skywalker\",\"height\":\"172\",\"mass\":\"77\",
## \"hair_color\":\"blond\",\"skin_color\":\"fair\",\"eye_color\":
## \"blue\",\"birth_year\":\"19BBY\",\"gender\":\"male\",\"homeworld\":
## \"http://swapi.dev/api/planets/1/\",\"films\":
## [\"http://swapi.dev/api/films/1/\",\"http://swapi.dev/api/films/2/\",
## \"http://swapi.dev/api/films/3/\",\"http://swapi.dev/api/films/6/\"],
## \"species\":[],\"vehicles\":[\"http://swapi.dev/api/vehicles/14/\",
## \"http://swapi.dev/api/vehicles/30/\"],\"starships\":
## [\"http://swapi.dev/api/starships/12/\",\"http://swapi.dev/api/
## starships/22/\"],\"created\":\"2014-12-09T13:50:
## 51.644000Z\",\"edited\":\"2014-12-20T21:17:56.891000Z\",\"url\":
## \"http://swapi.dev/api/people/1/\"}"






data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)




library(jsonlite)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()
## $name
## "Luke Skywalker"
## 
## $height
## "172"
## ...





resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp
## Response [https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.de]
##   Date: 2020-06-22 14:31
##   Status: 200
##   Content-Type: application/json
##   Size: 214 B
## {
##     "Error Message": "the parameter apikey is invalid or missing.




token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response
## Response [https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey="my_individual_token"]
##   Date: 2020-06-23 09:04
##   Status: 200
##   Content-Type: application/json
##   Size: 382 B
## {
##     "Global Quote": {
##         "01. symbol": "WDI.DE",
##         "02. open": "14.2500",
##         "03. high": "17.6000",
##         "04. low": "14.2180",
##         "05. price": "16.7080",
##         "06. volume": "7040228",
##         "07. latest trading day": "2020-06-23",
##         "08. previous close": "14.4400",
## ...

install.packages("rstudioapi")

alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"



library("rstudioapi")
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = askForPassword("token"))
)




# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()



url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()


rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()

title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

year <- html %>% 
  html_nodes(".titleColumn .secondaryInfo") %>%
  html_text() %>% 
  # Extract numbers
  stringr::str_extract(pattern = "[0-9]+") %>% 
  as.numeric()

people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")

rating <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_text() %>% 
  as.numeric()

num_ratings <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_attr('title') %>% 
  # Extract the numbers and remove the comma to make it numeric values
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()

imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)





install.packages("purrr")
library(purrr)

# purr functional programming approach
numbers_list <- map(numbers, print)
## 1
## 2
## 3
## 4
## 5




install.packages("jsonlite")
library(jsonlite)

# bike_data_lst <- fromJSON("bike_data.json")      Weg laut Website, darunter eigener Weg

bike_data_lst <- fromJSON("https://raw.githubusercontent.com/TUHHStartupEngineers/dat_sci_ss20/master/03/bike_data.json")

# Open the data by clicking on it in the environment or by running View()
View(bike_data_lst)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing




# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.rosebikes.de/"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".catalog-category-bikes_title-text") %>%
  # ...and extract the information of the id attribute
  html_attr('id') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"SALE")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

bike_family_tbl
## # A tibble: 5 x 3
##   position family_class                  family_id                     
##                                                        
##         1 js-navigationList-ROAD        #js-navigationList-ROAD       
##         2 js-navigationList-MOUNTAIN    #js-navigationList-MOUNTAIN   
##         3 js-navigationList-EBIKES      #js-navigationList-EBIKES     
##         4 js-navigationList-HYBRID-CITY #js-navigationList-HYBRID-CITY
##         5 js-navigationList-YOUNGHEROES #js-navigationList-YOUNGHEROES

# The updated page has now also ids for CFR and GRAVEL. You can either include or remove them.




# 1.2 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
family_id_css
## "#js-navigationList-ROAD, #js-navigationList-MOUNTAIN, #js-navigationList-EBIKES, #js-navigationList-HYBRID-CITY, #js-navigationList-YOUNGHEROES"

# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

bike_category_tbl
## # A tibble: 25 x 1
##    url                                                                    
##                                                                     
##  1 https://www.canyon.com/en-de/road-bikes/race-bikes/aeroad/             
##  2 https://www.canyon.com/en-de/road-bikes/endurance-bikes/endurace/      
##  3 https://www.canyon.com/en-de/e-bikes/e-road-bikes/endurace-on/         
##  4 https://www.canyon.com/en-de/road-bikes/gravel-bikes/grail/            
##  5 https://www.canyon.com/en-de/road-bikes/cyclocross-bikes/inflite/      
##  6 https://www.canyon.com/en-de/road-bikes/triathlon-bikes/speedmax/      
##  7 https://www.canyon.com/en-de/road-bikes/race-bikes/ultimate/           
##  8 https://www.canyon.com/en-de/mountain-bikes/fat-bikes/dude/            
##  9 https://www.canyon.com/en-de/mountain-bikes/cross-country-bikes/exceed/
## 10 https://www.canyon.com/en-de/mountain-bikes/trail-bikes/grand-canyon/  
## # … with 15 more rows





# 2.0 COLLECT BIKE DATA ----

# 2.1 Get URL for each bike of the Product categories

# select first bike category url
bike_category_url <- bike_category_tbl$url[1]

# Alternatives for selecting values
# bike_category_url <- bike_category_tbl %$% url %>% .[1]
# bike_category_url <- bike_category_tbl %>% pull(url) %>% .[1]
# bike_category_url <- deframe(bike_category_tbl[1,])
# bike_category_url <- bike_category_tbl %>% first %>% first

xopen(bike_category_url)

# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  
  # Get the 'a' nodes, which are hierarchally underneath 
  # the class productTile__contentWrapper
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

# 2.1.2 Extract the descriptions (since we have retrieved the data already)
bike_desc_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
  
  # Extract the content of the attribute content
  html_attr("content") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "description")




# 2.1.3 Get even more data from JSON files
bike_json_tbl  <- html_bike_category %>%
  
  html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
  html_attr("data-gtm-impression") %>%
  
  # Convert the JSON format to dataframe
  # map runs that function on each element of the list
  map(fromJSON) %>% # need JSON ### need lists
  
  # Extract relevant information of the nested list
  map(purrr::pluck, 2, "impressions") %>% # Need purrr and expl above
  
  # Set "not defined" and emtpy fields to NA (will be easier to work with)
  map(na_if, "not defined") %>%
  map(na_if, "") %>%
  
  # The class of dimension56 and price varies between numeric and char.
  # This converts this column in each list to numeric
  # across allows to perform the same operation on multiple columns
  map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
  
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble so that we have the same data format
  as_tibble() %>%
  
  # Add consecutive numbers so that we can bind all data together
  # You could have also just use bind_cols()
  rowid_to_column(var='position') %>%
  left_join(bike_desc_tbl) %>%
  left_join(bike_url_tbl)






# 2.2 Wrap it into a function ----
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

# Run the function with the first url to check if it is working
bike_category_url <- bike_category_tbl$url[1]
bike_data_tbl     <- get_bike_data(url = bike_category_url)

bike_data_tbl
## # A tibble: 24 x 21
##    position name  id    brand category variant dimension50 dimension51
##                               
##  1        1 Aero… 2881  Cany… Road/Ra… 500119… 2020        Team Repli…
##  2        2 Aero… 2873  Cany… Road/Ra… 500111… 2020        Stealth    
##  3        3 Aero… 2874  Cany… Road/Ra… 500111… 2020        Stealth    
##  4        4 Aero… 2876  Cany… Road/Ra… 500119… 2020        aero silver
##  5        5 Aero… 2875  Cany… Road/Ra… 500118… 2020        Stealth    
##  6        6 Aero… 2877  Cany… Road/Ra… 500119… 2020        Stealth    
##  7        7 Aero… 2878  Cany… Road/Ra… 500119… 2020        Stealth    
##  8        8 Aero… 2225  Cany… Road/Ra… 500042… 2019        Stealth    
##  9        9 Aero… 2096  Cany… Road/Ra… 500008… 2019        Stealth    
## 10       10 Aero… 2091  Cany… Road/Ra… 500007… 2019        Stealth    
## # … with 14 more rows, and 13 more variables: dimension52 ,
## #   dimension53 , dimension54 , dimension55 , dimension63 ,
## #   feedProductId , quantity , price , metric4 ,
## #   dimension56 , metric5 , description , url 







# 2.3.1a Map the function against all urls

# Extract the urls as a character vector
bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)
saveRDS(bike_data_tbl, "bike_data_tbl.rds")






# # 2.3.1b Alternative with a for loop
# 
# # Create an empty tibble, that we can populate
# bike_data_tbl <- tibble()
# 
# # Loop through all urls
# for (i in seq_along(bike_category_tbl$url)) {
#   
#   bike_category_url <- bike_category_tbl$url[i]
#   bike_data_tbl     <- bind_rows(bike_data_tbl, get_bike_data(bike_category_url))
#   
#   # Wait between each request to reduce the load on the server 
#   # Otherwise we could get blocked
#   Sys.sleep(5)
#   
#   # print the progress
#   print(i)
#   
#}






# Check for duplicates
bike_data_tbl %>%
  group_by(id) %>%
  filter(n()>1) %>%
  arrange(id) %>% 
  View()





# Filter non Canyon bikes (based on id length) and add an empty column for the colors
bike_data_cleaned_tbl <- bike_data_tbl %>%
  
  # Filter for bikes. Only unique ones
  filter(nchar(.$id) == 4) %>%
  filter(!(name %>% str_detect("Frameset"))) %>%
  distinct(id, .keep_all = T) %>%
  
  # Split categories (Speedmax had to be treated individually)
  mutate(category = replace(category, 
                            name == "Speedmax CF SLX 8.0 SL", "Road/Triathlon Bike/Speedmax")) %>%
  separate(col = category, into = c("category_1",
                                    "category_2",
                                    "category_3"),
           sep = "(?<!\\s)/(?!\\s)") %>%
  
  # Renaming
  rename("year"       = "dimension50") %>%
  rename("model"      = "name") %>%
  rename("gender"     = "dimension63") %>%
  rename("price_euro" = "metric4") %>%
  
  # Fix years manually (have checked the website)
  mutate(year = replace_na(year, 2021)) %>%
  
  # Add frame material
  mutate(frame_material = case_when(
    model %>% str_detect(" CF ") ~ "carbon",
    model %>% str_detect(" CFR ") ~ "carbon",
    TRUE ~ "aluminium"
  )
  ) %>%
  
  # Select and order columns
  select(-c(position, brand, variant, starts_with("dim"), 
            quantity, feedProductId, price, metric5)) %>%
  select(id, model, year, frame_material, price_euro, everything())

saveRDS(bike_data_cleaned_tbl, "bike_data_cleaned_tbl.rds")







# 3.1a Get all color variations for each bike

# Extract all bike urls
bike_url_vec <- bike_data_cleaned_tbl %>% 
  pull(url)

# Create function to get the variations
get_colors <- function(url) {
  
  url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
}

# Run the function over all urls and add result to bike_data_cleaned_tbl
# This will take a long time (~ 20-30 minutes) because we have to iterate over many bikes
bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl, "bike_data_colors_tbl.rds")



install.packages("furrr")
library(furrr)     # Parallel Processing using purrr (iteration)
plan("multiprocess")
bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = future_map(bike_url_vec, get_colors))





# 3.2 Create the urls for each variation

bike_data_colors_tbl <- bike_data_colors_tbl %>%
  
  # Create entry for each color variation
  unnest(colors) %>%
  
  # Merge url and query parameters for the colors
  mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
  select(-url) %>%
  
  # Use stringi to replace the last dash with the HTLM format of a dash (%2F)
  # Only if there is a dash in the color column
  mutate(url_color = ifelse(str_detect(colors, pattern = "/"),
                            
                            # if TRUE --> replace      
                            stringi::stri_replace_last_fixed(url_color, "/", "%2F"),
                            
                            # ELSE --> take the original url
                            url_color))

bike_data_colors_tbl %>% glimpse()
## Rows: 416
## Columns: 12
## $ id               <chr> "2493", "2453", "2452", "2452", "2451", "2451",…
## $ model            <chr> "Aeroad CFR Disc EPS", "Aeroad CFR Disc AXS",…
## $ year             <chr> "2020", "2020", "2020", "2020", "2020", …
## $ frame_material   <chr> "carbon", "carbon", "carbon", "carbon", …
## $ price_euro       <chr> "8999.00", "7999.00", "7499.00", "7499.00",…
## $ category_1       <chr> "Road", "Road", "Road", "Road", "Road", …
## $ category_2       <chr> "Race", "Race", "Race", "Race", "Race", …
## $ category_3       <chr> "Aeroad", "Aeroad", "Aeroad", "Aeroad", …
## $ gender           <chr> "unisex", "unisex", "unisex", "unisex", …
## $ description      <chr> "Canyon - An aero road bike that combines …
## $ colors           <chr> "BK/BK", "BK/BK", "BK/BK", "BK/MC", "BK/BK",…
## $ url_color        <chr> "https://www.canyon.com/en-de/road-bikes/…





# Create function
get_sizes <- function(url) {
  
  json <- url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON(flatten = T) %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 2) %>%
    
    # select(id, value, available, availability)# %>%
    select(id, value, availability.onlyXLeftNumber) %>%
    
    # Rename
    rename(id_size = id) %>%
    rename(size = value) %>%
    rename(stock_availability = availability.onlyXLeftNumber) %>%
    
    # Conver to tibble
    as_tibble()
  
}

# Pull url vector
bike_url_color_vec <- bike_data_colors_tbl %>% 
  pull(url_color)

# Map
bike_data_sizes_tbl <- bike_data_colors_tbl %>% 
  mutate(size = future_map(bike_url_color_vec, get_sizes))

# Unnest
bike_data_sizes_tbl <- bike_data_sizes_tbl %>% 
  unnest(size)

saveRDS(bike_data_sizes_tbl, "bike_data_sizes_tbl.rds")






# 
# library(RSelenium)
# # Start the headless browser
# driver <- rsDriver(browser = "firefox")
# remDr  <- driver$client
# 
# # Open the url
# url    <- "https://www.canyon.com/en-de/road-bikes/race-bikes/aeroad/"
# remDr$navigate(url)
# 
# # Locate and click the button
# button <- remDr$findElement(using = "css", ".productGrid__viewMore")
# button$clickElement()
# 
# # Get the html
# html <- remDr$getPageSource() %>% 
#   unlist() %>% 
#   read_html()


























install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%















url_rose <- "https://www.rosebikes.de/fahrräder/mtb"

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

