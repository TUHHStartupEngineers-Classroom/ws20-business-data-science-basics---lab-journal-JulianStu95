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



bikes_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  
  # Separate product category name in main and sub
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))


#1.1 Basic select() ----
bikes_tbl %>%
  select(bike_id, model, model_year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))


#1.2 Reduce columns ----
bikes_tbl %>%
  select(model, price)


#1.3 Rearrange columns ----
bikes_tbl %>%
  select(category_1:category_3, everything())

# Alternative using relocate()
bikes_tbl %>%
  relocate(category_1:category_3)

# 1.4 Select helpers ----
bikes_tbl %>%
  select(starts_with("model"))

# 1.5 Pull() extracts content of a tibble column ----
bikes_tbl %>%
  # select(price) %>% Does not work
  pull(price) %>%
  mean()

#1.6 Select() and where() ----
bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))

#1.7 select and rename ----
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )

#1.8 Select and rename all ----
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

# An example using str_replace
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())



#2.1 Arranging ----
bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%

#2.2 Filtering rows ----
#2.2.1 Filter rows, where price is greater than the mean of price ----
  bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price))

#2.2.2 Filter rows, where price is greater 5000 or lower 1000 and sort by descending price ----
bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

#2.2.3 Filter rows, where price is greater 5000 and the model contains “Endurace” ----
bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
  )

#2.2.4 Filter rows, where the category_1 is “Hybrid / City” or “E-Bikes”. Use the %in% operator: ----
bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

#2.2.5 Filter rows, where the category_2 is “E-Mountain”: ----
bikes_tbl %>%
  filter(category_2 == "E-Mountain")

#2.2.6 Negate 2.2.4 and 2.2.5 ----
bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))

# ----

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

#

# ----


library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)


covid_data_dt <- fread("00_data/01_bike_sales/02_wrangled_data/covid19.csv")

class(covid_data_dt)
##  "data.table" "data.frame"


# Create a large .csv file
test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
write.csv(test_df, 'test_df.csv', row.names = F)

# Time taken by read.csv to import
system.time({test_df_base <- read.csv("test_df.csv")})
# Time taken by read_csv to import
system.time({test_df_readr <- read_csv("test_df.csv")})
# Time taken by fread to import
system.time({test_dt <- fread("test_df.csv")})

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)


## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]

# covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2019, sum(cases), by = continentExp]



# List all internal data sets
data()

# Load specified data sets
data("airquality")


# List names 
colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")



# List all internal data sets
data()

# Load specified data sets
data("airquality")

# Solution 1
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]


# Solution 2
setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]


data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]






covid_data_dt[,sum(deaths > 1000)]

# to list the observations put it in i
covid_data_dt[deaths > 1000]



covid_data_dt[, deaths_per_capita := deaths / popData2019]


covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]

# To delete a column, assign it to NULL
covid_data_dt[, deaths_per_cases := NULL]


covid_data_dt[country == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              .N
]

covid_data_dt[deaths > 1000, .N, by = country]

covid_data_dt[,.I[deaths > 1000]]

covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)
]


covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases) %>% round(1), 
                                      m_deaths = mean(deaths) %>% round(1)), 
                                   by = .(country)
]

covid_data_dt[, .(
  m_cases  = round(mean(cases),  digits = 1), 
  m_deaths = round(mean(deaths), digits = 1)
), 
by = .(country)][order(-m_cases)]



covid_data_dt[, .N, 
              .(
                death_gt_1k = deaths > 1000, 
                cases_lt_1k = cases < 1000
              )
]


covid_data_dt[, print(.SD), by = year]

#covid_data_dt[, lapply(.SD, mean), by = year]


covid_data_dt[, lapply(.SD, mean), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]




# Create a new data.table
covid_data_EUR_dt <- covid_data_dt[ continent == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(country), 
                                    .SDcols = c("cases", "deaths")
]

# Set key
setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)

# Create two data.tables from that
cd_dt1 <- covid_data_EUR_dt[, .(country, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country, deaths)]

# Join them
cd_dt1[cd_dt2]


# Remove keys
setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)
# Join
cd_dt1[cd_dt2, on = "country"]
# If they had different colnames
#cd_dt1[cd_dt2, on = c(colA = "colB")]

# Alternatively you can use the function merge()
# Inner Join
merge(cd_dt1, cd_dt2, by='country')
# Left Join
merge(cd_dt1, cd_dt2, by='country', all.x = T)
# Outer Join
merge(cd_dt1, cd_dt2, by='country', all = T)
# If they had different colnames use by.x="colA", by.y="colB"

cd_dt1[cd_dt2, on = "country", deaths := i.deaths]


dt_list    <- list(cd_dt1, cd_dt2, cd_dt3)
merge_func <- function(...) merge(..., all = TRUE, by='country')
dt_merged  <- Reduce(merge_func, dt_list)








# 2.0 DATA IMPORT ----

# 2.1 Loan Acquisitions Data ----

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))


acquisition_data <- vroom(
  file       = "00_data/loan_data/Acquisition_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_acq),
  col_types  = col_types_acq,
  na         = c("", "NA", "NULL"))

acquisition_data %>% glimpse()





# 2.2 Performance Data ----
col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "00_data/loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))

performance_data %>% glimpse()




# 3.1 Acquisition Data ----
class(acquisition_data)

setDT(acquisition_data)

class(acquisition_data)

acquisition_data %>% glimpse()

# 3.2 Performance Data ----
setDT(performance_data)

performance_data %>% glimpse()



# 4.0 DATA WRANGLING ----

# 4.1 Joining / Merging Data ----

tic()
combined_data <- merge(x = acquisition_data, y = performance_data, 
                       by    = "loan_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()

combined_data %>% glimpse()

# Same operation with dplyr
tic()
performance_data %>%
  left_join(acquisition_data, by = "loan_id")
toc()


# 4.2 Prepare the data ----
# Preparing the Data Table

setkey(combined_data, "loan_id")
key(combined_data)

?setorder()
setorderv(combined_data, c("loan_id", "monthly_reporting_period"))

# 4.3 Select columns----
# 4.3 Select Columns ----
combined_data %>% dim()

keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")


combined_data <- combined_data[, ..keep_cols]

combined_data %>% dim()

combined_data %>% glimpse()


# 4.4 Grouped Mutations ----
combined_data$current_loan_delinquency_status %>% unique()
##  0 NA  1  2  3  4  5  6  7  8  9 10 11 12

# or:
combined_data[,current_loan_delinquency_status] %>% unique()
##  0 NA  1  2  3  4  5  6  7  8  9 10 11 12



# 4.4 Grouped Mutations ----
# - Add response variable (Predict wether loan will become delinquent in next 3 months)

# dplyr
tic()
temp <- combined_data %>%
  group_by(loan_id) %>%
  mutate(gt_1mo_behind_in_3mo_dplyr = lead(current_loan_delinquency_status, n = 3) >= 1) %>%
  ungroup()  
toc()

combined_data %>% dim()
temp %>% dim()

# data.table
tic()
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]
toc()

combined_data %>% dim()

# Remove the temp variable
rm(temp)



# 5.1 How many loans in a month ----
tic()
combined_data[!is.na(monthly_reporting_period), .N, by = monthly_reporting_period]
toc()

tic()
combined_data %>%
  filter(!is.na(monthly_reporting_period)) %>%
  count(monthly_reporting_period) 
toc()



# 5.2 Which loans have the most outstanding delinquencies ----
# data.table
tic()
combined_data[current_loan_delinquency_status >= 1, 
              list(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)][
                , max(current_loan_delinquency_status), by = loan_id][
                  order(V1, decreasing = TRUE)]
toc()

# dplyr
tic()
combined_data %>%
  group_by(loan_id) %>%
  summarise(total_delinq = max(current_loan_delinquency_status)) %>%
  ungroup() %>%
  arrange(desc(total_delinq))
toc()


# 5.3 Get last unpaid balance value for delinquent loans ----
# data.table
tic()
combined_data[current_loan_delinquency_status >= 1, .SD[.N], by = loan_id][
  !is.na(current_upb)][
    order(-current_upb), .(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)  
  ]
toc()

# dplyr
tic()
combined_data %>%
  filter(current_loan_delinquency_status >= 1) %>%
  filter(!is.na(current_upb)) %>%
  
  group_by(loan_id) %>%
  slice(n()) %>%
  ungroup() %>%
  
  arrange(desc(current_upb)) %>%
  select(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)
toc()




# 5.4 Loan Companies with highest unpaid balance
# data.table
tic()
upb_by_company_dt <- combined_data[!is.na(current_upb), .SD[.N], by = loan_id][
  , .(sum_current_upb = sum(current_upb, na.rm = TRUE), cnt_current_upb = .N), by = seller_name][
    order(sum_current_upb, decreasing = TRUE)]
toc()

upb_by_company_dt

# dplyr
tic()
upb_by_company_tbl <- combined_data %>%
  
  filter(!is.na(current_upb)) %>%
  group_by(loan_id) %>%
  slice(n()) %>%
  ungroup() %>%
  
  group_by(seller_name) %>%
  summarise(
    sum_current_upb = sum(current_upb, na.rm = TRUE),
    cnt_current_upb = n()
  ) %>%
  ungroup() %>%
  
  arrange(desc(sum_current_upb))
toc()