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



col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_character(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)




col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  uuid = col_character()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



# 3.1 Acquisition Data ----
class(patent_assignee_tbl)

setDT(patent_assignee_tbl)

class(patent_assignee_tbl)

# 3.1 Acquisition Data ----
class(assignee_tbl)

setDT(assignee_tbl)

class(assignee_tbl)

# 3.1 Acquisition Data ----
class(patent_tbl)

setDT(patent_tbl)

class(patent_tbl)

# 3.1 Acquisition Data ----
class(uspc_tbl)

setDT(uspc_tbl)

class(uspc_tbl)



# patent_assignee_tbl %>%
#   left_join(assignee_tbl, by = "loan_id")

# assignee_tbl %>%
#   select(id) %>% 
#   rename(
#     `assignee_id`   = id
#   )

setnames(assignee_tbl, "id", "assignee_id")

 assig_patassig_combined_tbl = patent_assignee_tbl %>%
   right_join(assignee_tbl, by = "assignee_id")



# Question 1 ----
assig_patassig_combined_tbl %>%
  group_by(organization) %>%
  summarise(
    count = n(),
    ) %>%
  ungroup() %>%
  arrange(desc(count))



# # A tibble: 464,605 x 2
# organization                                 count
# <chr>                                        <int>
#   1 International Business Machines Corporation 139092
# 2 NA                                          123331
# 3 Samsung Electronics Co., Ltd.                93562
# 4 Canon Kabushiki Kaisha                       75910
# 5 Sony Corporation                             54343
# 6 Kabushiki Kaisha Toshiba                     49443
# 7 General Electric Company                     47122
# 8 Hitachi, Ltd.                                45375
# 9 Intel Corporation                            42157
# 10 Fujitsu Limited                              37197
# # ... with 464,595 more rows





# Question 2 ----
setnames(patent_tbl, "id", "patent_id")
 
 assig_patassig_patent_combined_tbl <- assig_patassig_combined_tbl %>%
   right_join(patent_tbl, by = "patent_id")

 assig_patassig_patent_combined_reduced_tbl <- assig_patassig_patent_combined_tbl  %>%
   select(organization, country, date) %>%
   separate(col  = date,
          into = c("year", "month", "day"),
          sep  = "-", remove = FALSE)
 
 
 assig_patassig_patent_combined_reduced2_tbl <- assig_patassig_patent_combined_reduced_tbl  %>%
   select(organization, country, year) %>%
   filter(year == 2019,
          country %>% str_detect("US")
   )
 
 assig_patassig_patent_combined_reduced2_tbl %>%
   group_by(organization) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
   
 
 # # A tibble: 55,729 x 2
 # organization                                count
 # <chr>                                       <int>
 #   1 NA                                          28824
 # 2 International Business Machines Corporation  9265
 # 3 Samsung Electronics Co., Ltd.                7205
 # 4 Canon Kabushiki Kaisha                       3595
 # 5 Intel Corporation                            3526
 # 6 LG Electronics Inc.                          3314
 # 7 Microsoft Technology Licensing, LLC          3106
 # 8 Apple Inc.                                   2817
 # 9 Ford Global Technologies, LLC                2624
 # 10 Amazon Technologies, Inc.                    2533
 # # ... with 55,719 more rows
 
 
 
 
 # Question 3 ----
 uspc_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 
 
 # What is the most innovative tech sector?
 # #    mainclass_id  count
 # <chr>         <int>
 #   1 257          435151
 # 2 428          427348 
 # 3 435          363222
 # 4 514          325600
 # 5 424          281633
 # 6 438          280069
 # 7 455          267749
 # 8 370          232483
 # 9 427          205278
 # 10 340          202712
 # # ... with 1,227 more rows
 # > 
 
 
 # For the top 10 companies (worldwide) with the most patents, what are the top 5 USPTO tech main classes?
 

 uspc_reduced_tbl <- uspc_tbl %>%
 select(patent_id, mainclass_id)
 
 assig_patassig_uspc_combined_tbl = assig_patassig_combined_tbl %>%
   right_join(uspc_reduced_tbl, by = "patent_id")
 
 assig_patassig_uspc_combined_reduced_tbl <- assig_patassig_uspc_combined_tbl %>%
   select(organization, mainclass_id)
 
 
# finde 10 unternehmen raus
 
 assig_patassig_combined_tbl %>%
   group_by(organization) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))

 
#1 International Business Machines Corporation
 aaa_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("International Business Machines Corporation")
   )
     
 aaa_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #2 Samsung Electronics Co., Ltd.
 aab_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Samsung Electronics Co., Ltd.")
   )
 
 aa2_tbl <- aab_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #3 Canon Kabushiki Kaisha
 aac_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Canon Kabushiki Kaisha")
   )
 
 aac_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #4  Sony Corporation 
 aad_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Sony Corporation ")
   )
 
 aad_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #5 Kabushiki Kaisha Toshiba
 aae_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Kabushiki Kaisha Toshiba")
   )
 
 aae_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #6 General Electric Company
 aaf_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("General Electric Company")
   )
 
 aaf_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #7 Hitachi, Ltd. 
 aag_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Hitachi, Ltd. ")
   )
 
 aag_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #8 Intel Corporation
 aah_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Intel Corporation")
   )
 
 aah_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
 #9 Fujitsu Limited
 aai_tbl <- assig_patassig_uspc_combined_reduced_tbl %>%
   select(organization, mainclass_id) %>%
   filter(organization %>% str_detect("Fujitsu Limited")
   )
 
 aai_tbl %>%
   group_by(mainclass_id) %>%
   summarise(
     count = n(),
   ) %>%
   ungroup() %>%
   arrange(desc(count))
 
    
 