#' # Load libraries 

library('readr')
library('dplyr')
library("tidylog")
library("forcats")
library("lubridate")
library("anytime")
library("rpivotTable")
library("DT")

#' # Load data

## data downloaded from here: 
## https://bouldercolorado.gov/open-data/police-stop-demographics/

raw_results <- read_csv("police_stop_data_results_2018.csv")
raw_main    <- read_csv("police_stop_data_main_2018.csv")
police_stop_data_dictionary <- read_csv("police_stop_data_dictionary.csv")

#' # View Raw Results table 

datatable(raw_results)

#' # View Raw Main table 

datatable(raw_main)

#' # Format data

raw_joined <- raw_results %>% 
  full_join(raw_main, by = c("appid" = "rpmainid"))

format_joined <- raw_joined %>% 
  rename(birth_year            = `Year of birth`, 
         stop_duration_minutes = Min, 
         boulder_resident      = enfaction, 
         # stop_date             = stopdate, 
         stop_ID               = appid, 
         item_code             = itemcode, 
         item_description      = itemdesc,
         activity_type         = appkey) %>% 
  mutate(item_code        = as_factor(item_code), 
         item_description = as_factor(item_description), 
         activity_type  = factor(activity_type, 
                                 levels = c("RPT1", "RPT2", "RPT3", 
                                            "RPT4", "RPT5", "RPT6", 
                                            "RPT7", "1"), 
                                 labels = c("Type of Stop",
                                            "Stop Reason", 
                                            "Search Conducted", 
                                            "Search Authority", 
                                            "Contraband Found", 
                                            "Result of Stop", 
                                            "Type Code Not Disclosed", 
                                            "Data error")),
         activity_date_time = dmy_hm(addtime), 
         stop_date          = dmy(stopdate),
         stop_date_time     = dmy_hm(paste(stop_date, stoptime)), 
         sex                = as_factor(sex), 
         race               = factor(race, 
                                     levels = c("A", "B", "I", "U", "W"), 
                                     labels = c("Asian", 
                                                "Black or African American", 
                                                "American Indian or Alaskan Native", 
                                                "Unknown", 
                                                "White")), 
         ethnic        = factor(ethnic, 
                                levels = c("H", "N"), 
                                labels = c("Hispanic", "Non-Hispanic")), 
         boulder_resident = as_factor(boulder_resident)) %>% 
  # streetdir not currently used, per data dict
  select(-streetdir) 

glimpse(format_joined)

# Create pivot table 

sub_tbl_for_pivot <- format_joined %>% 
  # remove cols not needed for the pivot table 
  select(
    # can't get date time working right now, so excluding
    # -addtime, 
    # -stopdate,
    -stop_date, 
    -stoptime, 
    -activity_date_time,
    -stop_date_time
  ) %>% 
  # rearrange cols
  select(stop_ID, 
         stopdate, 
         stop_duration_minutes,
         streetnbr, 
         street,
         sex, 
         race, 
         ethnic, 
         birth_year, 
         boulder_resident, 
         activity_type, 
         addtime,
         item_code, 
         item_description) %>% 
  arrange(stopdate, stop_ID)

#' # View data

datatable(sub_tbl_for_pivot)

#' # Table of stop types by race
rpivotTable(sub_tbl_for_pivot, 
            rows = c("activity_type", 
                     "item_description"), 
            cols = "race", 
            aggregatorName = "Count Unique Values", 
            vals = "stop_ID", 
            rendererName = "Table")  
