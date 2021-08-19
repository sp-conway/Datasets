# marital_data_prep.R
# cleaning and tidying DOD active duty marital status data
# Sean Conway
# Last modified August 2021

# libraries
library(tidyverse)
library(here)
library(readxl)

### READING IN JUST FIRST SHEET ========
marital_dod <- read_excel(here("R","data","ActiveDuty_MaritalStatus.xls"),
                      sheet = 1, range = "B10:Q37", # 
                      col_names = c("pay_grade","single_without_children_male",
                                    "single_without_children_female",
                                    "single_without_children_total",
                                    "single_with_children_male",
                                    "single_with_children_female",
                                    "single_with_children_total",
                                    "joint_service_marriage_male",
                                    "joint_service_marriage_female",
                                    "joint_service_marriage_total",
                                    "civilian_marriage_male",
                                    "civilian_marriage_female",
                                    "civilian_marriage_total",
                                    "male_marriage_total",
                                    "female_marriage_total",
                                    "total_marriage_total")) %>%
  filter(str_detect(pay_grade, "TOTAL",negate = T))

# replacing underscore with a space for pivoting
marital_dod_1 <- marital_dod %>%
rename_with(.cols=2:last_col(), ~str_replace_all(.x, "_"," "))

# Use pivot_longer() to 
marital_dod_tidy <- marital_dod_1 %>%
  select(c(pay_grade,!contains("total"))) %>%
  pivot_longer(cols = !contains("pay_grade"),
               names_to = "status", values_to = "count") %>%
  mutate(status=str_replace_all(status, "_"," ")) %>%
  separate(col=pay_grade, into=c("enlisted","pay_grade"),
           sep="-")

#save(marital_dod_tidy, file=here("R","data_cleaned","marital_dod_tidy.RData"))

### READING IN ALL SHEETS - More Advanced ======

# Creating a vector of anticipated column names
# Fairly tedious - probably a better way to do this
col_names <- c("", "pay_grade","single_without_children_male",
              "single_without_children_female",
              "single_without_children_total",
              "single_with_children_male",
              "single_with_children_female",
              "single_with_children_total",
              "joint_service_marriage_male",
              "joint_service_marriage_female",
              "joint_service_marriage_total",
              "civilian_marriage_male",
              "civilian_marriage_female",
              "civilian_marriage_total",
              "male_marriage_total",
              "female_marriage_total",
              "total_marriage_total")

# defining our sheets
sheets <- excel_sheets(here("R","data","ActiveDuty_MaritalStatus.xls"))

# purrr-ing through the sheets. filtering, selecting, renaming
data <- purrr::map(sheets,~read_excel(here("R","data","ActiveDuty_MaritalStatus.xls"),
                              sheet = .x,
                              trim_ws = TRUE,
                              guess_max = 37,
                              col_names = col_names,
                              skip=9) %>% 
  filter(str_detect(pay_grade, "TOTAL",negate = T)) %>%
  select(2:last_col()) %>%
  rename_with(.cols=2:last_col(), ~str_replace_all(.x, "_"," ")) %>%
  separate(col=pay_grade, into=c("enlisted","pay_grade"),
           sep="-"))

# if it worked, this should look pretty
data[[1]]

# saving data 
#map2(.x=sheets,.y=data, ~write_csv(.y, file=here("R","data_cleaned",paste0(tolower(.x), "_cleaned.csv"))))



