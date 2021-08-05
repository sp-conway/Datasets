# marital_data_prep.R
# cleaning and tidying DOD active duty marital status data
# Sean Conway
# Last modified July 2021

# libraries
library(tidyverse)
library(here)
library(readxl)

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

marital_dod_tidy <- marital_dod %>%
  select(c(pay_grade,!contains("total"))) %>%
  pivot_longer(cols = !contains("pay_grade"),
               names_to = "status", values_to = "count")  

#save(marital_dod_tidy, file=here("R","data_cleaned","marital_dod_tidy.RData"))





