# marital_data_prep.R
# cleaning and tidying DOD active duty marital status data
# Sean Conway
# Last modified August 2021

# libraries
library(tidyverse)
library(here)
library(readxl)

# file path 
file_path <- here("data","ActiveDuty_MaritalStatus.xls")

### READING IN JUST FIRST SHEET ========
marital_dod_1 <- read_excel(file_path,
                      sheet = 1, range = "B10:Q37", 
                      col_names = c("pay_grade","single_withoutchildren_male",
                                    "single_withoutchildren_female",
                                    "single_withoutchildren_total",
                                    "single_withchildren_male",
                                    "single_withchildren_female",
                                    "single_withchildren_total",
                                    "married_jointservice_male",
                                    "married_jointservice_female",
                                    "married_jointservice_total",
                                    "married_civilian_female",
                                    "married_civilian_male",
                                    "married_civilian_total",
                                    "married_male_total",
                                    "married_female_total",
                                    "married_total_total")) %>%
  filter(str_detect(pay_grade, regex("total",ignore_case = TRUE),negate = T))

marital_dod_tidy <- marital_dod_1 %>%
  select(c(pay_grade,!contains("total"))) %>%
  pivot_longer(cols = !contains("pay_grade"),
               names_to = "status", values_to = "count") %>%
  separate(col=pay_grade, into=c("enlisted","pay_grade"),
           sep="-") %>% 
  separate(col=status, into = c("relationship", "family_status","gender"),
           sep = "_")

# Saving data
#save(marital_dod_tidy, file=here("R","data_cleaned","marital_dod_tidy.RData"))

### READING IN ALL SHEETS - More Advanced =========

# Creating a vector of anticipated column names
# Fairly tedious - probably a better way to do this
col_names_all <- c("", "pay_grade","single_withoutchildren_male",
                   "single_withoutchildren_female",
                   "single_withoutchildren_total",
                   "single_withchildren_male",
                   "single_withchildren_female",
                   "single_withchildren_total",
                   "married_jointservice_male",
                   "married_jointservice_female",
                   "married_jointservice_total",
                   "married_civilian_female",
                   "married_civilian_male",
                   "married_civilian_total",
                   "married_male_total",
                   "married_female_total",
                   "married_total_total")

# defining our sheets
sheets <- excel_sheets(file_path)

read_mar_sheets <- function(sheet_name){
  data <- read_excel(path = file_path,
                     sheet = sheet_name,
                     trim_ws = TRUE,
                     col_names = col_names_all,
                     skip=9) %>% 
    mutate("branch"=sheet_name) %>%
    select(2:last_col()) %>% # immediately remove out blank column
    select(c(pay_grade,!contains("total"))) %>% # remove columns with the word total
    filter(str_detect(pay_grade, regex("total",ignore_case = TRUE),negate = T)) %>%
    pivot_longer(cols = !contains(c("pay_grade","branch")), # these columns can remain as is
                 names_to = "status", values_to = "count") %>% 
    separate(col=pay_grade, into=c("enlisted","pay_grade"),
             sep="-") %>% 
    separate(col=status, into = c("relationship", "family_status","gender"),
             sep = "_")
  return(data)
}

# purrr-ing through the sheets. filtering, selecting, renaming, etc.
data <- purrr::map(sheets, read_mar_sheets)
                   
# if it worked, this should look pretty
data[[1]]

marital_tidy_all <- data %>% 
  bind_rows() 

# saving data 
#map2(.x=sheets,.y=data, ~write_csv(.y, file=here("data_cleaned",paste0(tolower(.x), "_cleaned.csv"))))
#write_csv(marital_tidy_all, here("data_cleaned","marital_tidy_all.csv"))
