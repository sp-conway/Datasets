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

### READING IN ALL SHEETS - More Advanced =========

# Creating a vector of anticipated column names
# Fairly tedious - probably a better way to do this
col_names <- c("", "pay_grade","single_withoutchildren_male",
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
sheets <- excel_sheets(here("R","data","ActiveDuty_MaritalStatus.xls"))

read_mar_sheets <- function(sheet){
  read_excel(here("R","data","ActiveDuty_MaritalStatus.xls"),
             sheet = sheet,
             trim_ws = TRUE,
             guess_max = 37,
             col_names = col_names,
             skip=9) %>% 
    mutate("branch"=sheet) %>%
    filter(str_detect(pay_grade, regex("total",ignore_case = TRUE),negate = T)) %>%
    select(2:last_col()) %>%
    rename_with(.cols=2:last_col(), ~str_replace_all(.x, "_"," ")) %>%
    separate(col=pay_grade, into=c("enlisted","pay_grade"),
             sep="-")
}

# purrr-ing through the sheets. filtering, selecting, renaming, etc.
data <- purrr::map(sheets, read_mar_sheets)
                   
# if it worked, this should look pretty
data[[1]]

marital_tidy_all <- data %>% 
  bind_rows() %>%
  select(c(pay_grade,!contains("total"))) %>%
  pivot_longer(cols = !contains(c("pay_grade","enlisted","branch")),
               names_to = "status", values_to = "count") 

# saving data 
#map2(.x=sheets,.y=data, ~write_csv(.y, file=here("R","data_cleaned",paste0(tolower(.x), "_cleaned.csv"))))
#write_csv(marital_tidy_all, here("R","data_cleaned","marital_tidy_all.csv"))
