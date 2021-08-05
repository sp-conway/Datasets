# aus_mar_data_prep.R
# Working with australian marriage law survey data
# Sean Conway
# Last Modified august 2021

# libraries
library(tidyverse)
library(readxl)
library(here)

# Only reading in clear responses, and ignoring totals
aus_mar <- read_excel(here("R","data","australian_marriage_law_postal_survey_2017_-_response_final.xls"),
                    sheet=2, range="A8:E15", 
                    col_names = c("territory","yes_num","yes_perc","no_num","no_perc"))
aus_mar

# This data may actually be fine as is, depending on your purpose
# But we are going to treat each individual territory's response as an observation
# e.g., tasmania - no response is an observation

# first, pivot_longer() elongates data
# Then we classify responses as "yes" or "no" using case_when()
# Also classify values as "number" or "percent" using case_when()
# then remove "name" column
# finally, use pivot_wider() to separate no and yes into two different rows
aus_mar_tidy <- aus_mar %>% 
  pivot_longer(cols=c(yes_num:no_perc)) %>% 
  mutate(
    resp=case_when(
      str_detect(name, "yes") ~ "yes",
      str_detect(name, "no") ~ "no"
    ),
    type=case_when(
      str_detect(name, "num") ~ "count",
      str_detect(name, "perc") ~ "percent"
    )
  ) %>% 
  select(-c(name)) %>%
  pivot_wider(id_cols = c(territory,resp),
              names_from = type, values_from = value)

# save data
#save(aus_mar_tidy, file=here("R","data_cleaned","australian_marriage_tidy.RData"))
