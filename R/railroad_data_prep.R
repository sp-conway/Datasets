# railroad_data_prep.R
# cleaning and tidying railroad 2012 data 
# Source - https://catalog.data.gov/dataset/total-railroad-employment-by-state-and-county-2012/resource/5a0b2831-23b9-4ce9-82e9-87a7d8f2c5d8
# Sean Conway
# Last modified July 2021

# libraries
library(tidyverse)
library(here)
library(readxl)

railroad_2012 <- read_excel(here("R","data","StateCounty2012.xls"),
                   skip = 3,trim_ws = T,n_max = 2986)

railroad_2012_clean_county <- railroad_2012 %>%
  select(STATE, COUNTY,TOTAL) %>% 
  rename(state=STATE, county=COUNTY, total_employees=TOTAL) %>%
  filter(str_detect(state, "Total",negate = T) & state != "CANADA")

railroad_2012_clean_state <- railroad_2012_clean_county %>%
  group_by(state) %>%
  summarise(total_employees=sum(total_employees,na.rm = T))

#save(railroad_2012_clean_county, file=here("R","data_cleaned","railroad_2012_county.RData"))
#save(railroad_2012_clean_state, file=here("R","data_cleaned","railroad_2012_state.RData"))
