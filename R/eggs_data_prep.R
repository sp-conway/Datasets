# eggs_data_prep.R
# Prepping organiceggpoultrydata.xls for use in DACSS 601 Data Wrangling problem
# Sean Conway
# Last modified August 2021

# libraries
library(tidyverse)
library(here)
library(readxl)

# reading in data
eggs <- read_excel(here("R","data","organiceggpoultry.xls"),
                   sheet = 1, range = "B5:F125")

# Data wrangling & cleaning
year <- 2004:2013
n <- 12
year_var <- map2(year, n, rep) %>% 
  unlist()

eggs_1 <- eggs %>% 
  rename(
    month=`...1`,
    extra_large_dozen=`Extra Large \nDozen`,
    extra_large_half_dozen=`Extra Large 1/2 Doz.\n1/2 Dozen`,
    large_dozen=`Large \nDozen`,
    large_half_dozen = `Large \n1/2 Doz.`
  ) %>%
  mutate(
    year=year_var,
    month=str_replace_all(month,c("\\d"="","[:punct:]"="","Jan"="January","\\s"=""))
  ) %>%
  relocate(month, year, large_half_dozen, large_dozen, extra_large_half_dozen, extra_large_dozen)

# writing data to file
#write_csv(eggs_1, file=here("R","data_cleaned","eggs_2004_2013.csv"))
