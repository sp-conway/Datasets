# poultry_data_prep.R
# Reading in Organic Poultry prices, 2004-2013 and wrangling the data
# Advanced data reading/wrangling
# Sean Conway
# Last modified August 2021

# libraries
library(tidyverse)
library(here)
library(readxl)

# Because the excel sheet is tricky, we need to read in the data in pieces and then bind it together
# First, get all the starting and ending points for each data section 
starting_points <- seq(5,77,by=8)
ending_points <- seq(9,81,by=8)

# Next, we paste these together to create all the cell ranges we need
make_range <- function(x,y) paste0("A",x,":M",y)
cell_ranges <- map2(starting_points, ending_points, make_range) 

# Each list element is a range of cells to read in individually (specifically, a different year's worth of data)
cell_ranges

# Manually specifying column names
col_names <- c("Product","January","February","March","April","May","June","July","August","September","October","November","December")

# Each section is a different year's data, but we don't actually have a column for that in the data
# So we have to make one
# BUT, data start at 2013 and go backwards
year <- 2013:2004

# Also specifying column types
# First column is the poultry product type, other 12 are prices for each month
col_types <- c("text",rep("numeric",12))

# This is the trickiest part - using map2() from purrr to create a custom function to read in each section of data
# .x = cell_ranges (the ranges for each section)
# .y = year (the year for each section)
# .f - custom function using read_excel() and mutate() (Also here() to specify the path)
#   sheet = 3 - data are in the third sheet from the excel file
#   range = .x - this is where we specify range for each data section (vectorized)
#   col_names = col_names - column names are the same for each data section
#   na = "too few" - 2004 data use "too few" when there wasn't enough data to get an avg. price.
#     This is tricky for us because it will cause the columns in 2013 to be character rather than numeric
#     so we need to replace "too few" with NA right away
#   col_types = col_types - specifying the column types to be safe
#   mutate(Year=.y) - creating a "Year" column using the input from .y (see year above)
data_lists <- map2(.x=cell_ranges, .y=year, .f=~read_excel(path=here("R","data","organiceggpoultry.xls"),
                  sheet = 3, range=.x, col_names = col_names, na="too few",col_types = col_types) %>%
              mutate(Year=.y)) 
data_lists 

# Now we have the data in a list, which each list element is a tibble containing poultry data from a different year
# We can use bind_rows() to bind oll our data into a single tibble
poultry <- data_lists %>%
  bind_rows()
poultry 
# Wha-la! We now have a single tibble with all our data 
# But there's some tidying to do
# The variable "month" is wide - spread across columns
# We use pivot_longer() to move the month name to a column "Month" and the price values to a column "Price"
# Then use mutate() to convert price from cents to dollars
# And then remove the cents column of price, only keeping "Price_Dollar"
poultry_tidy <- poultry %>%
  pivot_longer(cols=January:December,
               values_to = "Price",
               names_to = "Month") %>%
  mutate(
    Price_Dollar=Price/100
  ) %>%
  select(-c(Price))
poultry_tidy
# We now have a tidy poultry dataset! 
# You"ll notice that the prices stay (mostly) constant within a givin year
# That's okay.

# Saving data
#save(poultry_tidy, file = here("R","data_cleaned","poultry_tidy.RData"))
#write_csv(poultry_tidy, file=here("R","data_cleaned","poultry_tidy.csv"))
#xlsx::write.xlsx(as.data.frame(poultry_tidy),row.names=FALSE, file = here("R","data_cleaned","poultry_tidy.xlsx"))

    