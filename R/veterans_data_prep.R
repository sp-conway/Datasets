

library(here)
library(tidyverse)

veterans <- read_csv(here("R","data","Rural_atlas_update23", "Veterans.csv"))
key <- read_csv(here("R","data","Rural_atlas_update23", "Variable Name Lookup.csv")) %>%
  filter(Category=="Veterans")
key
