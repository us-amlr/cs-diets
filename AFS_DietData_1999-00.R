library(here)
library(readxl)
library(tidyverse)

#Need to start from beginning
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2002-03.xls"), 
           sheet = "Enema - Scat Contents", skip = 2, 
           range = "A3:AA106")

SC2002_03_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2002-03.xls"), 
  sheet = "Enema - Scat Contents", skip = 2, 
  range = "A3:AA106", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 4), rep("numeric", 16), 
                "text")
)