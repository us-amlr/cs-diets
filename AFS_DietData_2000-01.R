library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2000-01.xls"), 
           sheet = "Sample Log", skip = 2, 
           range = "A15:Y119")

SC2000_01_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2000-01.xls"), 
  sheet = "Sample Log", skip = 2, 
  range = "A15:Y119", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 3), rep("numeric", 12), 
                rep("text", 4))
)