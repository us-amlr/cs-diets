library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2001-02.xls"), 
           sheet = "Sample Log", skip = 2, 
           range = "A15:U130")

SC2001_02_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2001-02.xls"), 
  sheet = "Sample Log", skip = 2, 
  range = "A15:U130", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 3), rep("numeric", 11), 
                "text")
)
