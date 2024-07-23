library(dplyr)
library(odbc)
library(here)

con <- dbConnect(odbc(), filedsn = here("dsn/amlr-pinniped-db-test.dsn"))

diets <- tbl(con, "vDiets") %>% collect()

diets %>% glimpse()
