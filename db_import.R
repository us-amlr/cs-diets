# Sam's script to import data into the database

library(dplyr)
library(odbc)
library(here)

con <- dbConnect(odbc(), filedsn = here("dsn/amlr-pinniped-db-test.dsn"))

diets.colnames <- tbl(con, "vDiets") %>% colnames()

tbl(con, "vDiets") %>% glimpse()
tbl(con, "vDiets") %>%
  group_by(season_name) %>% 
  summarise(count = n()) %>% 
  arrange(season_name) %>% 
  print(n = 30)


### Source the file to import
# source("afs_diets_199899.R", local = TRUE)
# source("afs_diets_199900.R", local = TRUE)
# source("AFS_DietData_2000-01.R", local = TRUE); todb <- diets2000_01_todb
source("AFS_DietData_2001-02.R", local = TRUE); todb <- diets2001_02_todb
# source("AFS_DietData_2002-03.R", local = TRUE)
# source("AFS_DietData_2003-04.R", local = TRUE)
# source("AFS_DietData_2004-05.R", local = TRUE)
# source("AFS_DietData_2005-06.R", local = TRUE)
# source("AFS_DietData_2006-07.R", local = TRUE)
# source("AFS_DietData_2007-08.R", local = TRUE)
# source("AFS_DietData_2008-09.R", local = TRUE)
# source("AFS_DietData_2009-10.R", local = TRUE)
# source("AFS_DietData_2010-11.R", local = TRUE)
# source("AFS_DietData_2011-12.R", local = TRUE)

### Import the data
stopifnot(
  all(names(todb) %in% diets.colnames)
)
glimpse(todb)
DBI::dbAppendTable(con, "diets", todb)
