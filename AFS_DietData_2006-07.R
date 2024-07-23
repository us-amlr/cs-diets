library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2006-07.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A14:AA114")

SC2006_07_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2006-07.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A14:AA114", col_types = c("date", "numeric", "numeric", "date",
                                     "text", "text", "date", "text",
                                     rep("text", 4), rep("numeric", 14),
                                     "text"))
SC2006_07 <- SC2006_07_ORIG %>% 
  rename(start_date = DATE, week_num = `#...2`, 
         sample_num = `#...3`, collection_date = 
           Date...4, location = Loc, female_id = ID, 
         process_date = Date...7, observer_code = Obs.,
         krill_type = Krill, fish_type = Fish, 
         squid_type = Squid, krill_carapaces_measured = Measured,
         E.antarctica_left_Otolith_Count = left...13, 
         E.antarctica_right_Otolith_Count = right...14,
         E.carlsbergi_left_Otolith_Count = left...15, 
         E.carlsbergi_right_Otolith_Count = right...16,
         G.nicholsi_left_Otolith_Count = left...17, 
         G.nicholsi_right_Otolith_Count = right...18,
         G.sp._eroded_left_Otoliths = left...19,
         G.sp._eroded_right_Otoliths = right...20, 
         E.ant._Identity_eroded = eroded, unidentified_otoliths_all = all,
         squid_dorsal_beak_count = dorsal, squid_ventral_beak_count = ventral,
         otolith_slides = ...25, total_otoliths = Otoliths, 
         notes = ...27) %>%
  select(sample_num: squid_type, notes) %>% 
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"),
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date),  
         processor = NA_character_, #str_sub(Observer_Code, 1, 3), 
         collector = NA_character_,
         female_id = if_else(female_id == "-", NA, female_id), 
         carapace_save = "0") %>%
  select(sample_num: squid_type, collector, notes: carapace_save)
