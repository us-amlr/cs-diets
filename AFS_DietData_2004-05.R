library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2004-05.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:X116")

SC2004_05_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2004-05.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A3:X116", col_types = c("numeric", "numeric", "date",
                                   "text", "text", "date",
                                   rep("text", 4), rep("numeric", 13), 
                                   "text"))

SC2004_05 <- SC2004_05_ORIG %>% 
  rename(Week_Num = `#...1`, 
         Sample_Num = `#...2`, Collection_Date = 
           Date...3, Location = Loc, Female_ID = ID, 
         Process_Date = Date...6, Observer_Code = Obs.,
         Krill_Presence = Krill, Fish_Presence = Fish, 
         Squid_Presence = Squid, Krill_Carapaces_Measured = Measured,
         E.antarctica_left_Otolith_Count = left...12, 
         E.antarctica_right_Otolith_Count = right...13,
         E.carlsbergi_left_Otolith_Count = left...14, 
         E.carlsbergi_right_Otolith_Count = right...15,
         G.nicholsi_left_Otolith_Count = left...16, 
         G.nicholsi_right_Otolith_Count = right...17,
         G.sp._eroded_left_Otoliths = left...18,
         G.sp._eroded_right_Otoliths = right...19, 
         Unidentified_Otoliths_All = all,
         Squid_Dorsal_Beak_Count = Upper, Squid_Ventral_Beak_Count = Lower,
         Otolith_Slides = ...23,
         Comments = ...24) %>%
  select(Sample_Num: Squid_Presence, Comments) %>% 
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"),
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"),
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date),
         Female_ID = if_else(Female_ID == "-", NA, Female_ID), 
         Processor = str_sub(Observer_Code, 1, 3), 
         Collector = NA_character_, Carapace_Save = "0") %>%
  select(Sample_Num: Squid_Presence, Collector, Comments: Carapace_Save) %>% 
  relocate(Sample_Type:Carapace_Save, .before = Comments)
