library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2003-04.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:Z115")

SC2003_04_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2003-04.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A3:Z115", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 4), rep("numeric", 8), 
                "text", rep("numeric", 6), "text")
)

SC2003_04 <- SC2003_04_ORIG %>% 
  rename(Week_Num = `#...1`, Sample_Num = `#...2`, Collection_Date = Date...3, 
         Location = Loc, Female_ID = ID, Process_Date = Date...6, Observer_Code
         = Obs., Krill_Presence = Krill, Fish_Presence = Fish, Squid_Presence = 
           Squid, Krill_Carapaces_Measured = Measured, 
         E.antarctica_left_Otolith_Count = left...12, 
         E.antarctica_right_Otolith_Count = right...13, 
         E.carlsbergi_left_Otolith_Count = left...14, 
         E.carlsbergi_right_Otolith_Count = right...15,
         G.nicholsi_left_Otolith_Count = left...16,
         G.nicholsi_right_Otolith_Count = right...17,
         G.sp._Total_eroded = Total...18, G.sp._eroded_Species = Sp., 
         Unidentified_Otoliths_All = all,
         Squid_Dorsal_Beak_Count = Upper, Squid_Ventral_Beak_Count = Lower,
         Total_Squid_Beak_Count = Total...23, Otolith_Slides = ...24, 
         Total_Otoliths = ...25, Comments = ...26
  ) %>% 
  select(Sample_Num, Collection_Date: Squid_Presence, Comments) %>%
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"), 
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"), 
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date), 
         Collector = str_sub(Observer_Code, 1, 3), Carapace_Save = "0") %>%
  select(Sample_Num: Squid_Presence, Collector, Comments: Carapace_Save) %>% 
  relocate(Sample_Type:Carapace_Save, .before = Comments)


