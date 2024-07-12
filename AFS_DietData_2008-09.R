library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2008-09.xls"), 
           sheet = "Sample Contents_updated", skip = 2, 
           range = "A14:AE114")

SC2008_09_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2008-09.xls"), 
  sheet = "Sample Contents_updated", skip = 2, 
  range = ("A14:AE114"),
  col_types = c("date", "numeric", "numeric", "date", 
                "text", "guess",
                "date", "text", "text", "text", 
                "text", rep("numeric", 19), "text")
)

SC2008_09 <- SC2008_09_ORIG %>% 
  rename(Start_Date = DATE, Week_Num = `#...2`, 
         Sample_Num = `#...3`, Collection_Date = 
           Date...4, Location = Loc, Female_ID = ID, 
         Process_Date = Date...7, Observer_Code = Obs.,
         Krill_Presence = Krill, Fish_Presence = Fish, 
         Squid_Presence = Squid, Krill_Carapaces_Measured = Measured,
         E.antarctica_left_Otolith_Count = left...13, 
         E.antarctica_right_Otolith_Count = right...14,
         E.carlsbergi_left_Otolith_Count = left...15, 
         E.carlsbergi_right_Otolith_Count = right...16,
         G.nicholsi_left_Otolith_Count = left...17, 
         G.nicholsi_right_Otolith_Count = right...18,
         Notolepis_coatsi_left_Otolith_Count = left...19,
         Notolepis_coatsi_right_Otolith_Count = right...20, 
         Pleuragramma_antarcticum_left_Otolith_Count = left...21,
         Pleuragramma_antarcticum_right_Otolith_Count = right...22,
         G.sp._eroded_left_Otoliths = ...23,
         G.sp._eroded_right_Otoliths = ...24, 
         E.ant._Identity_eroded = eroded, Unidentified_Otoliths_All = all,
         Squid_Dorsal_Beak_Count = dorsal, Squid_Ventral_Beak_Count = ventral,
         Otolith_Slides = ...29, Total_Otoliths = Otoliths, 
         Comments = ...31) %>% 
  select(Sample_Num: Squid_Presence, Comments) %>% 
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"),
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"),
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date),
         Female_ID = if_else(Female_ID == "n/a", NA, Female_ID), 
         Collector = str_sub(Observer_Code, 1, 3), Carapace_Save = "0") %>%
  select(Sample_Num: Squid_Presence, Collector, Comments: Carapace_Save) %>% 
  relocate(Sample_Type:Carapace_Save, .before = Comments)


?read_excel
