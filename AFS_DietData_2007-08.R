library(here)
library(dplyr)
library(readxl)

read_excel(path = here("diets_historical_data", "Fur Seal Diet 2007-08.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A14:AB125")
SC2007_08_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2007-08.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = ("A14:AB125"), col_types = c(
    "date", "numeric", "text", 
    "date", "text", "numeric", "date", rep("text", 4), 
    rep("numeric", 16), "text")
)

SC2007_08 <- SC2007_08_ORIG %>% 
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
         Notolepis_coatsi_Total_Otoliths = `Notolepis coatsi`,
         G.sp._eroded_Total_Otoliths = eroded...20, 
         E.carlsbergi_eroded = eroded...21, E.antarctica_eroded = eroded...22,
         Unidentified_Otoliths_All = all,
         Squid_Dorsal_Beak_Count = dorsal, Squid_Ventral_Beak_Count = ventral,
         Otolith_Slides = ...26, Total_Otoliths = Otoliths, 
         Comments = Comments) %>% 
  select(Sample_Num: Squid_Presence, Comments) %>% 
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"),
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"),
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date),
         Female_ID = if_else(Female_ID == " ", NA, Female_ID)) %>% 
  select(Sample_Num: Squid_Presence, Comments: Sex)
