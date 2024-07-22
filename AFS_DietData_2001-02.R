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

SC2001_02 <- SC2001_02_ORIG %>% 
  rename(Week_Num = `Week #`, Sample_Num = `Sample #`, Collection_Date = Date...3,
         Location = Loc, Female_ID = ID, 
         Process_Date = Date...6, Krill_Presence = Krill, Fish_Presence = Fish,
         Squid_Presence = Squid, Krill_Carapaces_Measured = Measured,
         E.antarctica_left_Otolith_Count = left...11, 
         E.antarctica_right_Otolith_Count = right...12,
         E.carlsbergi_left_Otolith_Count = left...13, 
         E.carlsbergi_right_Otolith_Count = right...14,
         G.nicholsi_left_Otolith_Count = left...15, 
         G.nicholsi_right_Otolith_Count = right...16,
         G.sp._eroded_left_Otoliths = left...17, 
         G.sp._eroded_right_Otoliths = right...18,
         Unidentified_Otoliths_All = Unid., 
         Squid_Beaks_Total_Counts = Beaks,
         Comments = Obs.) %>%
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Observer_Code = NA,
         Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"), 
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"),
         Female_ID = if_else(Female_ID == "\"-\"", NA, Female_ID),
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date), 
         Processor = str_sub(Observer_Code, 1, 3), 
         Collector = NA_character_,
         Carapace_Save = "0") %>%
  select(Sample_Num: Squid_Presence,
         Comments: Carapace_Save) %>% 
  relocate(Sample_Type:Carapace_Save, .before = Comments)
