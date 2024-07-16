library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2002-03.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:AA106")

SC2002_03_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2002-03.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A3:AA106", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 4), rep("numeric", 16), 
                "text")
)

SC2002_03 <- SC2002_03_ORIG %>% 
  rename( 
    Week_Num = `Week #`, Sample_Num = `Sample #`, Collection_Date = Date...3,
    Location = Loc, Female_ID = `Female ID`, 
    Process_Date = Date...6,
    Krill_Presence = `Krill Obs.`, Fish_Presence = Fish,
    Squid_Presence = Squid,
    E.antarctica_Presence = E.antarctica,
    E.carlsbergi_Presence = `E. carlsbergi`,
    G.nicholsi_Presence = `G. nicholsi`,
    B.picta_Presence = `B. picta`,
    Krill_Carapaces_Measured = Measured,
    E.antarctica_left_Otolith_Count = left...16, 
    E.antarctica_right_Otolith_Count = right...17,
    E.carlsbergi_left_Otolith_Count = left...18, 
    E.carlsbergi_right_Otolith_Count = right...19,
    G.nicholsi_left_Otolith_Count = left...20, 
    G.nicholsi_right_Otolith_Count = right...21, 
    G.sp._eroded_left_Otoliths = left...22, 
    G.sp._eroded_right_Otoliths = right...23,
    Unidentified_Otoliths_All = all,
    Squid_Beaks_Total_Counts = Beaks,
    Otolith_Slides = ...26, 
    Comments = Notes) %>% 
  select(Sample_Num: Krill_Presence, Fish_Presence, Squid_Presence, Comments) %>%
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Observer_Code = NA, Collector = NA,
         #Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"), 
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"),
         Female_ID = if_else(Female_ID == "no id", NA, Female_ID),
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date), 
         Carapace_Save = "0") %>%
  select(Sample_Num: Krill_Presence, Fish_Presence, Squid_Presence,
         Comments: Carapace_Save) %>% 
  relocate(Sample_Type:Carapace_Save, .before = Comments)
