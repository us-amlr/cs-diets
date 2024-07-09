library(here)
library(readxl)
library(dplyr)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2010-11.xlsx"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:AE113")

SC2010_11_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2010-11.xlsx"), 
  sheet = "Sample Contents", skip = 2, 
  range = ("A3:AE113"), 
  col_types = c("date", "numeric", "numeric", "date", 
                "text", "guess",
                "date", "text", "text", "text", 
                "text", rep("numeric", 19), "text")
)
SC2010_11 <- SC2010_11_ORIG %>% 
  rename(Start_Date = DATE, Week_Num = `#...2`, Sample_Num = `#...3`, 
         Collection_Date = Date...4, Location = Loc, Female_ID = ID,
         Process_Date = Date...7, Observer_Code = Obs., 
         Krill_Presence = Krill, Fish_Presence = Fish, Squid_Presence = Squid, 
         Krill_Carapaces_Measured = Measured, 
         E.antarctica_left_Otolith_Count = left...13, 
         E.antarctica_right_Otolith_Count = right...14,
         E.carlsbergi_left_Otolith_Count = left...15, 
         E.carlsbergi_right_Otolith_Count = right...16,
         G.nicholsi_left_Otolith_Count = left...17, 
         G.nicholsi_right_Otolith_Count = right...18, 
         Pleuragramma_left_Otolith_Count = left...19,
         Pleuragramma_right_Otolith_Count = right...20, 
         Notolepis_coatsi_left_Otolith_Count = left...21, 
         Notolepis_coatsi_right_Otolith_Count = right...22, 
         G.sp._eroded_left_Otoliths = left...23, 
         G.sp._eroded_right_Otoliths = right...24, 
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
         Process_Date = as.Date(Process_Date)) %>% 
  select(Sample_Num: Squid_Presence, Comments: Sex)

#Notes on renaming columns---------------------
# View(SC2010_11)
# SC2010_11$DATE
# names(SC2010_11)
# 
# names(SC2010_11)[1] <- "Sample_Num"
# names(SC2010_11)[2] <- "Collection_Date"
# names(SC2010_11)[3] <- "Location"
# names(SC2010_11)[4] <- "Female_ID"
# names(SC2010_11)[5] <- "Process_Date"
# names(SC2010_11)[6] <- "Observer_Code"
# names(SC2010_11)[7] <- "Krill_Presence"
# names(SC2010_11)[8] <- "Fish_Presence"
# names(SC2010_11)[9] <- "Squid_Presence"
# names(SC2010_11)[10] <- "Krill_Carapaces_Measured" 
# names(SC2010_11)[11] <- "E.antarctica_left_Otolith_Count"
# names(SC2010_11)[12]<- "E.antarctica_right_Otolith_Count"
# names(SC2010_11)[13]<- "E.carlsbergi_left_Otolith_Count"
# names(SC2010_11)[14]<- "E.carlsbergi_right_Otolith_Count"
# names(SC2010_11)[15]<- "G.nicholsi_left_Otolith_Count"
# names(SC2010_11)[16]<- "G.nicholsi_right_Otolith_Count"
# names(SC2010_11)[17]<- "Pleuragramma_left_Otolith_Count"
# names(SC2010_11)[18]<- "Pleuragramma_right_Otolith_Count"
# names(SC2010_11)[19]<- "Notolepis_coatsi_left_Otolith_Count"
# names(SC2010_11)[20]<- "Notolepis_coatsi_right_Otolith_Count"
# names(SC2010_11)[21]<- "G.sp._eroded_left_Otoliths"
# names(SC2010_11)[22]<- "G.sp._eroded_right_Otoliths"
# names(SC2010_11)[23]<- "E.ant._Identity_eroded"
# names(SC2010_11)[24]<- "Unidentified_Otoliths_All"
# names(SC2010_11)[25]<- "Squid_Dorsal_Beak_Count"
# names(SC2010_11)[26]<- "Squid_Ventral_Beak_Count"
# names(SC2010_11)[27]<- "Otolith_Slides"
# names(SC2010_11)[28]<- "Total_Otoliths"
# names(SC2010_11)[29]<- "Comments"
# SC2010_11$Collection_Date
# 
# #formatting cells for all years to be the same; ex: 
# #female ID(blank cells instead of "NA"), dates; in single cell instead of 
# #individual(R by default puts "NA" turns dates into text), 
# #"/" instead of "-".

