library(here)
library(readxl)
library(dplyr)
library(stringr)
SC2011_12_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2011-12.xls"), 
  sheet = "Sample Contents", skip = 13, 
  range = "A14:AC105", 
  col_types = c("date", "numeric", 
                "numeric", "date",
                "text", "numeric", "date", 
                "text", "text", "text", "text", 
                rep("numeric", 17), "text")
)

SC2011_12 <- SC2011_12_ORIG %>% 
  rename(Start_Date = 1, 
         Week_Num = `#...2`, Sample_Num = `#...3`, Collection_Date = Date...4,
         Location = Loc, Female_ID = ID, 
         Process_Date = Date...7, Observer_Code = Obs., 
         Krill_Presence = Krill, Fish_Presence = Fish,
         Squid_Presence = Squid, Krill_Carapaces_Measured = Measured, 
         E.antarctica_left_Otolith_Count = left...13, 
         E.antarctica_right_Otolith_Count = right...14,
         E.carlsbergi_left_Otolith_Count = left...15, 
         E.carlsbergi_right_Otolith_Count = right...16,
         G.nicholsi_left_Otolith_Count = left...17, 
         G.nicholsi_right_Otolith_Count = right...18, 
         # # Notolepis_coatsi = Notolepis coatsi(Error: unexpected symbol in:
         # "         G.nicholsi_right_Otolith_Count = right...18, 
         # Notolepis_coatsi = Notolepis coatsi"),
         G.sp._eroded = eroded...20,
         E.carlbergi_eroded = eroded...21, 
         E.antarctica._eroded = eroded...22, Unidentified_Otoliths_All = all,
         Squid_Dorsal_Beak_Count = dorsal, Squid_Ventral_Beak_Count = ventral,
         Otolith_Slides_Start = ...26, Otolith_Slides_End = ...27, Total_Otoliths = Otoliths, 
         Comments = Comments) %>% 
  #Reassign column types in original dataframe before modifying 
    select(Sample_Num, Collection_Date: Squid_Presence, Comments) %>%
  mutate(Sample_Type = "Scat", Species = "Fur seal", Sex = "F",
         Krill_Presence = if_else(Krill_Presence == "Y", "Yes", "No"), 
         Fish_Presence = if_else(Fish_Presence == "Y", "Yes", "No"), 
         Squid_Presence = if_else(Squid_Presence == "Y", "Yes", "No"), 
         Collection_Date = as.Date(Collection_Date), 
         Process_Date = as.Date(Process_Date), 
         Collector = str_sub(Observer_Code, 1, 3)) %>%  
  select(Sample_Num: Squid_Presence, Collector, Comments: Sex)



#Renaming columns/Notes------
#excel uses "path" instead of "file" which is used in csv for pathway.
# SC2011_12$DATE
# SC2011_12$Date...7
# names(SC2011_12)
# names(SC2011_12)[1] <- "Start_Date"
# #rename columns, find number of krill, squid, fish, 
# #min/max of dates and sample column. To rename a renamed variable 
# #"Ctrl F" to find and replace.
# summary(SC2011_12$Start_Date)
# names(SC2011_12)[2] <- "Week_Num"
# names(SC2011_12)[3] <- "Sample_Num"
# names(SC2011_12)[4] <- "Collection_Date"
# names(SC2011_12)[5] <- "Location"
# names(SC2011_12)[6] <- "Female_ID"
# names(SC2011_12)[7] <- "Process_Date"
# names(SC2011_12)[8] <- "Observer_Code"
# names(SC2011_12)[9] <- "Krill_Presence"
# names(SC2011_12)[10]<- "Fish_Presence"
# names(SC2011_12)[11]<- "Squid_Presence"
# names(SC2011_12)[12]<- "Krill_Carapaces_Measured"
# names(SC2011_12)[13]<- "E.antarctica_left_Otolith_Count"
# names(SC2011_12)[14]<- "E.antarctica_right_Otolith_Count"
# names(SC2011_12)[15]<- "E.carlsbergi_left_Otolith_Count"
# names(SC2011_12)[16]<- "E.carlsbergi_right_Otolith_Count"
# names(SC2011_12)[17]<- "G.nicholsi_left_Otolith_Count"
# names(SC2011_12)[18]<- "G.nicholsi_right_Otolith_Count"
# names(SC2011_12)[19]<- "Notolepis_coatsi_Count"
# names(SC2011_12)[20]<- "G.sp._eroded_Otoliths"
# names(SC2011_12)[21]<- "E.carlsbergi_eroded_Otoliths"
# names(SC2011_12)[22]<- "E.antarctica_eroded_Otoliths"
# names(SC2011_12)[23]<- "Unidentified_Otoliths_All"
# names(SC2011_12)[24]<- "Squid_Dorsal_Beak_Count"
# names(SC2011_12)[25]<- "Squid_Ventral_Beak_Count"
# names(SC2011_12)[26]<- "Otolith_Slides_Start"
# names(SC2011_12)[27]<- "Otolith_Slides_End"
# names(SC2011_12)[28]<- "Total_Otoliths"

# SC2011_12$Krill_Presence
# table(SC2011_12$Krill_Presence, useNA = "ifany")
# 
# sum(SC2011_12$Krill_Presence == "Y")
# 
# is.na(SC2011_12$Krill_Presence)


# SC2011_12$Fish_Presence
# table(SC2011_12$Fish_Presence)
# 
# SC2011_12$Squid_Presence
# table(SC2011_12$Squid_Presence)
# 
# max(SC2011_12$Start_Date)
# min(SC2011_12$Start_Date)
# max(SC2011_12$Sample_Num)
# min(SC2011_12$Sample_Num)
# max(SC2011_12$Krill_Carapaces_Measured)
# min(SC2011_12$Krill_Carapaces_Measured)
# 
# View(SC2011_12)




#creating diets data frame

# diets <- SC2011_12 %>% 
#   select(Sample_Num, Collection_Date, Comments) %>% 
#   mutate(krill_presence == if_else(krill_presence = "Y", "Yes", "No")) 

#if_else function
