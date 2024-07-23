library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)

read_excel(path = here("diets_historical_data", "Fur Seal Diet 2010-11.xlsx"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:AE113")

SC2010_11_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2010-11.xlsx"), 
  sheet = "Sample Contents", skip = 2, 
  range = ("A3:AE113"), 
  col_types = c("date", "numeric", "numeric", "date", 
                "text", "text",
                "date", rep("text", 4), rep("numeric", 19), "text")
)
SC2010_11 <- SC2010_11_ORIG %>% 
  rename(start_date = DATE, week_num = `#...2`, sample_num = `#...3`, 
         collection_date = Date...4, location = Loc, female_id = ID,
         process_date = Date...7, observer_code = Obs., 
         krill_type = Krill, fish_type = Fish, squid_type = Squid, 
         krill_carapaces_measured = Measured, 
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
         E.ant._Identity_eroded = eroded, unidentified_otoliths_all = all, 
         squid_dorsal_beak_count = dorsal, squid_ventral_beak_count = ventral,
         otolith_slides = ...29, total_otoliths = Otoliths, 
         notes = ...31) %>% 
  select(sample_num: squid_type, notes) %>%
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"), 
         squid_type = if_else(squid_type == "Y", "Yes", "No"), 
         female_id = if_else(female_id == "None", NA, female_id),
         location = if_else(location == "Vei?" | location == "NS", NA, location),
         # collection_date = as.Date(collection_date), 
         collection_date = case_when(
           sample_num == 91 ~ ymd("2011-02-20"),
           sample_num %in% c(92:95) ~ ymd("2011-02-19"),
           .default = as.Date(collection_date)),
         process_date = as.Date(process_date), 
         processor = NA_character_, #str_sub(observer_code, 1, 3), 
         collector = NA_character_, 
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         carapace_save = case_when(
           is.na(notes) ~ 0, 
           str_detect(tolower(notes), "carapaces saved") ~ 1, 
           .default = 0)) %>%
  select(sample_num: squid_type, collector, notes: carapace_save) %>% 
  mutate_location()


table(SC2010_11$sample_num, useNA = "ifany")
table(SC2010_11$sample_type, useNA = "ifany")
table(SC2010_11$species, useNA = "ifany")
table(SC2010_11$sex, useNA = "ifany")
table(SC2010_11$collection_date, useNA = "ifany")
table(SC2010_11$krill_type, useNA = "ifany")
table(SC2010_11$squid_type, useNA = "ifany")
table(SC2010_11$fish_type, useNA = "ifany")
table(SC2010_11$carapace_save, useNA = "ifany")

table(SC2010_11$fish_type, SC2010_11$squid_type, SC2010_11$krill_type,
      useNA = "ifany")
sum(duplicated(SC2010_11$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2010_11$location) | (SC2010_11$location %in% beaches$location))
all(is.na(SC2010_11$collector) | (SC2010_11$collector %in% observers$observer))
all(is.na(SC2010_11$processor) | (SC2010_11$processor%in% observers$observer))

SC2010_11$location[!(is.na(SC2010_11$location) | (SC2010_11$location %in% beaches$location))]

diets2010_11_todb <- SC2010_11 %>%
  left_join(beaches, by = join_by(location)) %>%
  left_join(tags, by = join_by(species, tag)) %>%
  select(-c(location, tag, female_id, observer_code)) %>%
  relocate(sample_type, species: tag_id, .before = notes)


#NS values mark them as NA so do a case when for specific coll-date (cell#) 
#and change it to its respective date
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

