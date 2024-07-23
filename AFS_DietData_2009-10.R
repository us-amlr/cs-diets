library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
SC2009_10_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2009-10.xlsx"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A14:AA122", col_types = 
    c("date", "numeric", "text",
      "date", "text", "text", "date", rep("text", 4), 
      rep("numeric", 15), "text")
)

SC2009_10 <- SC2009_10_ORIG %>% 
  rename(start_date = DATE, week_num = `#...2`, sample_num = `#...3`,
         collection_date = Date...4, location = Loc, female_id = ID, 
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
         E.ant._Identity_eroded = `(eroded)`, unidentified_otoliths_all = all,
         squid_dorsal_beak_count = dorsal, squid_ventral_beak_count = ventral,
         otolith_slides = ...25, total_otoliths = Otoliths, 
         notes = ...27) %>% 
  select(sample_num: squid_type, notes) %>%
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"),
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         collection_date = as.Date(collection_date), 
         sample_num = if_else(sample_num == "48a", "47", sample_num),
         sample_num = as.numeric(sample_num),
         notes = if_else(sample_num == 47, 
                         "sample_num originally labeled as 48a", notes),
         process_date = as.Date(process_date),
         female_id = if_else(female_id == "na", NA, female_id), 
         carapace_save = 0,
         processor = NA_character_, #str_sub(Observer_Code, 1, 3),
         collector = NA_character_, 
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         carapace_save = case_when(
           is.na(notes) ~ 0,
           str_detect(tolower(notes), "carapaces saved") ~ 1,
           .default = 0)) %>%
  select(sample_num: squid_type, processor, collector, notes: carapace_save) %>% 
  mutate_location()


table(SC2009_10$sample_num, useNA = "ifany")
table(SC2009_10$sample_type, useNA = "ifany")
table(SC2009_10$species, useNA = "ifany")
table(SC2009_10$sex, useNA = "ifany")
table(SC2009_10$collection_date, useNA = "ifany")
table(SC2009_10$krill_type, useNA = "ifany")
table(SC2009_10$squid_type, useNA = "ifany")
table(SC2009_10$fish_type, useNA = "ifany")
table(SC2009_10$carapace_save, useNA = "ifany")
table(SC2009_10$fish_type, SC2009_10$squid_type, SC2009_10$krill_type,
      useNA = "ifany")
sum(duplicated(SC2009_10$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2009_10$location) | (SC2009_10$location %in% beaches$location))
all(is.na(SC2009_10$collector) | (SC2009_10$collector %in% observers$observer))
all(is.na(SC2009_10$processor) | (SC2009_10$processor%in% observers$observer))

SC2009_10$location[!(is.na(SC2009_10$location) | (SC2009_10$location %in% beaches$location))]

# diets2009_10_todb <- SC2009_10 %>%
#   left_join(beaches, by = join_by(location)) %>%
#   left_join(tags, by = join_by(species, tag)) %>%
#   select(-c(location, tag, female_id, observer_code)) %>% 
#   relocate(species: tag_id, .before = notes)

#TODO: Redownload the tamatoa() package for the most up to date mutate_location()
#before joining dfs

#Notes--------------------------------------------------------------------------
#Reading FALSE statement for location column argument
#Running lines SC2009_10$location and beaches$location lists the locations in
#these columns per cell
#is.na(SC2009_10$location) | (SC2009_10$location %in% beaches$location)
#a vector on where a TRUE statement appears or where both sides of argument are
#FALSE like those in lines 79-92 because the locations of SC2008_09$location were
#not found in beaches$location
#This line  SC2009_10$location[!(is.na(SC2009_10$location) | (SC2009_10$location %in% beaches$location))]
#gives the name output of the locations that don't appear in df in this case 
# being "Mod Fl" or wrapping this all in a table function to give a count of missing locations

#if_else() notes----
#Female_ID = if_else(Female_ID == "na"|"335", "NA", "na"
#Female_ID = if_else(Female_ID %n% ("na","335"), "NA", "na"
#  Female_ID = if_else(Female_ID == "na" | (can use the OR command for 
#multiple variables), NA, Female_ID)
# #mutate(rank = min_rank(desc(Sample_Num))) %>% 
# select(-rank)

#Changing cell value------
#SC2009_10[48,1] = 47
#SC2009_10$Sample_Num[48] = 47
#SC2009_10$Comments[48] = "Sample_Num originally labeled as 48a"