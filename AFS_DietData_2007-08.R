library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
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
  rename(start_date = DATE, week_num = `#...2`, 
         sample_num = `#...3`, collection_date = 
           Date...4, location = Loc, female_id = ID, 
         process_date = Date...7, observer_code = Obs.,
         krill_type = Krill, fish_type = Fish, 
         squid_type = Squid, krill_carapaces_measured = Measured,
         E.antarctica_left_Otolith_Count = left...13, 
         E.antarctica_right_Otolith_Count = right...14,
         E.carlsbergi_left_Otolith_Count = left...15, 
         E.carlsbergi_right_Otolith_Count = right...16,
         G.nicholsi_left_Otolith_Count = left...17, 
         G.nicholsi_right_Otolith_Count = right...18,
         Notolepis_coatsi_Total_Otoliths = `Notolepis coatsi`,
         G.sp._eroded_Total_Otoliths = eroded...20, 
         E.carlsbergi_eroded = eroded...21, E.antarctica_eroded = eroded...22,
         unidentified_otoliths_all = all,
         squid_dorsal_beak_count = dorsal, squid_ventral_beak_count = ventral,
         otolith_slides = ...26, total_otoliths = Otoliths, 
         notes = Comments) %>% 
  select(sample_num: squid_type, notes) %>% 
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"),
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date),
         female_id = if_else(female_id == " ", NA, female_id), 
         processor = NA_character_, #str_sub(Observer_Code, 1, 3), 
         collector = NA_character_,  carapace_save = case_when(
           is.na(notes) ~ 0, 
           str_detect(tolower(notes), "carapaces saved") ~ 1, 
           str_detect(tolower(notes), "saved 2 carapaces") ~ 1,
           .default = 0)) %>%
  filter(sample_num != "extra") %>% 
  select(sample_num: squid_type, collector, notes: carapace_save) 

table(SC2007_08$sample_num, useNA = "ifany")
table(SC2007_08$sample_type, useNA = "ifany")
table(SC2007_08$species, useNA = "ifany")
table(SC2007_08$sex, useNA = "ifany")
table(SC2007_08$collection_date, useNA = "ifany")
table(SC2007_08$fish_type, useNA = "ifany")
table(SC2007_08$squid_type, useNA = "ifany")
table(SC2007_08$krill_type, useNA = "ifany")
table(SC2007_08$carapace_save, useNA = "ifany")

beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2007_08$location) | (SC2007_08$location %in% beaches$location))
all(is.na(SC2007_08$collector) | (SC2007_08$collector %in% observers$observer))
all(is.na(SC2007_08$processor) | (SC2007_08$processor%in% observers$observer))

#TODO: "Copihue" needs to be added and Redownload tamatoa() for updated mutate_locations()

# diets2007_08_todb <- SC2007_08 %>%
#   left_join(beaches, by = join_by(location)) %>%
#   left_join(tags, by = join_by(species, tag)) %>%
#   select(-c(location, tag, female_id, observer_code)) %>% 
#   relocate(species: tag_id, .before = notes)
