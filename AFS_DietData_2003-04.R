library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
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
  rename(week_num = `#...1`, sample_num = `#...2`, collection_date = Date...3, 
         location = Loc, female_id = ID, process_date = Date...6, observer_code
         = Obs., krill_type = Krill, fish_type = Fish, squid_type = 
           Squid, krill_carapaces_measured = Measured, 
         E.antarctica_left_Otolith_Count = left...12, 
         E.antarctica_right_Otolith_Count = right...13, 
         E.carlsbergi_left_Otolith_Count = left...14, 
         E.carlsbergi_right_Otolith_Count = right...15,
         G.nicholsi_left_Otolith_Count = left...16,
         G.nicholsi_right_Otolith_Count = right...17,
         G.sp._Total_eroded = Total...18, G.sp._eroded_Species = Sp., 
         unidentified_otoliths_all = all,
         squid_dorsal_beak_count = Upper, squid_ventral_beak_count = Lower,
         total_squid_beak_count = Total...23, otolith_slides = ...24, 
         total_otoliths = ...25, notes = ...26
  ) %>% 
  select(sample_num, collection_date: squid_type, notes) %>%
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"), 
         squid_type = if_else(squid_type == "Y", "Yes", "No"), 
         female_id = if_else(female_id == "-", NA, female_id),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date), 
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         processor = NA_character_, #str_sub(observer_code, 1, 3), 
         collector = NA_character_, carapace_save = "0") %>%
  select(sample_num: squid_type, collector, notes: carapace_save) %>% 
  mutate_location()

table(SC2003_04$sample_num, useNA = "ifany")
table(SC2003_04$sample_type, useNA = "ifany")
table(SC2003_04$species, useNA = "ifany")
table(SC2003_04$sex, useNA = "ifany")
table(SC2003_04$collection_date, useNA = "ifany")
table(SC2003_04$krill_type, useNA = "ifany")
table(SC2003_04$squid_type, useNA = "ifany")
table(SC2003_04$fish_type, useNA = "ifany")
table(SC2003_04$carapace_save, useNA = "ifany")
table(SC2003_04$fish_type, SC2003_04$squid_type, SC2003_04$krill_type,
      useNA = "ifany")
sum(duplicated(SC2003_04$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2003_04$location) | (SC2003_04$location %in% beaches$location))
all(is.na(SC2003_04$collector) | (SC2003_04$collector %in% observers$observer))
all(is.na(SC2003_04$processor) | (SC2003_04$processor%in% observers$observer))

SC2003_04$location[!(is.na(SC2003_04$location) | (SC2003_04$location %in% beaches$location))]
# 
# diets2003_04_todb <- SC2003_04 %>%
#   left_join(beaches, by = join_by(location)) %>%
#   left_join(tags, by = join_by(species, tag)) %>%
#   select(-c(location, tag, female_id, observer_code)) %>%
#   relocate(species: tag_id, .before = notes)
