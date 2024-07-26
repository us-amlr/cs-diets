library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2004-05.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:X116")

SC2004_05_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2004-05.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A3:X116", col_types = c("numeric", "numeric", "date",
                                   "text", "text", "date",
                                   rep("text", 4), rep("numeric", 13), 
                                   "text"))

SC2004_05 <- SC2004_05_ORIG %>% 
  rename(week_num = `#...1`, 
         sample_num = `#...2`, collection_date = 
           Date...3, location = Loc, female_id = ID, 
         process_date = Date...6, observer_code = Obs.,
         krill_type = Krill, fish_type = Fish, 
         squid_type = Squid, krill_carapaces_measured = Measured,
         E.antarctica_left_Otolith_Count = left...12, 
         E.antarctica_right_Otolith_Count = right...13,
         E.carlsbergi_left_Otolith_Count = left...14, 
         E.carlsbergi_right_Otolith_Count = right...15,
         G.nicholsi_left_Otolith_Count = left...16, 
         G.nicholsi_right_Otolith_Count = right...17,
         G.sp._eroded_left_Otoliths = left...18,
         G.sp._eroded_right_Otoliths = right...19, 
         unidentified_otoliths_all = all,
         squid_dorsal_beak_count = Upper, squid_ventral_beak_count = Lower,
         otolith_slides = ...23,
         notes = ...24) %>%
  select(sample_num: squid_type, notes) %>% 
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"),
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         collection_date = as.Date(collection_date),
         process_date = as.Date(process_date),
         female_id = if_else(female_id == "-", NA, female_id), 
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         processor = str_sub(observer_code, 1, 3), 
         collector = NA_character_, carapace_save = "0") %>%
  select(sample_num: squid_type, collector, notes: carapace_save) %>% 
  relocate(sample_type:carapace_save, .before = notes)


table(SC2004_05$sample_num, useNA = "ifany")
table(SC2004_05$sample_type, useNA = "ifany")
table(SC2004_05$species, useNA = "ifany")
table(SC2004_05$sex, useNA = "ifany")
table(SC2004_05$collection_date, useNA = "ifany")
table(SC2004_05$krill_type, useNA = "ifany")
table(SC2004_05$squid_type, useNA = "ifany")
table(SC2004_05$fish_type, useNA = "ifany")
table(SC2004_05$carapace_save, useNA = "ifany")
table(SC2004_05$fish_type, SC2004_05$squid_type, SC2004_05$krill_type,
      useNA = "ifany")
sum(duplicated(SC2004_05$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2004_05$location) | (SC2004_05$location %in% beaches$location))
all(is.na(SC2004_05$collector) | (SC2004_05$collector %in% observers$observer))
all(is.na(SC2004_05$processor) | (SC2004_05$processor%in% observers$observer))

SC2004_05$location[!(is.na(SC2004_05$location) | (SC2004_05$location %in% beaches$location))]

# diets2004_05_todb <- SC2004_05 %>%
#   left_join(beaches, by = join_by(location)) %>%
#   left_join(tags, by = join_by(species, tag)) %>%
#   select(-c(location, tag, female_id, observer_code)) %>%
#   relocate(species: tag_id, .before = notes)