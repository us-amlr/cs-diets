library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2000-01.xls"), 
           sheet = "Sample Log", skip = 2, 
           range = "A15:Y119")

SC2000_01_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2000-01.xls"), 
  sheet = "Sample Log", skip = 2, 
  range = "A15:Y119", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 3), rep("numeric", 12), 
                rep("text", 4))
)

SC2000_01 <- SC2000_01_ORIG %>% 
  rename(week_num = `#...1`, sample_num = `#...2`, collection_date = Date...3,
         location = Loc, female_id = ID, 
         process_date = Date...6, krill_type = Krill, fish_type = Fish,
         squid_type = Squid, krill_carapaces_measured = Measured,
         E.antarctica_left_Otolith_Count = left...11, 
         E.antarctica_right_Otolith_Count = right...12,
         E.carlsbergi_left_Otolith_Count = left...13, 
         E.carlsbergi_right_Otolith_Count = right...14,
         G.nicholsi_left_Otolith_Count = left...15, 
         G.nicholsi_right_Otolith_Count = right...16,
         G.sp._eroded_left_Otoliths = left...17, 
         G.sp._eroded_right_Otoliths = right...18,
         unidentified_otoliths_all = Unid., 
         squid_dorsal_beak_count = upper, squid_ventral_beak_count = lower,
         sample_type = Type, notes = Obs.) %>% 
  mutate(species = "Fur seal", sex = "F",
         observer_code = NA,
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"), 
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         female_id = if_else(female_id == "-", NA, female_id),
         sample_num = if_else(sample_num == 47 & location == "Cach-e", 
                         105, sample_num),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date), 
         processor = str_sub(observer_code, 1, 3), 
         collector = NA_character_,
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         carapace_save = "0") %>%
  select(sample_num: squid_type,
         notes, sample_type, species: carapace_save) %>% 
  mutate_location()


table(SC2000_01$sample_num, useNA = "ifany")
table(SC2000_01$sample_type, useNA = "ifany")
table(SC2000_01$species, useNA = "ifany")
table(SC2000_01$sex, useNA = "ifany")
table(SC2000_01$collection_date, useNA = "ifany")
table(SC2000_01$krill_type, useNA = "ifany")
table(SC2000_01$squid_type, useNA = "ifany")
table(SC2000_01$fish_type, useNA = "ifany")
table(SC2000_01$carapace_save, useNA = "ifany")
table(SC2000_01$fish_type, SC2000_01$squid_type, SC2000_01$krill_type,
      useNA = "ifany")
sum(duplicated(SC2000_01$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2000_01$location) | (SC2000_01$location %in% beaches$location))
all(is.na(SC2000_01$collector) | (SC2000_01$collector %in% observers$observer))
all(is.na(SC2000_01$processor) | (SC2000_01$processor%in% observers$observer))

SC2000_01$location[!(is.na(SC2000_01$location) | (SC2000_01$location %in% beaches$location))]

diets2000_01_todb <- SC2000_01 %>%
  left_join(beaches, by = join_by(location)) %>%
  left_join(tags, by = join_by(species, tag)) %>%
  select(-c(location, tag, female_id, observer_code)) %>%
  relocate(species: tag_id, sample_type, .before = notes)
