library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2001-02.xls"), 
           sheet = "Sample Log", skip = 2, 
           range = "A15:U130")

SC2001_02_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2001-02.xls"), 
  sheet = "Sample Log", skip = 2, 
  range = "A15:U130", 
  col_types = c("text", "text", "date",
                "text", "text", "date",
                rep("text", 3), rep("numeric", 11), 
                "text")
)

SC2001_02 <- SC2001_02_ORIG %>% 
  rename(week_num = `Week #`, sample_num = `Sample #`, collection_date = Date...3,
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
         squid_beaks_total_counts = Beaks,
         notes = Obs.) %>%
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         observer_code = NA,
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"), 
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         location = if_else(location == "\"-\"", NA, location),
         female_id = if_else(female_id == "\"-\"", NA, female_id),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date), 
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         processor = str_sub(observer_code, 1, 3), 
         collector = NA_character_,
         carapace_save = "0") %>%
  filter(!(week_num %in% c("PTT-Scat", "Enema"))) %>% 
  filter(!(sample_num %in% c("V1", "V2", "V3", "V4", "V5", "V6"))) %>% 
  mutate(sample_num = as.numeric(sample_num)) %>%
  select(sample_num: squid_type,
         notes: carapace_save) %>% 
  mutate_location()


table(SC2001_02$sample_num, useNA = "ifany")
table(SC2001_02$sample_type, useNA = "ifany")
table(SC2001_02$species, useNA = "ifany")
table(SC2001_02$sex, useNA = "ifany")
table(SC2001_02$collection_date, useNA = "ifany")
table(SC2001_02$krill_type, useNA = "ifany")
table(SC2001_02$squid_type, useNA = "ifany")
table(SC2001_02$fish_type, useNA = "ifany")
table(SC2001_02$carapace_save, useNA = "ifany")
table(SC2001_02$fish_type, SC2001_02$squid_type, SC2001_02$krill_type,
      useNA = "ifany")
sum(duplicated(SC2001_02$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2001_02$location) | (SC2001_02$location %in% beaches$location))
all(is.na(SC2001_02$collector) | (SC2001_02$collector %in% observers$observer))
all(is.na(SC2001_02$processor) | (SC2001_02$processor%in% observers$observer))

SC2001_02$location[!(is.na(SC2001_02$location) | (SC2001_02$location %in% beaches$location))]

diets2001_02_todb <- SC2001_02 %>%
  left_join(beaches, by = join_by(location)) %>%
  left_join(tags, by = join_by(species, tag)) %>%
  select(-c(location, tag, female_id, observer_code)) %>%
  relocate(species: tag_id, sample_type, .before = notes)
