library(here)
library(readxl)
library(tidyverse)
library(tamatoamlr)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2008-09.xls"), 
           sheet = "Sample Contents_updated", skip = 2, 
           range = "A14:AE114")

SC2008_09_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2008-09.xls"), 
  sheet = "Sample Contents_updated", skip = 2, 
  range = ("A14:AE114"),
  col_types = c("date", "numeric", "numeric", "date", 
                "text", "text",
                "date", rep("text", 4), rep("numeric", 19), "text")
)

SC2008_09 <- SC2008_09_ORIG %>% 
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
         Notolepis_coatsi_left_Otolith_Count = left...19,
         Notolepis_coatsi_right_Otolith_Count = right...20, 
         Pleuragramma_antarcticum_left_Otolith_Count = left...21,
         Pleuragramma_antarcticum_right_Otolith_Count = right...22,
         G.sp._eroded_left_Otoliths = ...23,
         G.sp._eroded_right_Otoliths = ...24, 
         E.ant._Identity_eroded = eroded, unidentified_otoliths_all = all,
         squid_dorsal_beak_count = dorsal, squid_ventral_beak_count = ventral,
         otolith_slides = ...29, total_otoliths = Otoliths, 
         notes = ...31) %>% 
  select(sample_num: squid_type, notes) %>% 
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"),
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date),
         female_id = if_else(female_id == "n/a", NA, female_id), 
         processor = NA_character_, #str_sub(observer_code, 1, 3), 
         collector = NA_character_, 
         carapace_save = case_when(
           is.na(notes) ~ 0, 
           str_detect(tolower(notes), "carapaces saved") ~ 1, 
           .default = 0)) %>%
  select(sample_num: squid_type, collector, notes: carapace_save) %>% 
  mutate_location()


table(SC2008_09$sample_num, useNA = "ifany")
table(SC2008_09$sample_type, useNA = "ifany")
table(SC2008_09$species, useNA = "ifany")
table(SC2008_09$sex, useNA = "ifany")
table(SC2008_09$collection_date, useNA = "ifany")
table(SC2008_09$fish_type, useNA = "ifany")
table(SC2008_09$squid_type, useNA = "ifany")
table(SC2008_09$krill_type, useNA = "ifany")
table(SC2008_09$carapace_save, useNA = "ifany")


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2008_09$location) | (SC2008_09$location %in% beaches$location))
all(is.na(SC2008_09$collector) | (SC2008_09$collector %in% observers$observer))
all(is.na(SC2008_09$processor) | (SC2008_09$processor%in% observers$observer))

#TODO: Redownload tamatoa() for updated mutate_locations()

# diets2008_09_todb <- SC2008_09 %>%
#   left_join(beaches, by = join_by(location)) %>%
#   left_join(tags, by = join_by(species, tag)) %>%
#   select(-c(location, tag, female_id, observer_code)) %>% 
#   relocate(species: tag_id, .before = notes)



#Notes--------------------------------------------------------------------------
#Reading FALSE statement for location column argument
#Running lines SC2008_09$location and beaches$location lists the locations in
#these columns per cell
#is.na(SC2008_09$location) | (SC2008_09$location %in% beaches$location)
#a vector on where a TRUE statement appears or where both sides of argument are
#FALSE like those in lines 79-92 because the locations of SC2008_09$location were
#not found in beaches$location
#This line  SC2008_09$location[!(is.na(SC2008_09$location) | (SC2008_09$location %in% beaches$location))]
#gives the name output of the locations that don't appear in df in this case 
# being "Mod Fl" or wrapping this all in a table function to give a count of missing locations