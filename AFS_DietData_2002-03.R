library(here)
library(readxl)
library(tidyverse)
read_excel(path = here("diets_historical_data", "Fur Seal Diet 2002-03.xls"), 
           sheet = "Sample Contents", skip = 2, 
           range = "A3:AA106")

SC2002_03_ORIG <- read_excel(
  path = here("diets_historical_data", "Fur Seal Diet 2002-03.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A3:AA106", 
  col_types = c("numeric", "numeric", "date",
                "text", "text", "date",
                rep("text", 4), rep("numeric", 16), 
                "text")
)

SC2002_03 <- SC2002_03_ORIG %>% 
  rename( 
    week_num = `Week #`, sample_num = `Sample #`, collection_date = Date...3,
    location = Loc, female_id = `Female ID`, 
    process_date = Date...6,
    krill_type = `Krill Obs.`, fish_type = Fish,
    squid_type = Squid,
    E.antarctica_Presence = E.antarctica,
    E.carlsbergi_Presence = `E. carlsbergi`,
    G.nicholsi_Presence = `G. nicholsi`,
    B.picta_Presence = `B. picta`,
    krill_carapaces_measured = Measured,
    E.antarctica_left_Otolith_Count = left...16, 
    E.antarctica_right_Otolith_Count = right...17,
    E.carlsbergi_left_Otolith_Count = left...18, 
    E.carlsbergi_right_Otolith_Count = right...19,
    G.nicholsi_left_Otolith_Count = left...20, 
    G.nicholsi_right_Otolith_Count = right...21, 
    G.sp._eroded_left_Otoliths = left...22, 
    G.sp._eroded_right_Otoliths = right...23,
    unidentified_otoliths_all = all,
    squid_beaks_total_counts = Beaks,
    otolith_slides = ...26, 
    notes = Notes) %>% 
  select(sample_num: krill_type, fish_type, squid_type, notes) %>%
  mutate(sample_type = "scat", species = "Fur seal", sex = "F",
         observer_code = NA,
         krill_type = if_else(krill_type == "Yes"| krill_type == "Y"
                              , "Yes", "No"),
         fish_type = if_else(fish_type == "Y", "Yes", "No"), 
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         female_id = if_else(female_id == "no id", NA, female_id),
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date),
         female_id = str_sub(female_id, 1, 3),
         # notes = if_else(female_id == 103,
         #                 "female_id originally labeled as 103/313 might have
         #                 been a retagging", notes), 
         #TODO (This is overriding previos notes cell info)
         processor = NA_character_, #str_sub(observer_code, 1, 3), 
         collector = NA_character_,
         carapace_save = case_when(
           is.na(notes) ~ 0, 
           str_detect(tolower(notes), "carapaces saved") ~ 1, 
           .default = 0)) %>%
  select(sample_num: krill_type, fish_type, squid_type,
         notes: carapace_save) %>% 
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

#103/313 str_sub() to just include 103 femaleID