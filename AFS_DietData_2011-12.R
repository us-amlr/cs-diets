library(here)
library(readxl)
library(dplyr)
library(stringr)
#pak::pkg_install("us-amlr/tamatoamlr")
library(tamatoamlr)

SC2011_12_ORIG <- read_excel(
  path = here("diets_historical_data", 
              "Fur Seal Diet 2011-12.xls"), 
  sheet = "Sample Contents", skip = 2, 
  range = "A14:AC105", 
  col_types = c("date", "numeric", 
                "text", "date",
                "text", "numeric", "date", 
                "text", "text", "text", "text", 
                rep("numeric", 17), "text")
)

# which(is.na(SC2011_12_ORIG$`Date...4`))
# which(is.na(SC2011_12_ORIG$`Date...7`))


SC2011_12 <- SC2011_12_ORIG %>% 
  rename(start_date = 1, 
         week_num = `#...2`, sample_num = `#...3`, collection_date = Date...4,
         location = Loc, female_id = ID, 
         process_date = Date...7, observer_code = Obs., 
         krill_type = Krill, fish_type = Fish,
         squid_type = Squid, krill_carapaces_measured = Measured, 
         E.antarctica_left_Otolith_Count = left...13, 
         E.antarctica_right_Otolith_Count = right...14,
         E.carlsbergi_left_Otolith_Count = left...15, 
         E.carlsbergi_right_Otolith_Count = right...16,
         G.nicholsi_left_Otolith_Count = left...17, 
         G.nicholsi_right_Otolith_Count = right...18, 
         Notolepis_coatsi = `Notolepis coatsi`,
         G.sp._eroded = eroded...20,
         E.carlbergi_eroded = eroded...21, 
         E.antarctica._eroded = eroded...22, unidentified_otoliths_all = all,
         squid_dorsal_beak_count = dorsal, squid_ventral_beak_count = ventral,
         otolith_slides_start = ...26, otolith_slides_end = ...27, total_otoliths 
         = Otoliths, notes = Comments) %>% 
  select(sample_num, collection_date: squid_type, notes) %>%
  mutate(species = "Fur seal", sex = "F",
         krill_type = if_else(krill_type == "Y", "Yes", "No"), 
         fish_type = if_else(fish_type == "Y", "Yes", "No"), 
         squid_type = if_else(squid_type == "Y", "Yes", "No"), 
         collection_date = as.Date(collection_date), 
         process_date = as.Date(process_date), 
         processor = NA_character_, #str_sub(observer_code, 1, 3), 
         sample_num = if_else(sample_num == "20A", "91", sample_num),
         sample_num = as.numeric(sample_num),
         notes = if_else(sample_num == 91, 
                         paste0(notes, "; sample_num originally labeled as 20A"), 
                         notes),
         sample_type = if_else(sample_num == 91, "vomitus", "scat"),
         tag = str_pad(as.numeric(female_id), width = 3, pad = "0", side = "left"),
         collector = NA_character_,
         carapace_save = case_when(
           is.na(notes) ~ 0, 
           str_detect(tolower(notes), "carapaces saved") ~ 1, 
           .default = 0), female_id = as.character(female_id)) %>%
  select(sample_num: squid_type, collector, notes: carapace_save) %>%
  mutate_location()

table(SC2011_12$sample_num, useNA = "ifany")
table(SC2011_12$sample_type, useNA = "ifany")
table(SC2011_12$sex, useNA = "ifany")
table(SC2011_12$collection_date, useNA = "ifany")
table(SC2011_12$krill_type, useNA = "ifany")
table(SC2011_12$squid_type, useNA = "ifany")
table(SC2011_12$fish_type, SC2011_12$squid_type, SC2011_12$krill_type,
useNA = "ifany")
table(SC2011_12$carapace_save, useNA = "ifany")
sum(duplicated(SC2011_12$sample_num)) == 0


beaches <- read.csv(here("reference_tables/beaches.csv")) %>% 
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>% 
  filter(tag_species == "Fur seal", tag_type != "U-tag") %>% 
  select(tag_id = ID, tag, species = tag_species)

all(is.na(SC2011_12$location) | (SC2011_12$location %in% beaches$location))
all(is.na(SC2011_12$collector) | (SC2011_12$collector %in% observers$observer))
all(is.na(SC2011_12$processor) | (SC2011_12$processor%in% observers$observer))
 
 
# all(is.na(SC2011_12$tag) | (SC2011_12$tag%in% tags$tag))
# table(SC2011_12$location[!(is.na(SC2011_12$location) | 
#                              (SC2011_12$location %in% beaches$name))])


diets2011_12_todb <- SC2011_12 %>%
  left_join(beaches, by = join_by(location)) %>%
  left_join(tags, by = join_by(species, tag)) %>%
  select(-c(location, tag, female_id, observer_code)) %>% 
  relocate(species: tag_id, .before = notes)


# TODO: reinstall Tamatoa pakage that's been updated for locations
# TODO: there should be no NAs for columns sample_num, sample_type, species, sex
# collection_date, {3x} _type, carapace_save.
# TODO: remove columns not found in clean data frame
# TODO: filter through carapace saved notes column





#Renaming columns/Notes------
#Reassign col_types in original dataframe before modifying 
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

#All(is.na) | operator----
#all(is.na(SC2011_12$location) | (SC2011_12$location %in% beaches$location))
#two split commands; first half is asking if all(is.na(SC2011_12$location)) 
#which was FALSE because all of them are NON-NAs values. The second half
#is asking if the location column values in the beaches data frame are the same
#locations found in the SC2011_12 data frame.
# TRUE/FALSE Statements
# T | T = TRUE
# T | F = TRUE
# F | F = FALSE
# T & T = TRUE
# T & F = FALSE
# F & F = FALSE
#(If a TRUE statement if present anywhere in the OR operator comparison then its
# output will = TRUE)
#creating diets data frame----

# diets <- SC2011_12 %>% 
#   select(Sample_Num, Collection_Date, Comments) %>% 
#   mutate(krill_presence == if_else(krill_presence = "Y", "Yes", "No")) 

# if_else function----
# sample_num = if_else(sample_num == "20A", "91", sample_num)
#if the present value is "20A" then the "yes" statement will be the value you
#want it to change to and the second statement which would be the "no" turns 
#everything else to this value by default unless specifying it to leave it as 
#the original column values