# Read and clean AFS diet data from 1999/00, to import into database

# pak::pkg_install("us-amlr/tamatoamlr")

library(tidyverse)
library(readxl)
library(here)
#pak::pkg_install("us-amlr/tamatoamlr")
library(tamatoamlr)


#-------------------------------------------------------------------------------
# Read in and combine the scat and enema data

#----------------------------------------------------------
# Scat data
diets.orig <- read_excel(
  here("diets_historical_data", "Fur Seal Diet 1999-00.xls"),
  sheet = "Enema - Scat Contents", skip = 0, 
  col_types = c(rep("text", 4), "date", rep("text", 14), rep("skip", 7))
)

# See which columns have NA date values, to make sure it is as expected (aka 0)
which(is.na(diets.orig$`Date Collectd`))


#-------------------------------------------------------------------------------
# Explore the data, to inform the mutate operations below
sum(duplicated(diets.orig$`Log #`)) == 0

table(diets.orig$`Enema (E), Scat (S), Vomit (V)`, useNA = "ifany")
table(diets.orig$Composition, useNA = "ifany")
table(diets.orig$Krill, useNA = "ifany")
table(diets.orig$Fish, useNA = "ifany")
table(diets.orig$Squid, useNA = "ifany")
table(diets.orig$`Measrbl Krill Present`, useNA = "ifany")
table(diets.orig$`Measrbl Krill Present`, diets.orig$Krill, useNA = "ifany")
table(diets.orig$Other, useNA = "ifany")
table(diets.orig$`Female Tag Number`, useNA = "ifany")

# Check that all month values can all be dropped
vapply(tolower(diets.orig$Month), function(i) which(i == tolower(month.abb)), 1) == 
  month(diets.orig$`Date Collectd`)
# They are not equivalent, but just ignoring the month column

# Check that Composition matches up with Krill/Fish/Squid Yes/No
identical(str_detect(diets.orig$Composition, "Krill"), diets.orig$Krill =="Y")
identical(str_detect(diets.orig$Composition, "Fish"), diets.orig$Fish =="Y")
identical(str_detect(diets.orig$Composition, "Squid"), diets.orig$Squid =="Y")


# Create the processed diets data frame
diets <- diets.orig %>%
  # Select and rename columns for data frame
  select(log_num = `Log #`, sample_type = `Enema (E), Scat (S), Vomit (V)`, 
         location = `Location Collected`, collection_date = `Date Collectd`, 
         krill_type = Krill, fish_type = Fish, squid_type = Squid, 
         tag = `Female Tag Number`, notes = Notes, 
         Composition, Other) %>%
  # Add yellow liquidy context to notes
  mutate(notes = case_when(Composition == "yellow liquidy" ~ "yellow liquidy", 
                           Other == "Amphipods" ~ "Amphipods", 
                           .default = notes)) %>% 
  select(-c(Composition, Other)) %>% 
  # Create new columns, and adjust values in existing columns as necessary
  mutate(season_name = "1999/00",
         sample_num = seq_along(log_num),
         species = "Fur seal",
         sex = "F",
         sample_type = case_when(sample_type == "S" ~ "Scat", 
                                 sample_type == "E" ~ "Enema", 
                                 sample_type == "V" ~ "Vomitus", 
                                 .default = NA_character_), 
         collection_date = as.Date(collection_date), 
         collector = NA_character_,
         processor = NA_character_,
         krill_type = if_else(krill_type == "Y", "Yes", "No"),
         fish_type = if_else(fish_type == "Y", "Yes", "No"),
         squid_type = if_else(squid_type == "Y", "Yes", "No"),
         tag = if_else(tag == "-", NA_character_, 
                       str_pad(tag, width = 3, pad = "0")),
         carapace_save = 0,
         location = if_else(location == "?", NA_character_, location), 
         notes = case_when(
           is.na(log_num) ~ notes,
           !is.na(log_num) & is.na(notes) ~
             paste0("Original log number: ", log_num),
           !is.na(log_num) & !is.na(notes) ~
             paste0(notes, "; ", "Original log number: ", log_num))) %>%
  mutate_location() #%>%
#   select(season_name, sample_num, species, sex, sample_type, location, 
#          collection_date, collector, process_date, processor, 
#          krill_type, fish_type, squid_type, tag, carapace_save, notes)


# Confirm that all locations, observers, and tags are in database tables
beaches <- read.csv(here("reference_tables/beaches.csv")) %>%
  select(beach_id = ID, location = name)
observers <- read.csv(here("reference_tables/observers.csv"))
tags <- read.csv(here("reference_tables/tags.csv")) %>%
  filter(tag_species != "Fur seal" | tag_type != "U-tag") %>%
  select(tag_id = ID, tag, species = tag_species)

all(is.na(diets$location) | (diets$location %in% beaches$location))
all(is.na(diets$collector) | (diets$collector%in% observers$observer))
all(is.na(diets$processor) | (diets$processor%in% observers$observer))
all(is.na(diets$tag) | (diets$tag%in% tags$tag))

# Report the still-invalid locations
table(diets$location[!(is.na(diets$location) | 
                         (diets$location %in% beaches$location))])


#-------------------------------------------------------------------------------
# Get ID columns, ready everything to go into the db
diets.todb <- diets %>%
  left_join(beaches, by = join_by(location)) %>%
  left_join(tags, by = join_by(species, tag)) %>%
  select(-c(location, tag, log_num, season_name))

#-------------------------------------------------------------------------------
# Prepare krill carapace data
kc.orig <- read_excel(
  here("diets_historical_data", "Fur Seal Diet 1999-00.xls"),
  sheet = "Krill Measurments", skip = 13, 
  col_types = c(rep("text", 3), "date", rep("text", 14), rep("skip", 5))
)

# # TODO: need to sanity check where log num/other info doesn't match up
# dplyr::setdiff(kc.orig$`Log Desigination`, diets.orig$`Log #`)
# dplyr::setdiff(diets.orig$`Log #`, kc.orig$`Log Desigination`)
# 
# y <- kc.orig %>%
#   select(log_num = `Log Desigination`, tag = `Tag # if known`,
#          collection_date = `Date Collected`, sample_type = `Enema or Scat`,
#          location = `Location (Beach)`) %>%
#   filter(!is.na(log_num)) %>% 
#   mutate(location = if_else(location == "?", NA_character_, location)) %>% 
#   group_by(log_num, sample_type, location, collection_date, tag) %>%
#   summarise(n_rows = n())
# sum(duplicated(y$log_num))
# diets.tmp <- diets.orig %>%
#   select(log_num = `Log #`, sample_type = `Enema (E), Scat (S), Vomit (V)`,
#          location = `Location Collected`, collection_date = `Date Collectd`,
#          tag = `Female Tag Number`) %>%
#   mutate(sample_type = case_when(sample_type == "S" ~ "Scat",
#                                  sample_type == "E" ~ "Enema",
#                                  sample_type == "V" ~ "Vomitus",
#                                  .default = NA_character_),
#          tag = if_else(tag == "-", NA_character_, tag), 
#          location = if_else(location == "?", NA_character_, location)) %>%
#   arrange(log_num)
# 
# d <- inner_join(y, diets.tmp)
# y.logg <- dplyr::setdiff(y$log_num, d$log_num)
# y %>% filter(log_num %in% y.logg); diets.tmp %>% filter(log_num %in% y.logg)

# TODO: get diets_id after diets df is imported into the database

(kc.orig %>% filter(duplicated(`Original Order`)))
diets.kc <- kc.orig %>% 
  select(log_num = `Log Desigination`, 
         carapace_length_mm = `RC Length mm`, 
         carapace_width_mm = `RC Width mm`, 
         notes = `Krill length measrd`, 
         `Original Order`) %>% 
  mutate(carapace_length_mm = as.numeric(carapace_length_mm), 
         carapace_width_mm = as.numeric(carapace_width_mm)) %>% 
  filter(!is.na(log_num), 
         !is.na(carapace_length_mm), carapace_length_mm != 0, 
         !is.na(carapace_width_mm), carapace_width_mm != 0) %>% 
  # tableNA(diets.kc$notes)
  arrange(as.numeric(`Original Order`)) %>% 
  mutate(carapace_num = seq_along(carapace_length_mm), 
         .by = "log_num", .before = carapace_length_mm)


#-------------------------------------------------------------------------------
# Prepare otolith data

# TODO: get diets_id after diets df is imported into the database

diets.nonkrill <- diets.orig %>%
  select(log_num = `Log #`, 
         e_ant = `Otoliths E. ant.`, 
         e_carl = `Otoliths E. carl.`, 
         g_nich = `Otoliths G. nich`, 
         squid_beaks = `Cephalopod Beaks`) %>% 
  pivot_longer(cols = e_ant:squid_beaks, 
               names_to = "species", values_to = "amount") %>% 
  mutate(side = "Unk", 
         erosion = NA_integer_, 
         species = case_when(
           species == "e_ant" ~ "E. antarctica", 
           species == "e_carl" ~ "E. carlsbergi", 
           species == "g_nich" ~ "G. nicholsi", 
           species == "squid_beaks" ~ "Squid beaks", 
           .default = NA_character_
         ))
