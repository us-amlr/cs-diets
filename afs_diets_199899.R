# Read and clean AFS diet data from 1998/99, to import into database

# pak::pkg_install("us-amlr/tamatoamlr")

library(tidyverse)
library(readxl)
library(here)
library(tamatoamlr)


#-------------------------------------------------------------------------------
# Read in and combine the scat and enema data

#----------------------------------------------------------
# Scat data
scat.orig <- read_excel(
  here("diets_historical_data", "Fur Seal Diet 1998-99.xls"),
  sheet = "Scat Log", skip = 2, 
  col_types = c(rep("text", 5), "date", "date", rep("text", 8))
)

# See which columns have NA date values, to make sure it is as expected
which(is.na(scat.orig$`Date Collected`))
which(is.na(scat.orig$`Date Processed`))

# Filter rows to remove non-data rows
#   We do this now, so as to not have garbage values when exploring data
# Filter for where collection date is not null, 
#   to remove rows w/out data from the original file
# NOTE: done below in 'Combine data frames' section

#----------------------------------------------------------
# Enema data
enema.orig <- read_excel(
  here("diets_historical_data", "Fur Seal Diet 1998-99.xls"),
  sheet = "Enema Log", skip = 2,
  col_types = c(rep("text", 4), "date", "date", rep("text", 8))
)

# See which columns have NA date values, to make sure it is as expected
which(is.na(enema.orig$`Date Collected`))
which(is.na(enema.orig$`Date Processed`))

# Filter rows to remove non-data rows
#   We do this now, so as to not have garbage values when exploring data
# Filter for where collection date is not null,
#   to remove rows w/out data from the original file
# NOTE: done below in 'Combine data frames' section

#----------------------------------------------------------
# Combine data frames

# First, we need to make it so that like columns have the same names
# Also, remove columns with NA date collected values
# Also, do scat/enema-specific processing
x <- scat.orig %>% 
  select(-`...2`) %>% 
  rename(`Sub-samples...8` = `Sub-samples...9`, 
         `Sub-samples...10` = `Sub-samples...11`) %>% 
  filter(!is.na(`Date Collected`)) %>% 
  mutate(sample_type = "scat")

y <- enema.orig %>% 
  filter(!is.na(`Date Collected`)) %>% 
  # We also add sample numbers here, because it's easiest to do with single 
  #   data frame. Make sample numbers increment from 103 (max in scat)
  #   Use the same format as the scat df for easy processing
  mutate(Sample = paste0("99-", 104:137), 
         sample_type = "enema")

# Sanity check
identical(names(x), names(y))

# Combine data frame
diets.orig <- bind_rows(x, y)

#-------------------------------------------------------------------------------
# Explore the data, to inform the mutate operations below
table(diets.orig$Animal, useNA = "ifany")
table(diets.orig$Sex, useNA = "ifany")
table(diets.orig$`Krill?`, useNA = "ifany")
table(diets.orig$`Beaks?`, useNA = "ifany")
table(diets.orig$`Otoliths?`, useNA = "ifany")
table(diets.orig$`Parts?`, useNA = "ifany")
table(diets.orig$`Otoliths?`, diets.orig$`Parts?`, useNA = "ifany")
#FS=FUR SEAL, WEDDELL, NUMBERS=TAGGED SEALS, YRL=YEARLING, 
#UNK=KNOWN SEAL, BUT UNKNOWN TAG NUM
# Check for no duplicate sample numbers
sum(duplicated(diets.orig$Sample)) == 0


# Create the processed diets data frame
diets <- diets.orig %>% 
  # Select and rename columns for data frame
  select(Sample, animal = Animal, sex = Sex, location = Location, 
         collection_date = `Date Collected`, process_date = `Date Processed`, 
         `Otoliths?`:`Krill?`, sample_type, 
         notes = Comments, tentative_otolith_id = `Tentative Otolith ID`) %>% 
  # Filter out 'Yrl' data, because we don't to store this
  filter(animal != "Yrl") %>%
  # Filter out row with 'bad enema' comment
  filter(notes != "Bad Enema" | is.na(notes)) %>% 
  # Create new columns, and adjust values in existing columns as necessary
  mutate(season_name = "1998/99",
         # Use the -## as the sample number
         sample_num = as.integer(str_sub(Sample, 4, -1)), 
         species = if_else(animal == "Weddell", "Weddell seal", "Fur seal"), 
         sex = if_else(sex == "F?", NA_character_, toupper(sex)), 
         collector = NA_character_, 
         processor = NA_character_,
         krill_type = if_else(`Krill?` == "Y", "Yes", "No"),
         fish_type = case_when(
           `Otoliths?` %in% c("Y", "y", "100's") ~ "Yes", 
           `Parts?` == "Y" ~ "Yes", 
           .default = "No"
         ),
         squid_type = if_else(`Beaks?` == "Y", "Yes", "No"), 
         tag = str_pad(as.numeric(animal), width = 3, pad = "0"), 
         carapace_save = 0, 
         notes = case_when(
           is.na(tentative_otolith_id) ~ notes, 
           !is.na(tentative_otolith_id) & is.na(notes) ~ 
             paste0("Tentative otolith ID: ", tentative_otolith_id), 
           !is.na(tentative_otolith_id) & !is.na(notes) ~ 
             paste0(notes, "; ", "Tentative otolith ID: ", tentative_otolith_id)
         )) %>% 
  mutate_location() %>% 
  select(season_name, sample_num, species, sex, sample_type, location, 
         collection_date, collector, process_date, processor, 
         krill_type, fish_type, squid_type, tag, carapace_save, notes)


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

# # Report the still-invalid locations 
# table(diets$location[!(is.na(diets$location) | (diets$location %in% beaches$name))])


#-------------------------------------------------------------------------------
# Get ID columns, ready everything to go into the db
diets.todb <- diets %>% 
  left_join(beaches, by = join_by(location)) %>% 
  left_join(tags, by = join_by(species, tag)) %>% 
  select(-c(location, tag))

#-------------------------------------------------------------------------------
# NOTE: no krill carapace length or otolith count data for 1998/99
