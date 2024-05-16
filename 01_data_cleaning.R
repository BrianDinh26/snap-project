# SNAP Project, Initial Set-Up

# load packages ----
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(statnet)

# load in data ----
orchestra <- read_csv(here("data/output.csv")) |> 
  janitor::clean_names() |> 
  mutate(
    instrument = tolower(instrument),
    across(instrument, ~ gsub(c("violinist"), "violin", .)),
    across(instrument, ~ gsub(c("violist"), "viola", .)),
    across(instrument, ~ gsub(c("bassist"), "bass", .)),
    across(instrument, ~ gsub(c("cellist"), "cello", .)),
    across(instrument, ~ gsub(c("contrabassoonist"), "contrabassoon", .)),
    across(instrument, ~ gsub(c("english hornist"), "english horn", .)),
    across(instrument, ~ gsub(c("flutist"), "flute", .)),
    across(instrument, ~ gsub(c("percussionist"), "percussion", .)),
    across(instrument, ~ gsub(c("pianist"), "piano", .)),
    across(teachers, ~ gsub("\\; ", ";", .)),
  ) |> 
  mutate_all(~replace_na(., "unknown"))

gg_miss_var(orchestra)

#expanded_df <- data.frame(name = character(), 
#                          instrument = character(),
#                          teachers = character(), 
#                          schools = character(),
#                          competitions = character(),
#                          festivals = character(),
#                         chamber_groups = character(),
#                          chamber_group_partners = character(),
#                          when_began_current = character(),
#                          stringsAsFactors = FALSE)

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  # Create a data frame with each item as a separate row (unlisted ones have to be combined)
  # may need help with writing code to label the connection as to teacher, school, chamber groups, or partners
  teachers_list <- unlist(strsplit(orchestra$teachers[i], ";"))
  schools_list <- unlist(strsplit(orchestra$schools[i], ";"))
  competitions_list <- unlist(strsplit(orchestra$competitions[i], ";"))
  festivals_list <- unlist(strsplit(orchestra$festivals[i], ";"))
  chamber_groups_list <- unlist(strsplit(orchestra$chamber_groups[i], ";"))
  chamber_group_partners_list <- unlist(strsplit(orchestra$chamber_group_partners[i], ";"))
  
  # create df.
#  temp_df <- data.frame(name = name_id, 
#                        instrument = instrument_id,
#                        teachers = teachers_list, 
#                        schools = schools_list,
#                        competitions = competitions_list,
#                        festivals = festivals_list,
#                        chamber_groups = chamber_groups_list,
#                        chamber_group_partners = chamber_group_partners_list,
#                        when_began_current = character(),
#                        stringsAsFactors = FALSE)
  
  # Add the temp_df to the expanded_df
  expanded_df <- rbind(expanded_df, temp_df)
}

