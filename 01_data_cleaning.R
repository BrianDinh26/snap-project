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
    across(schools, ~ gsub("\\; ", ";", .)),
    across(competitions, ~ gsub("\\; ", ";", .)),
    across(festivals, ~ gsub("\\; ", ";", .)),
    across(chamber_groups, ~ gsub("\\; ", ";", .)),
    across(chamber_group_partners, ~ gsub("\\; ", ";", .)),
  ) |> 
  mutate_all(~replace_na(., "unknown"))

# missingness check
gg_miss_var(orchestra)

# teachers
teachers_df <- data.frame(name = character(),
                          instrument = character(),
                          when_began_current = character(),
                          receiver = character(),
                          type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  teachers_list <- unlist(strsplit(orchestra$teachers[i], ";"))
  label = "teachers"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = teachers_list,
                        type = label)
  
  teachers_df <- rbind(teachers_df, temp_df)
}

# schools
schools_df <- data.frame(name = character(),
                          instrument = character(),
                          when_began_current = character(),
                          receiver = character(),
                          type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  schools_list <- unlist(strsplit(orchestra$schools[i], ";"))
  label = "schools"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = schools_list,
                        type = label)
  
  schools_df <- rbind(schools_df, temp_df)
}

# competitions
competitions_df <- data.frame(name = character(),
                         instrument = character(),
                         when_began_current = character(),
                         receiver = character(),
                         type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  competitions_list <- unlist(strsplit(orchestra$competitions[i], ";"))
  label = "competitions"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = competitions_list,
                        type = label)
  
  competitions_df <- rbind(competitions_df, temp_df)
}

# festivals
festivals_df <- data.frame(name = character(),
                              instrument = character(),
                              when_began_current = character(),
                              receiver = character(),
                              type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  festivals_list <- unlist(strsplit(orchestra$festivals[i], ";"))
  label = "festivals"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = festivals_list,
                        type = label)
  
  festivals_df <- rbind(festivals_df, temp_df)
}

# chamber groups
chamber_groups_df <- data.frame(name = character(),
                           instrument = character(),
                           when_began_current = character(),
                           receiver = character(),
                           type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  chamber_groups_list <- unlist(strsplit(orchestra$chamber_groups[i], ";"))
  label = "chamber_groups"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = chamber_groups_list,
                        type = label)
  
  chamber_groups_df <- rbind(chamber_groups_df, temp_df)
}

# chamber group partners
chamber_group_partners_df <- data.frame(name = character(),
                                instrument = character(),
                                when_began_current = character(),
                                receiver = character(),
                                type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  
  chamber_group_partners_list <- unlist(strsplit(orchestra$chamber_group_partners[i], ";"))
  label = "chamber_group_partners"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = chamber_group_partners_list,
                        type = label)
  
  chamber_group_partners_df <- rbind(chamber_group_partners_df, temp_df)
}

# bind rows together and filter out unknowns
orchestra_network_cleanish <- bind_rows(
  teachers_df,
  schools_df,
  festivals_df,
  competitions_df,
  chamber_groups_df,
  chamber_group_partners_df
) |> 
  filter(receiver != "unknown") |> 
  filter(receiver != "Unknown")

save(orchestra_network_cleanish, file = here("data/orchestra_network_cleanish.rda"))


