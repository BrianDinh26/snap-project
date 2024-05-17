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

teachers_df <- teachers_df |> 
  filter(receiver != "University of British Columbia") |> 
  filter(receiver != "Temple University") |> 
  filter(receiver != "Various") |> 
  filter(receiver != "Not specified") |> 
  filter(receiver != "Not mentioned") |> 
  filter(receiver != "Not available") |> 
  filter(receiver != "Juilliard Preparatory Division") |> 
  filter(receiver != "N/A") |> 
  filter(receiver != "Paris Conservatoire") |> 
  filter(receiver != "Central Conservatory in Beijing, China") |> 
  filter(receiver != "Paris Conservatoire") |> 
  filter(receiver != "Juilliard School") |> 
  filter(receiver != "Juilliard School of Music") |> 
  filter(receiver != "Longy School of Music") |> 
  filter(receiver != "unknown") |> 
  filter(receiver != "Unknown") |> 
  filter(receiver != "Jaap van Zweden began his conducting career almost 20 years later, in 1996.") |> 
  filter(receiver != "Jaap van Zweden was appointed the youngest-ever concertmaster of Amsterdams Royal Concertgebouw Orchestra at age 19.")


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


schools_df <- schools_df |> 
  filter(receiver != "Aspen Music Festival") |> 
  filter(receiver != "Aspen Music Festival and School") |>
  filter(receiver != "Born in Amsterdam.") |> 
  mutate(
    across(receiver, ~ gsub("Boston Conservatory University", "Boston Conservatory", .)),
    across(receiver, ~ gsub("New England Conservatory of Music", "New England Conservatory", .)),
    across(receiver, ~ gsub("Indiana Unviersity School of Music", "Indiana University", .))
  )

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
  filter(receiver != "Unknown") |> 
  select(
    name,
    receiver,
    type,
    instrument,
    when_began_current
  )

save(orchestra_network_cleanish, file = here("data/orchestra_network_cleanish.rda"))

orchestra_type <- orchestra_network_cleanish |> 
  select(type)

orchestra_instrument <- orchestra_network_cleanish |> 
  select(instrument)

orchestra_begin <- orchestra_network_cleanish |> 
  select(when_began_current)


save(orchestra_type, orchestra_instrument, orchestra_begin, file = here("data/orchestra_vertexes.rda"))

# some other attempt
orch_lim <- bind_rows(
  teachers_df) |> 
  select(name, receiver)



