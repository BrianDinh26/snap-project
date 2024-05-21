# Question 3:  What is the probability of an edge forming between a particular student and orchestra?
# Probably have to change this question to be more ERGM-oriented.
# Other ideas for question #:
# Test Hypothesis: if you go to a "top" school you’re more likely to participate in one of the top three festivals 
# Award/festival participants are more likely to be in major orchestras than those who are not award/festival participants

# gonna try the "top" school and festival performance
# if else?

# load packages ----
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(statnet)

# load in data ----
load(here("data/orchestra.rda"))

# schools
schools_df <- data.frame(name = character(),
                         instrument = character(),
                         when_began_current = character(),
                         receiver = character(),
                         festival = character(),
                         type = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  festival_id <- orchestra$festivals[i]
  
  schools_list <- unlist(strsplit(orchestra$schools[i], ";"))
  label = "schools"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        receiver = schools_list,
                        festival = festival_id,
                        type = label)
  
  schools_df <- rbind(schools_df, temp_df)
}

schools_df_clean <- schools_df |> 
  filter(receiver != "Aspen Music Festival") |> 
  filter(receiver != "Aspen Music Festival and School") |>
  filter(receiver != "Born in Amsterdam.") |> 
  filter(receiver != "Yale Philharmonia") |> 
  filter(receiver != "unknown") |> 
  filter(receiver != "Unknown") |> 
  filter(receiver != "The Philadelphia Orchestra") |> 
  filter(receiver != "Los Angeles Philharmonic") |> 
  filter(receiver != "Juilliard Pre-College Division") |> 
  filter(receiver != "Interlochen Arts Academy High School") |> 
  filter(receiver != "Louisville Orchestra") |>
  filter(receiver != "New World Symphony") |>
  filter(receiver != "New York Philharmonic") |>
  filter(receiver != "Temple Preparatory School") |>
  filter(receiver != "Tanglewood Festival") |>
  filter(receiver != "Colorado Philharmonic") |>
  filter(receiver != "Cleveland Institute of Music (CIM)") |> 
  filter(receiver != "Manhattan School of Music Precollege program") |> 
  filter(receiver != "Curtis Institute (Philadelphia") |> 
  mutate(
    across(receiver, ~ gsub("Boston Conservatory University", "Boston Conservatory", .)),
    across(receiver, ~ gsub("Boston University School for the Arts", "Boston University", .)),
    across(receiver, ~ gsub("Boston University Tanglewood Institute", "Boston University", .)),
    across(receiver, ~ gsub("Boston University's School of Music", "Boston University", .)),
    across(receiver, ~ gsub("Boston Universitys College of Fine Arts", "Boston University", .)),
    across(receiver, ~ gsub("New England Conservatory of Music", "New England Conservatory", .)),
    across(receiver, ~ gsub("Brevard Music Center", "Brevard College", .)),
    across(receiver, ~ gsub("Beijing Central Conservatory", "Central Conservatory of Music in Beijing", .)),
    across(receiver, ~ gsub("Central Conservatory of Music (Beijing)", "Central Conservatory of Music in Beijing", .)),
    across(receiver, ~ gsub("Cleveland Institute of Music (CIM)", "Cleveland Institute of Music", .)),
    across(receiver, ~ gsub("The Cleveland Institute of Music" , "Cleveland Institute of Music", .)),
    across(receiver, ~ gsub("Colburn Conservatory of Music", "Colburn Conservatory", .)),
    across(receiver, ~ gsub("Colburn School Conservatory", "Colburn Conservatory", .)),
    across(receiver, ~ gsub("Colburn School of Performing Arts", "Colburn Conservatory", .)),
    across(receiver, ~ gsub("Colburn Schools Music Academy", "Colburn Conservatory", .)),
    across(receiver, ~ gsub("Colburn School", "Colburn Conservatory", .)),
    across(receiver, ~ gsub("The Colburn Conservatory", "Colburn Conservatory", .)),
    across(receiver, ~ gsub("Indiana University School of Music", "Indiana University", .)),
    across(receiver, ~ gsub("Indiana University Jacobs School of Music", "Indiana University", .)),
    across(receiver, ~ gsub("Indiana University String Academy", "Indiana University", .)),
    across(receiver, ~ gsub("Indiana Universitys Jacobs School of Music", "Indiana University", .)),
    across(receiver, ~ gsub("Jacobs School of Music at Indiana University", "Indiana University", .)),
    across(receiver, ~ gsub("Jacobs School of Music Indiana University", "Indiana University", .)),
    across(receiver, ~ gsub("Yale College", "Yale University", .)),
    across(receiver, ~ gsub("Yale School of Music", "Yale University", .)),
    across(receiver, ~ gsub("Yale University School of Music", "Yale University", .)),
    across(receiver, ~ gsub("University of Southern California Thornton School of Music", "USC", .)),
    across(receiver, ~ gsub("University of Southern Californias School of Music", "USC", .)),
    across(receiver, ~ gsub("University of Southern Californias Thornton School of Music", "USC", .)),
    across(receiver, ~ gsub("University of Southern California", "USC", .)),
    across(receiver, ~ gsub("USCs Thornton School of Music", "USC", .)),
    across(receiver, ~ gsub("USC Thornton School of Music", "USC", .)),
    across(receiver, ~ gsub("University of WisconsinMadison", "University of Wisconsin-Madison", .)),
    across(receiver, ~ gsub("University of Michigan School of Music", "University of Michigan", .)),
    across(receiver, ~ gsub("University of MichiganAnn Arbor", "University of Michigan", .)),
    across(receiver, ~ gsub("University of Cincinnati CollegeConservatory of Music", "University of Cincinnati College Conservatory of Music", .)),
    across(receiver, ~ gsub("University of Cincinnati College-Conservatory of Music", "University of Cincinnati College Conservatory of Music", .)),
    across(receiver, ~ gsub("UCLA Herb Alpert School of Music", "UCLA", .)),
    across(receiver, ~ gsub("University of California, Los Angeles", "UCLA", .)),
    across(receiver, ~ gsub("Toho School of Music", "Toho School", .)),
    across(receiver, ~ gsub("TohoGakuen School of Music", "Toho School", .)),
    across(receiver, ~ gsub("UCLA Herb Alpert School of Music", "UCLA", .)),
    across(receiver, ~ gsub("Northwestern University School of Music", "Northwestern University", .)),
    across(receiver, ~ gsub("Northwestern Universitys Bienen School of Music", "Northwestern University", .)),
    across(receiver, ~ gsub("Northwestern Universitys School of Music", "Northwestern University", .)),
    across(receiver, ~ gsub("The Juilliard School", "Juilliard School", .)),
    across(receiver, ~ gsub("Juilliard School of Music", "Juilliard School", .)),
    across(receiver, ~ gsub("Eastman School of Musics Preparatory Department", "Eastman", .)),
    across(receiver, ~ gsub("Eastman School of Music", "Eastman", .)),
    across(receiver, ~ gsub("Oberlin College and Conservatory of Music", "Oberlin College", .)),
    across(receiver, ~ gsub("Oberlin College Conservatory of Music", "Oberlin College", .)),
    across(receiver, ~ gsub("Oberlin Conservatory of Music", "Oberlin College", .)),
    across(receiver, ~ gsub("Oberlin Conservatory", "Oberlin College", .)),
    across(receiver, ~ gsub("State University of New York at Purchase", "SUNY Purchase", .)),
    across(receiver, ~ gsub("St. Petersburg Conservatory School of Music", "St. Petersburg Conservatory", .)),
    across(receiver, ~ gsub("Special Music School at St. Petersburg", "St. Petersburg Conservatory", .)),
    across(receiver, ~ gsub("Shepherd School of Music at Rice University", "Rice University", .)),
    across(receiver, ~ gsub("Rice Universitys Shepherd School of Music", "Rice University", .)),
    across(receiver, ~ gsub("Rice University's Shepherd School of Music", "Rice University", .)),
    across(receiver, ~ gsub("Rice University School of Music", "Rice University", .)),
    across(receiver, ~ gsub("Rice University Shepherd School of Music" , "Rice University", .)),
    across(receiver, ~ gsub("San Francisco Conservatory of Music Preparatory Division" , "San Francisco Conservatory", .)),
    across(receiver, ~ gsub("San Francisco Conservatory of Music" , "San Francisco Conservatory", .)),
    across(receiver, ~ gsub("Curtis Institute (Philadelphia)", "Curtis Institute", .)),
    across(receiver, ~ gsub("The Curtis Institute", "Curtis Institute", .)),
    across(receiver, ~ gsub("Curtis Institute of Music", "Curtis Institute", .)),
    across(receiver, ~ gsub("Geneva Conservatory", "Genevea", .)),
    across(receiver, ~ gsub("Geneva", "Geneva Conservatory", .)),
    across(receiver, ~ gsub("Peabody Conservatory of Johns Hopkins University", "Peabody Conservatory", .)),
    across(receiver, ~ gsub("Peabody Institute of the Johns Hopkins University", "Peabody Conservatory", .)),
    across(receiver, ~ gsub("Peabody Institute", "Peabody Conservatory", .)),
    across(receiver, ~ gsub("New England Conservatory Preparatory Division", "New England Conservatory", .)),
    across(receiver, ~ gsub("New England Conservatory Preparatory School", "New England Conservatory", .)),
  )

## create if else condition to find interlochen, aspen, or tanglewood.
grouping <- c('Interlochen', 'Aspen', 'Tanglewood')

schools_df_clean_q3 <- schools_df_clean |> 
  mutate(
    big_three_festivals = as.numeric(if_else(str_detect(festival, paste(grouping, collapse = "|")), "1", "0"))
  )

schools_df_clean_q3 |> ggplot(aes(x = big_three_festivals)) + geom_bar()

school_graphing_df <- schools_df_clean_q3 |> 
  select(name, receiver)

# graph objects and working w/ adding variables
school_q3 <- as.network.matrix(school_graphing_df, matrix.type = "edgelist") 

school_q3 |> network::set.vertex.attribute("big_three",schools_df_clean_q3$big_three_festivals)

# check vertex attributes
school_q3
network::get.vertex.attribute(school_q3,"big_three")

# ergm model
summary(school_q3 ~ edges)
summary(school_q3 ~ nodematch("big_three"))  

model1 <- ergm(school_q3 ~ edges              
               # Structural patterns
               + nodemix("big_three",base = 3)
               # Model constraints
               , constraints =~ bd(maxout=5) # This constraint enforces the maximum outdegree is 5
               , control = control.ergm(seed = 42)
               ,verbose = F
)

summary(model1) 

