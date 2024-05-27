# 1) Overall average centrality measures of musician-orchestra-school network.
# 2) Then those averages but sliced over only musicians who attended a prestigious school
# 3) And only those who didn't.

# load packages ----
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(statnet)
library(doMC)
library(igraph)
library(statnet)

# set up parallel processing
num_cores <- parallel::detectCores(logical = TRUE)

registerDoMC(cores = num_cores)

# load in data ----
load(here("data/orchestra.rda"))

# just realized we don't have each orchestra name in the data. here's the fix.
orchestra <- orchestra |> 
  mutate(new_column = case_when(
    row_number() >= 1 & row_number() <= 98 ~ "Chicago Symphony Orchestra",
    row_number() >= 99 & row_number() <= 216 ~ "LA Phil",
    row_number() >= 217 & row_number() <= 316 ~ "Boston Symphony Orchestra",
    row_number() >= 317 & row_number() <= 413 ~ "Philadelphia Orchestra",
    row_number() >= 414 & row_number() <= 518 ~ "New York Philharmonic",
    TRUE ~ NA_character_ # if there are rows outside the defined ranges, fill with NA
  )) |> 
  rename(orchestra = new_column)

# create prestige by MUSICIAN
prestige_schools <- c('Juilliard', 'Curtis', 'Manhattan', 'New England Conservatory',
                      'Boston Conservatory', 'Cleveland Institute of Music', 'San Francisco Conservatory', 'Oberlin', 'New World Symphony')

orchestra <- orchestra |> 
  mutate(
    prestige = as.character(if_else(str_detect(schools, paste(prestige_schools, collapse = "|")), "1", "0"))
  )

save(orchestra, file = here("data/orchestra.rda"))

# create the musician-orchestra-school network

# create dataframes for school
schools_df <- data.frame(name = character(),
                         instrument = character(),
                         when_began_current = character(),
                         receiver = character(),
                         type = character(),
                         prestige = character(),
                         orchestra = character())

for (i in 1:nrow(orchestra)) {
  name_id <- orchestra$name[i]
  instrument_id <- orchestra$instrument[i]
  when_began_current_id <- orchestra$when_began_current[i]
  prestige_id <- orchestra$prestige[i]
  orchestra_id <- orchestra$orchestra[i]
  
  schools_list <- unlist(strsplit(orchestra$schools[i], ";"))
  label = "schools"
  
  temp_df <- data.frame(name = name_id,
                        instrument = instrument_id,
                        when_began_current = when_began_current_id,
                        prestige = prestige_id,
                        orchestra = orchestra_id,
                        receiver = schools_list,
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

# wait i'm dumb this is easier than i thought

# part 1)

# I'm adding type if we want to use it in vertex-specific visualizations later.
musician_orchestra <- orchestra |> 
  select(name, 
         receiver = orchestra,
         prestige) |> 
  mutate(type = "orchestra")

musician_school <- schools_df_clean |> 
  select(name, receiver, prestige, type)

musician_orchestra_school <- bind_rows(musician_orchestra, musician_school)

musician_orchestra_school_p1 <- musician_orchestra_school |>
  select(name, receiver)

mso_network_full <- as.network.matrix(musician_orchestra_school_p1, matrix.type = "edgelist", directed = FALSE) 
mso_network_full |> network::set.vertex.attribute("prestige", musician_orchestra_school$prestige)
network::get.vertex.attribute(mso_network_full,"prestige")

mso_igraph <- graph_from_adjacency_matrix(as.matrix.network(mso_network_full), mode = c("undirected"))
is_directed(mso_igraph)

sna_mso <-
  igraph::as_adjacency_matrix(mso_igraph, sparse = FALSE) |> 
  network::as.network.matrix()

### CENTRALITY MEASURES FOR ENTIRE MUSICIAN-SCHOOL-ORCHESTRA NETWORK (MSO)
detach('package:igraph')
library(statnet)
# create empty dataframe
centralities_mso <- data.frame('node_name' = as.character(network.vertex.names(mso_network_full)))

# degree centrality:
centralities_mso$degree <- degree(sna_mso, cmode = 'freeman')

centralities_mso |> 
  dplyr::slice_max(order_by = degree, n = 10) |>
  select(node_name, degree) |> 
  kableExtra::kable() 

# betweenness centrality:
centralities_mso$betweenness <- betweenness(sna_mso)

centralities_mso |> 
  dplyr::slice_max(order_by = betweenness, n = 10) |> 
  select(node_name, betweenness) |> 
  kableExtra::kable()

# closeness centrality:
centralities_mso$closeness <-
  igraph::closeness(
    mso_igraph, 
    mode = 'all'
  )

centralities_mso |> 
  dplyr::slice_max(order_by = closeness, n = 10) |> 
  select(node_name, closeness) |> 
  kableExtra::kable()

# eigenvector centrality
centralities_mso$eigen <-
  igraph::eigen_centrality(mso_igraph)$vector

centralities_mso |> 
  dplyr::slice_max(order_by = eigen, n = 10) |> 
  select(node_name, eigen) |> 
  kableExtra::kable()

# hub centrality
centralities_mso$hub <- igraph::hub_score(mso_igraph, scale = TRUE)$`vector`

centralities_mso |> 
  dplyr::slice_max(order_by = hub, n = 10) |> 
  select(node_name, hub) |> 
  kableExtra::kable()

# authority
centralities_mso$authority <- igraph::authority_score(mso_igraph, scale = TRUE)$`vector`

centralities_mso |> 
  dplyr::slice_max(order_by = authority, n = 10) |> 
  select(node_name, authority) |> 
  kableExtra::kable()

save(centralities_mso, file = here("results/centralities_mso.rda"))

##### PART 2): MUSICIANS WHO ATTENDED A PRESTIGIOUS SCHOOL

mso_has_prestige <- musician_orchestra_school |>
  filter(prestige == 1) |> 
  select(name, receiver)

# create network objects
library(igraph)
mso_network_prestige <- as.network.matrix(mso_has_prestige, matrix.type = "edgelist", directed = FALSE) 

mso_igraph_prestige <- graph_from_adjacency_matrix(as.matrix.network(mso_network_prestige), mode = c("undirected"))
is_directed(mso_igraph_prestige)

sna_mso_prestige <-
  igraph::as_adjacency_matrix(mso_igraph_prestige, sparse = FALSE) |> 
  network::as.network.matrix()

#### CENTRALITY MEASURES FOR PRESTIGIOUS ATTENDERS
detach('package:igraph')
library(statnet)
# create empty dataframe
centralities_mso_prestige <- data.frame('node_name' = as.character(network.vertex.names(mso_network_prestige)))

# degree centrality:
centralities_mso_prestige$degree <- degree(sna_mso_prestige, cmode = 'freeman')

centralities_mso_prestige |> 
  dplyr::slice_max(order_by = degree, n = 10) |>
  select(node_name, degree) |> 
  kableExtra::kable() 

# betweenness centrality:
centralities_mso_prestige$betweenness <- betweenness(sna_mso_prestige)

centralities_mso_prestige |> 
  dplyr::slice_max(order_by = betweenness, n = 10) |> 
  select(node_name, betweenness) |> 
  kableExtra::kable()

# closeness centrality:
centralities_mso_prestige$closeness <-
  igraph::closeness(
    mso_igraph_prestige, 
    mode = 'all'
  )

centralities_mso_prestige |> 
  dplyr::slice_max(order_by = closeness, n = 10) |> 
  select(node_name, closeness) |> 
  kableExtra::kable()

# eigenvector centrality
centralities_mso_prestige$eigen <-
  igraph::eigen_centrality(mso_igraph_prestige)$vector

centralities_mso_prestige |> 
  dplyr::slice_max(order_by = eigen, n = 10) |> 
  select(node_name, eigen) |> 
  kableExtra::kable()

# hub centrality
centralities_mso_prestige$hub <- igraph::hub_score(mso_igraph_prestige, scale = TRUE)$`vector`

centralities_mso_prestige |> 
  dplyr::slice_max(order_by = hub, n = 10) |> 
  select(node_name, hub) |> 
  kableExtra::kable()

# authority
centralities_mso_prestige$authority <- igraph::authority_score(mso_igraph_prestige, scale = TRUE)$`vector`

centralities_mso_prestige |> 
  dplyr::slice_max(order_by = authority, n = 10) |> 
  select(node_name, authority) |> 
  kableExtra::kable()

save(centralities_mso_prestige, file = here("results/centralities_mso_prestige.rda"))





##### PART 3): MUSICIANS WHO DID NOT ATTEND A PRESTIGIOUS SCHOOL
mso_no_prestige <- musician_orchestra_school |>
  filter(prestige == 0) |> 
  select(name, receiver)

# create network objects
library(igraph)
mso_network_no_prestige <- as.network.matrix(mso_no_prestige, matrix.type = "edgelist", directed = FALSE) 

mso_igraph_no_prestige <- graph_from_adjacency_matrix(as.matrix.network(mso_network_no_prestige), mode = c("undirected"))
is_directed(mso_igraph_no_prestige)

sna_mso_no_prestige <-
  igraph::as_adjacency_matrix(mso_igraph_no_prestige, sparse = FALSE) |> 
  network::as.network.matrix()

#### CENTRALITY MEASURES FOR PRESTIGIOUS ATTENDERS
detach('package:igraph')
library(statnet)
# create empty dataframe
centralities_mso_no_prestige <- data.frame('node_name' = as.character(network.vertex.names(mso_network_no_prestige)))

# degree centrality:
centralities_mso_no_prestige$degree <- degree(sna_mso_no_prestige, cmode = 'freeman')

centralities_mso_no_prestige |> 
  dplyr::slice_max(order_by = degree, n = 10) |>
  select(node_name, degree) |> 
  kableExtra::kable() 

# betweenness centrality:
centralities_mso_no_prestige$betweenness <- betweenness(sna_mso_no_prestige)

centralities_mso_no_prestige |> 
  dplyr::slice_max(order_by = betweenness, n = 10) |> 
  select(node_name, betweenness) |> 
  kableExtra::kable()

# closeness centrality:
centralities_mso_no_prestige$closeness <-
  igraph::closeness(
    mso_igraph_no_prestige, 
    mode = 'all'
  )

centralities_mso_no_prestige |> 
  dplyr::slice_max(order_by = closeness, n = 10) |> 
  select(node_name, closeness) |> 
  kableExtra::kable()

# eigenvector centrality
centralities_mso_no_prestige$eigen <-
  igraph::eigen_centrality(mso_igraph_no_prestige)$vector

centralities_mso_no_prestige |> 
  dplyr::slice_max(order_by = eigen, n = 10) |> 
  select(node_name, eigen) |> 
  kableExtra::kable()

# hub centrality
centralities_mso_no_prestige$hub <- igraph::hub_score(mso_igraph_no_prestige, scale = TRUE)$`vector`

centralities_mso_no_prestige |> 
  dplyr::slice_max(order_by = hub, n = 10) |> 
  select(node_name, hub) |> 
  kableExtra::kable()

# authority
centralities_mso_no_prestige$authority <- igraph::authority_score(mso_igraph_no_prestige, scale = TRUE)$`vector`

centralities_mso_no_prestige |> 
  dplyr::slice_max(order_by = authority, n = 10) |> 
  select(node_name, authority) |> 
  kableExtra::kable()

save(centralities_mso_no_prestige, file = here("results/centralities_mso_no_prestige.rda"))


