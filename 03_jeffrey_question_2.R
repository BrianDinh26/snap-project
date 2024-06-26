# How does prestige of school correlate to presence and centrality measures in this network? 

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
load(here("data/orchestra_network_cleanish.rda"))
orchestra_prestige <- read_csv(here("Prestige_data/orchestra_prestige.csv"))

orchestra_prestige |> filter(Prestige == 1)

# prestige schools according to jeffrey
#prestige_schools <- c('Juilliard', 'Curtis', 'Manhattan', 'Berklee', 'Mannes', 'New England Conservatory',
#                      'Boston Conservatory', 'Cleveland Institute of Music', 'San Francisco Conservatory',
#                      'Peabody', 'Eastman', 'Oberlin', 'Academy of Vocal Arts', 'Indiana University', 'New World Symphony',
#                      'Los Angeles College of Music', 'Bard College', 'McNally Smith College of Music', 'Colburn Conservatory',
#                      'Musicians Institute')

prestige_schools <- c('Juilliard', 'Curtis', 'Manhattan', 'New England Conservatory',
                      'Boston Conservatory', 'Cleveland Institute of Music', 'San Francisco Conservatory', 'Oberlin', 'New World Symphony')

# create column for prestige.
schools_df_clean_q2 <- schools_df_clean |> 
  mutate(
    prestige = as.character(if_else(str_detect(receiver, paste(prestige_schools, collapse = "|")), "1", "0"))
  )

save(schools_df_clean_q2, file = here("results/schools_df_clean_q2.rda"))

# distribution of prestige by statistical measures...
schools_df_clean_q2 |> ggplot(aes(x = as.factor(prestige))) + 
  geom_bar(aes(fill = as.factor(prestige))) +
  labs(title = "Distribution of Schools by Prestige in Orchestral Musician Network",
       x = "Is this school considered prestigious?",
       y = "Count") +
  theme_classic()


### SOCIAL NETWORK GRAPHING
school_graphing_df <- schools_df_clean_q2 |> 
  select(name, receiver)

# graph objects and working w/ adding variables
school_q2 <- as.network.matrix(school_graphing_df, matrix.type = "edgelist", directed = FALSE) 
school_q2 |> network::set.vertex.attribute("prestige", schools_df_clean_q2$prestige)
network::get.vertex.attribute(school_q2,"prestige")

school_igraph <- graph_from_adjacency_matrix(as.matrix.network(school_q2), mode = c("undirected"))
is_directed(school_igraph)


sna_school <-
  igraph::as_adjacency_matrix(school_igraph, sparse = FALSE) |> 
  network::as.network.matrix()


### QUESTION #2 STUFF:

detach('package:igraph')
library(statnet)

# create empty dataframe
centralities_school <- data.frame('node_name' = as.character(network.vertex.names(school_q2)))

centralities_school <- centralities_school |> 
  mutate(
    prestige = as.character(if_else(str_detect(node_name, paste(prestige_schools, collapse = "|")), "1", "0"))
  )

# degree centrality:
centralities_school$degree <- degree(sna_school, cmode = 'freeman')

centralities_school |> 
  dplyr::slice_max(order_by = degree, n = 10) |>
  select(node_name, degree, prestige) |> 
  kableExtra::kable() 
# top 5 schools by degree centrality are:
# Juilliard, Curtis, New England COnservatory, USC, and the CLeveland Institue of Music

### MEASURES OF CENTRALITY

## in-degree & out-degree centrality:
centralities_school$in_degree <- degree(sna_school, cmode = 'indegree')
centralities_school$out_degree <- degree(sna_school, cmode = 'outdegree')

# betweenness centrality:
centralities_school$betweenness <- betweenness(sna_school)

centralities_school |> 
  dplyr::slice_max(order_by = betweenness, n = 10) |> 
  select(node_name, betweenness, prestige) |> 
  kableExtra::kable()

# closeness centrality:
centralities_school$closeness <-
  igraph::closeness(
    school_igraph, 
    mode = 'all'
  )

## in-closeness & out-closeness centrality:
centralities_school$incloseness <- igraph::closeness(school_igraph, mode = 'in')
centralities_school$outcloseness <- igraph::closeness(school_igraph, mode = 'out')

centralities_school |> 
  dplyr::slice_max(order_by = closeness, n = 10) |> 
  select(node_name, closeness, prestige) |> 
  kableExtra::kable()
# sorta useless tbh, maybe get rid of it in final analysis?

# eigenvector centrality
centralities_school$eigen <-
  igraph::eigen_centrality(school_igraph)$vector

centralities_school |> 
  dplyr::slice_max(order_by = eigen, n = 10) |> 
  select(node_name, eigen, prestige) |> 
  kableExtra::kable()

# hub centrality
centralities_school$hub <- igraph::hub_score(school_igraph, scale = TRUE)$`vector`

centralities_school |> 
  dplyr::slice_max(order_by = hub, n = 10) |> 
  select(node_name, hub, prestige) |> 
  kableExtra::kable()

# authority
centralities_school$authority <- igraph::authority_score(school_igraph, scale = TRUE)$`vector`

centralities_school |> 
  dplyr::slice_max(order_by = authority, n = 10) |> 
  select(node_name, authority, prestige) |> 
  kableExtra::kable()

save(centralities_school, file = here("results/centralities_school.rda"))

## VISUALIZAITONS FOR QUESTION 2
library(igraph)

deg <- degree(school_igraph, mode="in")
ver_size <- (deg * 0.1) + 1
net_layout_school <- layout_with_fr(school_igraph)
check <- as.data.frame(deg)
igraph_options(vertex.size = 2, vertex.color = 'white', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='red', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)    


head(V(school_igraph))


plot(
  school_igraph,
  layout = layout_nicely(school_igraph),
  edge.color = 'black',
  vertex.label = NA,
  vertex.color = ifelse(centralities_school$prestige == "1", "red", "white"),
  vertex.shape = ifelse(centralities_school$prestige == "1", "csquare", "circle"),
  vertex.size = ifelse(degree(school_igraph, mode = "in") > 15, ver_size, 1.5),
  
)


## ERGM THING WE MIGHT USE FOR QUESTION 1????? LATER
school_q2 |> network::set.vertex.attribute("prestige", schools_df_clean_q2$prestige)

# check vertex attributes
school_q2
network::get.vertex.attribute(school_q2,"prestige")

# ergm model
summary(school_q2 ~ edges)

model1 <- ergm(school_q2 ~ edges              
               # Structural patterns
               + nodefactor("prestige")
               # Model constraints
               , constraints =~ bd(maxout=5) # This constraint enforces the maximum outdegree is 5
               , control = control.ergm(seed = 42)
               ,verbose = F
)

summary(model1) 
