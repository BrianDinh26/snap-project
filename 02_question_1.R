# Running baseline code from labs 1, 2, and 3 in order to complete part of the project.
#### question #1: What schools seem to be most central/influential within the orchestra network?

# load packages
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(statnet)
library(igraph)

# load in data
load(here("data/orchestra_network_cleanish.rda"))

# convert data into social network data 
schools_df_2 <- schools_df_clean |> 
  select(
    name,
    receiver
  )

school_ties <- as.network.matrix(schools_df_2, matrix.type = "edgelist")
school_igraph <- graph_from_adjacency_matrix(as.matrix.network(school_ties))

save(school_igraph, file = here("data/school_igraph"))

### descriptive statistics

is_directed(school_igraph) # yes, this graph is DIRECTED

# the number of nodes
vcount(school_igraph) 
# 692 nodes

# the number of edges
ecount(school_igraph)
# 868 edges

# count of potential links:
# formula is n * (n - 1), with n being # of nodes
n = 692
n * (n - 1)
# we have 478,172 potential links!

# density of network
edge_density(school_igraph)
# 0.001815246

#### question #1: What schools seem to be most central/influential within the orchestra network?
# for this question, we look towards MEASURES OF CENTRALITY... since we have a max page limit of 12,
# go for the top 5 schools that seem to be dope...

# convert igraph to sna
sna_school <-
  igraph::as_adjacency_matrix(school_igraph, sparse = FALSE) %>% network::as.network.matrix()

detach('package:igraph')
library(statnet)

# create empty dataframe
centralities_school <- data.frame('node_name' = as.character(network.vertex.names(sna_school)))

# degree centrality:
centralities_school$degree <- degree(sna_school, cmode = 'freeman')

centralities_school |> 
  dplyr::slice_max(order_by = degree, n = 5) |>
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
  dplyr::slice_max(order_by = betweenness, n = 5) |> 
  select(node_name, betweenness) |> 
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
  dplyr::slice_max(order_by = closeness, n = 5) |> 
  select(node_name, closeness) |> 
  kableExtra::kable()
# sorta useless tbh, maybe get rid of it in final analysis?

# eigenvector centrality
centralities_school$eigen <-
  igraph::eigen_centrality(school_igraph)$vector

centralities_school |> 
  dplyr::slice_max(order_by = eigen, n = 5) |> 
  select(node_name, eigen) |> 
  kableExtra::kable()

# hub centrality
centralities_school$hub <- igraph::hub_score(school_igraph, scale = TRUE)$`vector`

centralities_school |> 
  dplyr::slice_max(order_by = hub, n = 5) |> 
  select(node_name, hub) |> 
  kableExtra::kable()

# authority
centralities_school$authority <- igraph::authority_score(school_igraph, scale = TRUE)$`vector`

centralities_school |> 
  dplyr::slice_max(order_by = authority, n = 5) |> 
  select(node_name, eigen) |> 
  kableExtra::kable()

#### VISUALIZATIONS

