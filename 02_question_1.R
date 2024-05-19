# Running baseline code from labs 1, 2, and 3 in order to complete part of the project.

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

# descriptive statistics

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

# question #1: What schools seem to be most central/influential within the orchestra network?



