# How are communities formed in the orchestra network? What trends are there and why?
# do a case study of 1) the connected component and 2) the isolated nodes

rm(list = ls())
# load packages
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(igraph)
library(statnet)
library(here)

# load in data
load(here("data/orchestra_network_cleanish.rda"))




### ANALYSIS on SCHOOLS
schools_df_clean_2 <- schools_df_clean |> 
  select(
    name,
    receiver
  )

school_ties <- as.network.matrix(schools_df_clean_2, matrix.type = "edgelist", directed = FALSE)
school_igraph <- graph_from_adjacency_matrix(as.matrix.network(school_ties), mode = c("undirected"))

#### NETWORK DESCRIPTIVE STATISTICS FOR FINAL REPORT
# DESCRIPTIVE STATISTICS (same throughout all questions)

is_directed(school_igraph)

# the number of nodes
vcount(school_igraph) 
# 692 nodes

# the number of edges
ecount(school_igraph)
# 868 edges

# count of potential links:
# formula is n * (n - 1), with n being # of nodes
n = 692
(n * (n - 1)) / 2
# we have 239,086 potential links!

# density of network
edge_density(school_igraph)
# 0.003630493

# k-cores
detach('package:statnet', unload = TRUE)
library(igraph)

kcore_schools <-
  school_igraph %>% graph.coreness(.)
kcore_schools

# community detection algorithm & stats
# 3 options from the lab
cluster_schools <- school_igraph %>% cluster_walktrap()
#cluster_schools <- school_igraph %>% cluster_edge_betweenness()
#cluster_schools <- school_igraph %>% cluster_edge_infomap()

#More possible community detection algorithms I found online
#cluster_schools <- school_igraph %>% cluster_louvain()
#cluster_schools <- school_igraph %>% cluster_label_prop()
#cluster_schools <- school_igraph %>% cluster_fast_greedy()
#cluster_schools <- school_igraph %>% cluster_leading_eigen()
#cluster_schools <- school_igraph %>% cluster_bigclam()
#cluster_schools <- school_igraph %>% cluster_mcl()

membership_df <- as.data.frame(membership(cluster_schools))
colnames(membership_df) <- c("group")
membership_sorted <- membership_df %>% arrange(group)

# Print the sorted membership
print(membership_sorted)

# some communities to check large ones (like 3) and small ones.

length(cluster_schools)
sizes(cluster_schools) 
modularity(cluster_schools)


set.seed(50)

plot(
  cluster_schools, 
  school_igraph, 
  layout = layout_with_drl(school_igraph),
  edge.arrow.size = .03,
  vertex.size = 2,
  vertex.label = NA,
  vertex.color = adjustcolor(membership(cluster_schools), alpha.f = .3),
  vertex.label.cex = .5,
  vertex.label.color = 'black',
  mark.groups = by(seq_along(membership(cluster_schools)), membership(cluster_schools), invisible),
  mark.shape = 1 / 4,
  mark.col = rainbow(length(cluster_schools), alpha = .04),
  mark.border = NA
)


