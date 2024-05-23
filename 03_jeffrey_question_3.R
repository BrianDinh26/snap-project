# How are communities formed in the orchestra network? What trends are there and why?
# do a case study of 1) the connected component and 2) the isolated nodes

# load packages
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(igraph)
library(statnet)

# load in data
load(here("data/orchestra_network_cleanish.rda"))


### ANALYSIS on SCHOOLS
schools_df_clean_2 <- schools_df_clean |> 
  select(
    name,
    receiver
  )

school_ties <- as.network.matrix(schools_df_clean_2, matrix.type = "edgelist")
school_igraph <- graph_from_adjacency_matrix(as.matrix.network(school_ties))

# k-cores
detach('package:statnet', unload = TRUE)
library(igraph)

kcore_schools <-
  school_igraph %>% graph.coreness(.)
kcore_schools

# community detection algorithm & stats
cluster_schools <- school_igraph %>% cluster_walktrap()

check <- as.data.frame(membership(cluster_schools))

# some communities to check large ones (like 3) and small ones.

length(cluster_schools)
sizes(cluster_schools) 
modularity(cluster_schools)

# plot these communities
set.seed(50)

cluster_schools %>% plot(
  .,
  school_igraph,
  # layout = layout_with_gem(.),
  # layout = layout_nicely(school_igraph),
  layout = layout_with_drl(school_igraph),
  edge.arrow.size = .03,
  vertex.size = 2,
  vertex.label = NA,
  vertex.color = adjustcolor(membership(.), alpha.f = .3),
  vertex.label.cex = .5,
  vertex.label.color = 'black',
  mark.groups = by(seq_along(membership(.)), membership(.), invisible),
  mark.shape = 1 / 4,
  mark.col = rainbow(length(.), alpha = .04),
  mark.border = NA
)


