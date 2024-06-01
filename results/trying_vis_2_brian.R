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

# load in igraph object used for this. it's pretty convoluted but if you want to see how I denoted nodes,
# check out 03_nonvisual_Q1.R
# otherwise just mess around w/ graph settings here
load(file = here("results/mso_igraph2.rda"))


# the real code goes here
authority_scores <- authority_score(mso_igraph2, scale = TRUE)$`vector`
authority_scores[order(authority_scores)]
# highest is 1, lowest is 0.

color_function <- colorRampPalette(c("white", "darkblue"))

node_colors <- color_function(100)[as.numeric(cut(authority_scores, breaks = 100))]

auth_ver_size <- (authority_scores * 5) + 1

category_shapes <- c("musician" = "circle", "school" = "square", "orchestra" = "vrectangle")
V(mso_igraph2)$shape <- category_shapes[V(mso_igraph2)$type]
print(V(mso_igraph2)$shape)

plot(
  mso_igraph2,
  layout = layout_nicely(mso_igraph2),
  edge.color = 'black',
  vertex.label = NA,
  vertex.color = node_colors,
  vertex.size = auth_ver_size,
  vertex.shapes = V(mso_igraph2)$shape
)

# old code maybe use
deg <- degree(school_igraph, mode="in")
ver_size <- (deg * 0.1) + 1
net_layout_school <- layout_with_fr(school_igraph)
check <- as.data.frame(deg)

unique(degree(school_igraph, mode="in"))

igraph_options(vertex.size = 2, vertex.color = 'white', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='red', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)    


plot(
  school_igraph,
  layout = layout_nicely(school_igraph),
  edge.color = 'black',
  vertex.label = NA,
  vertex.color = ifelse(degree(school_igraph, mode = "in") > 15, "red", "white"),
  vertex.size = ifelse(degree(school_igraph, mode = "in") > 15, ver_size, 1.5)
)