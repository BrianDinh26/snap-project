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

# coding stuff.


print(V(mso_igraph2)$type)

category_colors <- c("musician" = "red", "school" = "blue", "orchestra" = "green")
V(mso_igraph2)$color <- category_colors[V(mso_igraph2)$type]

print(V(mso_igraph2)$color)

plot(
  mso_igraph2,
  layout = layout_nicely(mso_igraph),
  edge.color = 'black',
  vertex.label = NA,
  vertex.size = 2,
  vertex.color = V(mso_igraph2)$color
)
