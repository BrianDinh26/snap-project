# SNAP Project, Statnet Testing

# load packages ----
library(here)
library(statnet)
library(readr)
library(tidytext)
library(tidygraph)
library(ggraph)
library(igraph)
library(tidyverse)
library(topicmodels)
library(textstem)
library(udpipe)
library(dplyr)
library(`igraph`)

load(here("data/orchestra_network_cleanish.rda"))
load(here("data/orchestra_vertexes.rda"))

backforth <- orchestra_network_cleanish |> 
  select(
    name,
    receiver
  )

head(backforth)

# Convert the edgelist to a network object in statnet format:
orchestra_ties <- as.network.matrix(backforth, matrix.type = "edgelist") 

# see summary
orchestra_ties
# 3387 vertices, directed network, 5084 edges

orchestra_ties |> network::set.vertex.attribute("type", 
                                                value = orchestra_type$type)

orchestra_ties |> network::set.vertex.attribute("instrument", 
                                        value = orchestra_instrument$instrument)

orchestra_ties |> network::set.vertex.attribute("begin", 
                                                value = orchestra_begin$when_began_current)
# check if independent attributes have been added
orchestra_ties

network::get.vertex.attribute(orchestra_ties,"type")
network::get.vertex.attribute(orchestra_ties,"instrument")
network::get.vertex.attribute(orchestra_ties,"begin")

# alright visualization time
orchestra_igraph <- graph.adjacency(as.matrix.network(orchestra_ties))
net_layout <- layout_with_fr(orchestra_igraph)

# plot
igraph_options(vertex.size = 2, vertex.color = 'red', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)   

plot(orchestra_igraph, layout=net_layout, edge.color='black', vertex.label = NA)
# aight it works but it's also super unreadable
# may have to limit our analysis to maybe a few things???? or like definitely less than 5414... reframe
# the questions possibly.

# attempt by visualizing only the school networks
schools_df_2 <- schools_df |> 
  select(
    name,
    receiver
  )

school_ties <- as.network.matrix(schools_df_2, matrix.type = "edgelist")
school_igraph <- graph.adjacency(as.matrix.network(school_ties))
net_layout_school <- layout_with_fr(school_igraph)

igraph_options(vertex.size = 2, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)    

plot(school_igraph, layout=net_layout_school, edge.color='black', vertex.label = NA)

# plotting only teachers

three_ties <- as.network.matrix(orch_lim, matrix.type = "edgelist")
three_igraph <- graph.adjacency(as.matrix.network(three_ties))
net_layout_three <- layout_with_fr(three_igraph)

igraph_options(vertex.size = 1, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)    

plot(three_igraph, layout=net_layout_three, edge.color='black', vertex.label = NA)


