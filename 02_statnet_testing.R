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

load(here("data/orchestra_network_cleanish.rda"))

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

advice |> network::set.vertex.attribute("department", 
                                        value = read.csv("departmentNode.csv")$department)


