# Use ERGM to find out the theta parameters (common small structures) associated with this school network.
# NOTE: CHANGE ALL NETWORK CONVERSIONS TO UNDIRECTED...


# load packages ----
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(statnet)
library(doMC)
library(igraph)

# set up parallel processing
num_cores <- parallel::detectCores(logical = TRUE)

registerDoMC(cores = num_cores)

# load in data ----
load(here("data/orchestra.rda"))
load(here("data/orchestra_network_cleanish.rda"))
load(here("results/schools_df_clean_q2.rda"))

## CONVERT DATA INTO NETWORK DATA

schools_df_clean_mat <- schools_df_clean |>
  select(name, receiver)

# create matrix object

school_q1 <- as.network.matrix(schools_df_clean_mat,
                               matrix.type = "edgelist",
                               directed = FALSE)
school_q1 |> network::set.vertex.attribute("prestige", schools_df_clean_q2$prestige)
network::get.vertex.attribute(school_q1,"prestige")


# ergm stuff
question_1_model <- ergm(
  school_q1 ~
    edges +
    nodefactor("prestige") +
    nodematch("prestige") +
    nodemix("prestige", base = 1)   
  ,
  control = control.ergm(seed = 42, MCMLE.density.guard = 1500)
  ,
  verbose = F
)

# edges works
# triangles does not work
# isolates does not work
# gwdsp works
# gwesp does NOT work.
# gwdegree takes forever to run.
# kstar(2) takes forever to load but is probably helpful???

summary(question_1_model)
