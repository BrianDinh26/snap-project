# Use ERGM to find out the theta parameters (common small structures) associated with this school network.
# NOTE: CHANGE ALL NETWORK CONVERSIONS TO UNDIRECTED.


# load packages ----
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(statnet)
library(doMC)

# set up parallel processing
num_cores <- parallel::detectCores(logical = TRUE)

registerDoMC(cores = num_cores)

# load in data ----
load(here("data/orchestra.rda"))
load(here("data/orchestra_network_cleanish.rda"))


## CONVERT DATA INTO NETWORK DATA

schools_df_clean_mat <- schools_df_clean |> 
  select(name, receiver)

# create matrix object
school_matrix <- as.network.matrix(schools_df_clean_mat, matrix.type = "edgelist")

school_q3 <- as.network.matrix(schools_df_clean_mat, matrix.type = "edgelist") 

sna_school <-
  igraph::as_adjacency_matrix(school_q3, sparse = FALSE) %>% network::as.network.matrix()

schools_df_clean_mat |> graph_from_data_frame() |>
  as_tbl_graph() |>
  as.undirected()

# ergm stuff
model1 <- ergm(school_matrix ~ 
                 edges +
                 gwidegree(cutoff = 150)
               , control = control.ergm(seed = 42, MCMLE.density.guard = 200)
               ,verbose = F
)

summary(model1)

