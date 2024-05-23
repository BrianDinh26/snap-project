# Use ERGM to find out the theta parameters (common small structures) associated with this school network.

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

# ergm stuff
model1 <- ergm(school_matrix ~ 
                 edges +
                 gwidegree(cutoff = 150)
               , control = control.ergm(seed = 42, MCMLE.density.guard = 1000)
               ,verbose = F
)

summary(model1)

