rm(list = ls())

# load packages ----
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
library(here)

# load in data ----
load(file = here("data/orchestra.rda"))
load(file = here("data/orchestra_network_cleanish.rda"))
load(file = here("results/schools_df_clean_q2.rda"))
orchestra_prestige <- read.csv(file = here("Prestige_Data/orchestra_prestige.csv"))



## CONVERT DATA INTO NETWORK DATA

schools_df_clean_mat <- schools_df_clean |>
  select(name, receiver)

# Assuming 'orchestra' and 'schools_df_clean_mat' are your data frames

# Selecting the relevant columns from 'orchestra'
orchestra_subset <- orchestra[, c("name", "new_column")]

# Merge orchestra_prestige with merged_data based on the school names
merged_data <- merge(merged_data, orchestra_prestige, by.x = "receiver", by.y = "School", all.x = TRUE)

# Displaying the first few rows of the updated merged data
head(merged_data)


# Plot the whole network
plot(merged_data, vertex.size = 7, vertex.label = NA,
     # Settings for layouts:
     # Uncomment and experiment with different layout options to find the best one for your data
     # layout = layout_nicely(merged_data)      ## Automated layout recommendation from iGraph
     # layout = layout_with_fr(merged_data)    ## Fruchterman-Reingold algorithm
     # layout = layout_with_dh(merged_data)    ## Davidson and Harel algorithm
     # layout = layout_with_drl(merged_data)   ## Force-directed algorithm
     # layout = layout_with_kk(merged_data)    ## Kamada-Kawai algorithm
     # layout = layout_with_lgl(merged_data)   ## Large graph layout
)

# You can comment on the macro-level structure of your graph based on the visualization in a paragraph here

# Convert merged_data into an igraph graph object
merged_graph <- graph_from_data_frame(merged_data, directed = TRUE)

# Take out the largest component from the merged graph
merged_comp <- components(merged_graph)
giantGraph_merged <- merged_graph %>%
  induced_subgraph(which(merged_comp$membership == which.max(merged_comp$csize)))

# Plot the largest component of the merged network
plot(giantGraph_merged, vertex.size = 7, vertex.label = NA,
     # Settings for layouts:
     # Uncomment and experiment with different layout options to find the best one for your data
     # layout = layout_nicely(giantGraph_merged)      ## Automated layout recommendation from iGraph
     # layout = layout_with_fr(giantGraph_merged)    ## Fruchterman-Reingold algorithm
     # layout = layout_with_dh(giantGraph_merged)    ## Davidson and Harel algorithm
     # layout = layout_with_drl(giantGraph_merged)   ## Force-directed algorithm
     # layout = layout_with_kk(giantGraph_merged)    ## Kamada-Kawai algorithm
     # layout = layout_with_lgl(giantGraph_merged)   ## Large graph layout
)

# Convert merged_data into an igraph graph object
# Plot the merged network with node color representing topics from the 'new_column' variable
# Convert merged_data into an igraph graph object
merged_graph <- graph_from_data_frame(merged_data, directed = TRUE)

# Plot the merged network with node color representing topics from the third column of merged_data
plot_merged_network <- merged_graph |>
  as_tbl_graph() |>
  ggraph(layout = 'fr') +
  geom_edge_link2() +
  geom_node_label(aes(label = name, colour=Prestige) +  # Color nodes by the third column
  theme_void()
                  
print(plot_merged_network)
                  
                  
                  
                  
