# Question #2: What factors predict how well orchestras, professors, 
# and schools connect to each other? It is based on location, prestige, neither?

# load packages
library(tidyverse)
library(tidymodels)
library(naniar)
library(here)
library(igraph)
library(statnet)

# load in data
load(here("data/orchestra_network_cleanish.rda"))

### ANALYSIS ON TEACHERS -----------

# create social network graph object
teachers_df_2 <- teachers_df_clean |> 
  select(
    name,
    receiver
  )

teacher_ties <- as.network.matrix(teachers_df_2, matrix.type = "edgelist")
teacher_igraph <- graph_from_adjacency_matrix(as.matrix.network(teacher_ties))

save(teacher_igraph, file = here("data/teacher_igraph"))

### descriptive statistics
is_directed(teacher_igraph) # yes, this graph is DIRECTED

# the number of nodes
vcount(teacher_igraph) 
# 1153 nodes

# the number of edges
ecount(teacher_igraph)
# 1236 edges

# count of potential links:
# formula is n * (n - 1), with n being # of nodes
n = 1153
n * (n - 1)
# we have 1,328,256 potential links!

# density of network
edge_density(teacher_igraph)
# 0.0009305435


# k-core stuff?
detach('package:statnet', unload = TRUE)
library(igraph)

kcore_teachers <-
  teacher_igraph %>% graph.coreness(.)
kcore_teachers ## show the results of k-core decomposition

# visualization? This code has to be adjusted to make the graph more readable.
teacher_igraph %>%
  plot(
    .,
    layout = layout_with_gem(.),
    # layout = layout_with_sugiyama(.),
    edge.arrow.size = .3,
    vertex.size = 2,
    vertex.label = NA,
    vertex.color = adjustcolor(graph.coreness(.), alpha.f = .3),
    vertex.label.cex = .5,
    vertex.label.color = 'black',
    mark.groups = by(seq_along(graph.coreness(.)), graph.coreness(.), invisible),
    mark.shape = 1 / 4,
    mark.col = rainbow(length(unique(graph.coreness(
      .
    ))), alpha = .1),
    mark.border = NA
  )

# community detection algorithm & stats
cluster_teachers <- teacher_igraph %>% cluster_walktrap()

membership(cluster_teachers)
length(cluster_teachers)
sizes(cluster_teachers) 

modularity(cluster_teachers) # score is 0.845919 so it's pretty good.
# cluster 5 is pretty interesting, 199!!!!!
# bruh.

# plot these communities
set.seed(50)

cluster_teachers %>% plot(
  .,
  teacher_igraph,
  # layout = layout_with_gem(.),
  layout = layout_with_drl(teacher_igraph),
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
# for future coding: consider marking off the communities by INSTRUMENT and see if it works...

# degree distribution
teacher_igraph %>% degree.distribution(., mode = "in") %>%
  plot(
    .,
    col = 'black',
    pch = 19,
    cex = 1.5,
    main = 'In-degree Distribution',
    ylab = 'Density',
    xlab = 'In-degree'
  )

in_power_teacher <- teacher_igraph %>%
  degree.distribution(., mode = 'in') %>%
  fit_power_law(.)
in_power_teacher

# what type of network is it? (small world, caveman, spaceman?)
# by analyzing this, we can conclude yada yada yada

ntrials <- 1000

cl.rg_teacher <-
  numeric(ntrials) ## create an estimated value holder for clustering coefficient
apl.rg_teacher <-
  numeric(ntrials) ## create an estimated value holder for average path length
for (i in (1:ntrials)) {
  g.rg <- rewire(teacher_igraph, keeping_degseq(niter = 100))
  cl.rg_teacher[i] <- transitivity(g.rg, type = 'average')
  apl.rg_teacher[i] <- mean_distance(g.rg)
}

# clustering coefficient
hist(cl.rg_teacher,
     main = 'Histogram of Clustering Coefficient',
     xlab = 'Clustering Coefficient',
     xlim = c(0, 0.1))
par(xpd = FALSE)
abline(
  v = teacher_igraph %>% transitivity(., type = 'average'),
  col = 'red',
  lty = 2
)
t.test(
  cl.rg_teacher,
  mu = teacher_igraph %>% transitivity(., type = 'average'),
  alternative = 'less'
)

# average path length
hist(apl.rg_teacher,
     main = 'Histogram of Average Path Length',
     xlab = 'Average Path Length',
     xlim = c(1, 1.4))
abline(v = teacher_igraph %>% mean_distance(),
       col = 'red',
       lty = 2)
t.test(apl.rg_teacher,
       mu = teacher_igraph %>% mean_distance(.),
       alternative = 'greater')


### ANALYSIS ON SCHOOLS -----------



