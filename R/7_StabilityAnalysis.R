#
## Stability analysis: Quasi-Sign Stability (QSS)
#


## Load packages ----

packages <- c("multiweb", "ggplot2", "igraph", "dplyr")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


## Load data ----

load(file="Data/all_igraph.rda")

# Get largest component
g_all_ok <- lapply(g_all, function(x){
  components <- igraph::clusters(x, mode="weak")
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- V(x)[components$membership == biggest_cluster_id]
  g_largest <- igraph::induced_subgraph(x, vert_ids)
})


## Stability analysis (QSS) ----
# QSS is always zero meaning the proportion of locally stable matrices is zero
# Then look at 'MEing': the closer to zero, the relatively more stable is the network

QSS <- calc_QSS(g_all_ok, nsim = 1000, ncores = 4, istrength = FALSE, returnRaw = FALSE) %>% 
  mutate(Network = names(g_all_ok))
QSS


# Save
save(g_all_ok, QSS,
     file = "Results/Stability.rda")
