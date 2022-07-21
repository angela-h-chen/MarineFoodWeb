
library(multiweb)
library(igraph)
library(dplyr)
library(measurements)
library(ggplot2)

# Load data ----

load(file="Data/all_igraph.rda")
metadata <- read.csv("Data/Metadata_FW.csv")


# Analysis ----

## Get largest connected component

g_all_ok <- lapply(g_all, function(x){
  components <- igraph::clusters(x, mode="weak")
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- V(x)[components$membership == biggest_cluster_id]
  g_largest <- igraph::induced_subgraph(x, vert_ids)
})


## Calculate structure properties & stability (QSS)

data <- bind_cols(calc_topological_indices(g_all_ok), calc_modularity(g_all_ok), 
                  calc_QSS(g_all_ok, nsim = 1000, ncores = 4, istrength = FALSE, returnRaw = FALSE)) %>% 
  mutate(Network = names(g_all_ok)) 
data <- data %>% 
  dplyr::select(Network, everything())

#QSS <- calc_QSS(g_all_ok, nsim = 1000, ncores = 4, istrength = FALSE, returnRaw = TRUE)

# Add Metadata
# First, convert latitude/longitude to decimals
metadata <- metadata %>% 
  rename(Network = FoodWeb) %>% 
  mutate(Longitude = conv_unit(Longitude, from = "deg_min_sec", to = "dec_deg"),
         Latitude = conv_unit(Latitude, from = "deg_min_sec", to = "dec_deg"))

fw_results <- data %>% 
  left_join(metadata)


save(g_all_ok, metadata, data, fw_results,
     file = "Results/comp_str_stab.rda")


## Exploratory plots

ggplot(fw_results, aes(x = Connectance, y = MEing)) +
  geom_point()

