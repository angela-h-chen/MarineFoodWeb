#
# Curveball analysis to compare food webs
# 


## Load packages ----

packages <- c("dplyr", "multiweb", "ggplot2", "ggjoy", "igraph","measurements")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


## Load data ----

load(file="Data/all_igraph.rda")
load(file="Results/comp_str_stab.rda")


## Largest connected component ----

g_all_ok <- lapply(g_all, function(x){
  components <- igraph::clusters(x, mode="weak")
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- V(x)[components$membership == biggest_cluster_id]
  g_largest <- igraph::induced_subgraph(x, vert_ids)
})


## Curveball & properties ----

g_list_ok <- within(g_all_ok, rm("Gulf Alaska"))  # discard Gulf Alaska (too many basal sp)
CB <- lapply(g_list_ok, function(x) {
  print(vcount(x))
  curve <- curveBall(x, nsim=1000, istrength = FALSE)
  IC <- bind_cols(calc_topological_indices(curve, ncores = 4), calc_modularity(curve, ncores = 4))

})


# Convert curveball list into data frame
# and add network's name, latitude, longitude & region

all_CB <- bind_rows(CB, .id = "Network") %>% 
  left_join(Metadata_FW)

all_CB <- all_CB %>% 
  mutate(Longitude = as.numeric(conv_unit(Longitude, from = "deg_min_sec", to = "dec_deg")),
         Latitude = as.numeric(conv_unit(Latitude, from = "deg_min_sec", to = "dec_deg")))

all_CB$Lat_abs <- abs(all_CB$Latitude)


# Make individual dataframe for connectance for graphing
CoLat <- aggregate(cbind(Connectance, Latitude) ~ Network, all_CB, mean)

ggplot(CoLat, aes(x=Connectance,y=Latitude, color=Region))+
  geom_point()

# Save data
save(g_list_ok, CB, all_CB,
     file = "Results/curveball.rda")


## Plots ----

load("Results/curveball.rda")

# Creating color and stylistic themes for geom_joy plos 
all_CB$Region <- factor(all_CB$Region, levels = c("Polar", "Subpolar", 
                                                  "Temperate", "Subtropical", 
                                                  "Tropical"))


regionfill<-scale_fill_manual(labels = c("Polar", "Subpolar", "Temperate", 
                                          "Subtropical",  "Tropical"), 
                               values = c("#1984c5", "#a7d5ed","gray50",
                                          "#e1a692","#c23728"))

geomjoy_theme <- theme(panel.grid = element_blank(),
                       axis.title = element_text(size = 18, face = "bold"),
                       axis.text.x = element_text(size = 10),
                       axis.text.y = element_text(size = 15))

boxplot_theme <- theme(panel.grid = element_blank(),
                       axis.title = element_text(size = 18, face = "bold"),
                       axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust=1),
                       axis.text.y = element_text(size = 15))


### Modularity ----
# Modularity by connectance
ggplot(all_CB, aes(x = Modularity, y = reorder(Network, Connectance), fill=Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Modularity", y = "Food web (increasing C)") +
  regionfill+
  geomjoy_theme

# Modularity by latitude
ggplot(all_CB, aes(x = Modularity, y = reorder(Network, Latitude), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Modularity", y = "Food web (increasing latitude") +
  regionfill+
  geomjoy_theme


### Generality ----
# Generality by connectance
ggplot(all_CB, aes(x = Generality, y = reorder(Network, Connectance), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Generality", y = "Food web (increasing connectance") +
  regionfill+
  geomjoy_theme

# Generality by latitude
ggplot(all_CB, aes(x = Generality, y = reorder(Network, Latitude), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Generality", y = "Food web (increasing latitude") +
  regionfill+
  geomjoy_theme


### Path Length ----
# Path length by connectance
ggplot(all_CB, aes(x = PathLength, y = reorder(Network, Connectance), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Path Length", y = "Food web (increasing connectance") +
  regionfill+
  geomjoy_theme

# Path length by latitude
ggplot(all_CB, aes(x = PathLength, y = reorder(Network, Latitude), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Path Length", y = "Food web (increasing latitude") +
  regionfill+
  geomjoy_theme


### Clustering ----
# Clustering by connectance
ggplot(all_CB, aes(x = Clustering, y = reorder(Network, Connectance), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Clustering", y = "Food web (increasing connectance") +
  regionfill+
  geomjoy_theme

# Clustering by latitude
ggplot(all_CB, aes(x = Clustering, y = reorder(Network, Latitude), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Clustering", y = "Food web (increasing latitude") +
  regionfill+
  geomjoy_theme


### Vulnerability ----
# Vulnerability by connectance
ggplot(all_CB, aes(x = Vulnerability, y = reorder(Network, Connectance), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Vulnerability", y = "Food web (increasing connectance") +
  regionfill+
  geomjoy_theme

# Vulnerability by latitude
ggplot(all_CB, aes(x = Vulnerability, y = reorder(Network, Latitude), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Vulnerability", y = "Food web (increasing latitude") +
  regionfill+
  geomjoy_theme


#
ggplot(all_CB, aes(x = Modularity, y = reorder(Network, fw_results$Clustering), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Vulnerability", y = "Food web (increasing latitude") +
  regionfill+
  geomjoy_theme


ggplot(fw_results, aes(x = Modularity, y = Clustering, color = Region))+
  geom_point()


## Boxplots by latitude ----

ggplot(all_CB, aes(x = reorder(Network, Lat_abs), y = Generality, fill = Region)) +
  geom_boxplot() +
  geom_smooth(method = "lm", color="black", aes(group=1)) +
  theme_classic() +
  labs(x = "Food web (increasing latitude)", y = "Generality") +
  regionfill+
  boxplot_theme


