#
# Create a new script 'CurveBall_Analysis.R' from here including all food webs
# Curveball


## Load packages ----

packages <- c("dplyr", "multiweb", "ggplot2", "ggjoy", "igraph")
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
  left_join(metadata)


# Save data
save(g_list_ok, CB, all_CB,
     file = "Results/curveball.rda")


# This is not necessary. Follow the results in 'all_CB' df
load("Results/curveball.rda")
load("Data/all_igraph.rda")

link <- aggregate(all_CB$Links, list(all_CB$Network), FUN=mean)

l_all<-as.data.frame(t(lapply(g_all_ok,ecount)))
l_all<- t(l_all)
colnames(l_all) <- c("Links")
write.csv(l_all,"links.csv")


# Add latitude, longitude, and region

all_CB <- all_CB %>% #Los valores de links no están actualizados#
  mutate(Latitude = case_when(Links == 127 ~ "-11.25", Links == 191 ~ "60.25",
                              Links == 848 ~ "78.56631", Links == 1546 ~ "73.10794444",
                              Links == 88 ~ "-12.18566667", Links == 203 ~ "-34.10097222", Links == 3373 ~ "18.01375", 
                              Links == 3766 ~ "19.40069444", Links == 169 ~ "49.09280556", Links == 1362 ~ "-36.05547222", 
                              Links == 3874 ~ "21.70027778", Links == 221 ~ "30.03788889", Links == 410 ~ "36.98222222",
                              Links == 189 ~ "42.43833333", Links == 643 ~ "3.613805556",
                              Links == 4105 ~ "17.76119444", Links == 173 ~ "-50",
                              Links == 198 ~ "12.34859167", Links == 79 ~ "36.79786111",
                              Links == 1490 ~ "43.82688889", Links == 70 ~ "42.31197222",
                              Links == 840 ~ "-25.704", Links == 202 ~ "-35.37338889",
                              Links == 1115 ~ "-54.85", Links == 649 ~ "-62.23333333",
                              Links == 1804 ~ "54.43963889", Links == 6774 ~ "54.43963889",
                              Links == 1972 ~ "-74.43263889")) %>%
  dplyr::select(Latitude, everything())
all_CB$Latitude <- as.numeric(all_CB$Latitude)

all_CB <- all_CB %>% #Los valores de links no están actualizados#
  mutate(Longitude = case_when(Links == 127 ~ "13.41666667", Links == 191 ~ "20",
                             Links == 848 ~ "46.60169444", Links == 1546 ~ "24.98061111",
                             Links == 88 ~ "-77.39558333", Links == 203 ~ "18.00222222", Links == 3373 ~ "65.584806", 
                             Links == 3766 ~ "-81.30005556", Links == 169 ~ "-8.168527778", Links == 1362 ~ "-74.509", 
                             Links == 3874 ~ "-80.32338889", Links == 221 ~ "-84.12511111", Links == 410 ~ "-7.215944444",
                             Links == 189 ~ "3.165", Links == 643 ~ "-77.50480556",
                             Links == 4105 ~ "-76.82972222", Links == 173 ~ "70",
                             Links == 198 ~ "-71.31281389", Links == 79 ~ "-121.9085278",
                             Links == 1490 ~ "-68.27191667", Links == 70 ~ "-8.62925",
                             Links == 840 ~ "-48.082", Links == 202 ~ "153.2627778",
                             Links == 1115 ~ "-68.56666667", Links == 649 ~ "-58.66666667",
                             Links == 1804 ~ "-162.6478333", Links == 6774 ~ "-162.6478333",
                             Links == 1972 ~ "-21.77463889")) %>% 
  dplyr::select(Longitude, everything())
all_CB$Longitude <- as.numeric(all_CB$Longitude)

all_CB <- all_CB %>% #Los valores de links no están actualizados#
  mutate(Region = case_when(Links == 127 ~ "Subtropical", Links == 191 ~ "Temperate",
                             Links == 848 ~ "Polar", Links == 1546 ~ "Temperate",
                             Links == 88 ~ "Subtropical", Links == 203 ~ "Temperate", Links == 3373 ~ "Tropical", 
                             Links == 3766 ~ "Tropical", Links == 169 ~ "Temperate", Links == 1362 ~ "Temperate", 
                             Links == 3874 ~ "Tropical", Links == 221 ~ "Tropical", Links == 410 ~ "Tropical",
                             Links == 189 ~ "Temperate", Links == 643 ~ "Tropical",
                             Links == 4105 ~ "Tropical", Links == 173 ~ "Subpolar",
                             Links == 198 ~ "Tropical", Links == 79 ~ "Temperate",
                             Links == 1490 ~ "Temperate", Links == 70 ~ "Temperate",
                             Links == 840 ~ "Subtropical", Links == 202 ~ "Temperate",
                             Links == 1115 ~ "Subpolar", Links == 649 ~ "Polar",
                             Links == 1804 ~ "Polar", Links == 6774 ~ "Polar",
                             Links == 1972 ~ "Polar")) %>% 
  dplyr::select(Region, everything())

## Plots ----

# Modularity by connectance
regioncolor<-scale_color_manual(labels = c("Polar", "Subpolar", "Temperate", "Subtropical",  "Tropical"), 
                                values = c("#1984c5", "#a7d5ed","gray50","#e1a692","#c23728"))


ggplot(all_CB, aes(x = Modularity, y = reorder(Network, Connectance)), fill=Region) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Modularity", y = "Food web (increasing connectance)") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Modularity by latitude
ggplot(all_CB, aes(x = Modularity, y = reorder(Network, Latitude))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing latitude") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))


# Generality by connectance
ggplot(all_CB, aes(x = Generality, y = reorder(Network, Connectance))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing connectance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Generality by latitude
ggplot(all_CB, aes(x = Generality, y = reorder(Network, Latitude))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing latitude") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Path length by connectance
ggplot(all_CB, aes(x = PathLength, y = reorder(Network, Connectance))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing connectance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))


# Path length by latitude
ggplot(all_CB, aes(x = PathLength, y = reorder(Network, Latitude))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing latitude") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Clustering by connectance
ggplot(all_CB, aes(x = Clustering, y = reorder(Network, Connectance))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing connectance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Clustering by latitude
ggplot(all_CB, aes(x = Clustering, y = reorder(Network, Latitude))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing latitude") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Vulnerability by connectance
ggplot(all_CB, aes(x = Vulnerability, y = reorder(Network, Connectance))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing connectance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

# Vulnerability by latitude
ggplot(all_CB, aes(x = Vulnerability, y = reorder(Network, Latitude))) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "", y = "Food web (increasing latitude") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

