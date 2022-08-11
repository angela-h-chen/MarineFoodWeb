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
# Then look at 'maxre': the closer to zero, the relatively more stable is the network
# More info in 'calc_QSS' help & original paper: Allesina & Pascual 2008, https://doi.org/10.1007/s12080-007-0007-8

QSS_raw <- calc_QSS(g_all_ok, nsim = 1000, ncores = 4, istrength = FALSE, returnRaw = TRUE)
QSS_raw$Network <- rep(names(g_all_ok), each = 1000)


# Save
save(g_all_ok, QSS_raw,
     file = "Results/Stability.rda")


## Exploratory plots ----
load(file="Results/Stability.rda")
load(file="Data/Metadata_FW.csv")

# Add region to QSS_raw

QSS_raw <- QSS_raw %>%
  left_join(Metadata_FW, by = "Network")
  

# QSS x connectance

QSS_raw$Region <- factor(QSS_raw$Region, levels = c("Polar", "Subpolar", 
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


ggplot(QSS_raw, aes(x = maxre, y = reorder(Network, Connectance), fill = Region)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "QSS", y = "Food web (increasing connectance") +
  regionfill+
  geomjoy_theme

