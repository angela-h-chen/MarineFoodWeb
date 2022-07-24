#
## Analysis of complexity and structure for a set of highly-resolved marine food webs
# Authors: Angela Chen & Tomas I. Marina
# Objective: Load networks and convert it to igraph objects
#


# Load packages ----
library(data.table)
library(tidyverse)
library(igraph)

# Load data ----

#setwd("C:/Users/Angela/MarineFoodWeb/Data")   

temp <- list.files(path = "Data", pattern="*.csv", full.names = TRUE)  # I added the path to 'Data' directory
myfiles <- lapply(temp, read_csv)
names(myfiles) <- c("Angola", "Baltic", "BarentsArctic", "BarentsBoreal",
                    "Peru", "Beagle", "Benguela", "Caribbean","Cayman", 
                    "Celtic","Chile", "Cuba", "Florida", "Alaska", "Cadiz",
                    "Lions", "Tortugas", "Jamaica", "Kerguelen",
                    "LaGuajira", "Metadata", "Monterey", "NEUS", "Potter",
                    "SanakIntertidal", "SanakNearshore", "Simon", "Brazil",
                    "Pacific", "Weddell")
mynetworks <- within(myfiles, rm(Metadata))  # remove 'Metadata' item


# Convert to igraph objects ----

## From adjacency matrix ----

adj_l <- within(mynetworks, rm("Beagle", "Alaska", "Potter",
                              "SanakIntertidal", "SanakNearshore", "Weddell",
                              "Cayman", "Cuba", "Jamaica"))  # discard networks as edge lists
# Transpose matrix for "Baltic Sea" & "Southern Brazil"
m_sb <- adj_l[["Brazil"]]
m_sb <- as.matrix(m_sb)[,-1]
names <- colnames(m_sb)
m_t_sb <- t(m_sb)
colnames(m_t_sb) <- names

m_b <- adj_l[["Baltic"]]
m_b <- as.matrix(m_b)[,-1]
names <- colnames(m_b)
m_t_b <- t(m_b)
colnames(m_t_b) <- names

# Convert matrix to igraph
g_b <- graph_from_adjacency_matrix(m_t_b, mode = "directed")  # Baltic Sea
g_sb <- graph_from_adjacency_matrix(m_t_sb, mode = "directed")  # Southern Brazil

# Function to convert adj matrix to g objects
adj_to_g <- function(x){
  to_m <- lapply(x, as.matrix)
  col_dis <- lapply(to_m, function(z) z[, -1])
  g_adj <- lapply(col_dis, graph_from_adjacency_matrix)
}
g_adj <- adj_to_g(adj_l)

# Replace g objects for "Baltic Sea" & "Southern Brazil"
g_adj[["Baltic"]] <- g_b
g_adj[["Brazil"]] <- g_sb


## From edge list ----

edge_l <- mynetworks[c("Beagle", "Alaska", "Potter",
                       "SanakIntertidal", "SanakNearshore", "Weddell",
                       "Cayman", "Cuba", "Jamaica")]
edge_to_g <- function(x){
  to_m <- lapply(x, as.matrix)
  to_g <- lapply(to_m, graph_from_edgelist)
  }
g_edge <- edge_to_g(edge_l)


# Append g lists and sorting alphabetically ----

g_all <- append(g_adj, g_edge)
g_all<-g_all[order(names(g_all))]


# Save data ----

save(g_adj, g_edge, g_all,
     file = "all_igraph.rda")
