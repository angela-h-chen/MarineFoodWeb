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
setwd("C:/Users/Angela/MarineFoodWeb/Data")
temp <- list.files(pattern="*.csv", full.names = TRUE)
myfiles <- lapply(temp, read_csv)
names(myfiles) <- c("Angola", "Baltic Sea", "Barents Sea Arctic", "Barents Sea Boreal",
                    "Beach Peru", "Beagle Channel", "Benguela", "Caribbean Reef","Cayman Is", 
                    "Celtic Sea","Chilean rocky", "Cuba", "Florida", "Gulf Alaska", "Gulf Cadiz",
                    "Gulf Lions", "Gulf Tortugas", "Jamaica", "Kerguelen Plateau",
                    "La Guajira", "Metadata", "Monterey Bay", "NE US Shelf", "Potter Cove",
                    "Sanak intertidal", "Sanak nearshore", "Simon Bay", "Southern Brazil",
                    "SW Pacific Ocean", "Weddell Sea")
mynetworks <- within(myfiles, rm(Metadata))  # remove 'Metadata' item


# Convert to igraph objects ----

## From adjacency matrix ----

adj_l <- within(mynetworks, rm("Beagle Channel", "Gulf Alaska", "Potter Cove",
                              "Sanak intertidal", "Sanak nearshore", "Weddell Sea",
                              "Cayman Is", "Cuba", "Jamaica"))  # discard networks as edge lists
# Transpose matrix for "Baltic Sea" & "Southern Brazil"
m_sb <- adj_l[["Southern Brazil"]]  # m_b "Baltic Sea"
m_sb <- as.matrix(m_sb)[,-1]  # m_b
names <- colnames(m_sb)  # m_b
m_t_sb <- t(m_sb)  # m_t_b m_b
colnames(m_t_sb) <- names  # m_t_b

m_b <- adj_l[["Baltic Sea"]]  # m_b "Baltic Sea"
m_b <- as.matrix(m_b)[,-1]  # m_b
names <- colnames(m_b)  # m_b
m_t_b <- t(m_b)  # m_t_b m_b
colnames(m_t_b) <- names  # m_t_b


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
g_adj[["Baltic Sea"]] <- g_b
g_adj[["Southern Brazil"]] <- g_sb


## From edge list ----

edge_l <- mynetworks[c("Beagle Channel", "Gulf Alaska", "Potter Cove",
                       "Sanak intertidal", "Sanak nearshore", "Weddell Sea",
                       "Cayman Is", "Cuba", "Jamaica")]
edge_to_g <- function(x){
  to_m <- lapply(x, as.matrix)
  to_g <- lapply(to_m, graph_from_edgelist)
  }
g_edge <- edge_to_g(edge_l)


# Append g lists ----

g_all <- append(g_adj, g_edge)


# Save data ----

save(g_adj, g_edge, g_all,
     file = "Data/all_igraph.rda")


