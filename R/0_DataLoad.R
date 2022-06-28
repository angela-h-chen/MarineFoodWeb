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
temp <- list.files(path = "./Data", pattern="*.csv", full.names = TRUE)
myfiles <- lapply(temp, read_csv)
names(myfiles) <- c("Angola", "Baltic Sea", "Barents Sea Arctic", "Barents Sea Boreal",
                    "Beach Peru", "Beagle Channel", "Benguela", "Cayman Is", "Celtic Sea",
                    "Chilean rocky", "Cuba", "Florida", "Gulf Alaska", "Gulf Cadiz",
                    "Gulf Lions", "Gulf Tortugas", "Jamaica", "Kerguelen Plateau",
                    "La Guajira", "Metadata", "Monterey Bay", "NE US Shelf", "Potter Cove",
                    "Sanak intertidal", "Sanak nearshore", "Simon Bay", "Southern Brazil",
                    "SW Pacific Ocean", "Weddell Sea")
mynetworks <- within(myfiles, rm(Metadata))  # remove 'Metadata' item


# Convert to igraph objects ----
# From adjacency matrix
adj_l <- within(mynetworks, rm("Beagle Channel", "Gulf Alaska", "Potter Cove",
                              "Sanak intertidal", "Sanak nearshore", "Weddell Sea"))
adj_to_g <- function(x){
  to_m <- lapply(x, as.matrix)
  col_dis <- lapply(to_m, function(z) z[, -1])
  g_adj <- lapply(col_dis, graph_from_adjacency_matrix)
}
g_adj <- adj_to_g(adj_l)

m_b <- adj_l[["Baltic Sea"]]
m_b <- as.matrix(m_b)[,-1]
names <- colnames(m_b)
m_t <- t(m_b)
colnames(m_t) <- names

g_b <- graph_from_adjacency_matrix(m_t)


# From edge list
edge_l <- mynetworks[c("Beagle Channel", "Gulf Alaska", "Potter Cove",
                        "Sanak intertidal", "Sanak nearshore", "Weddell Sea")]
edge_to_g <- function(x){
  to_m <- lapply(x, as.matrix)
  to_g <- lapply(to_m, graph_from_edgelist)
  }
g_edge <- edge_to_g(edge_l)





