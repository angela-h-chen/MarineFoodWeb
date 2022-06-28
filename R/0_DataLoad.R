#
## Analysis of complexity and structure for a set of highly-resolved marine food webs
# Authors: Angela Chen & Tomas I. Marina
# Objective: Load networks and convert it to igraph objects
#


# Load packages ----
library(data.table)
library(tidyverse)


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


# From edge list







