
####Install Packages ----
# install.packages("NetIndices")
# install.packages("bipartite")
# install.packages("devtools")
# install_github("lsaravia/multiweb")
# install.packages("sna")

###Load libraries and all_lists.rda file 

setwd("C://Users/Angela/OneDrive/Desktop/ISP/R")
load(file="../Data/.all_igraph.rda")
library(dplyr)
library(tidyr)
library(bipartite)
library(NetIndices)
library(multiweb)


###Calculate complexity properties (S, L, LD, Co) ----

library(igraph)
s_all<-lapply(g_all,vcount)
l_all<-lapply(g_all,ecount)
ld_all<-lapply(g_all, function(x) {
  ld<-round(ecount(x)/vcount(x),3)
})
co_all<-lapply(g_all, function(x) {
  ld<-round(ecount(x)/vcount(x)^2,3)
})

###Bind complexity properties in a data frame and add region
library(dplyr)
complexity<-data.frame(cbind(s_all,l_all,ld_all,co_all))
colnames(complexity)<-c("Especies","Interacciones","Densidad","Conectividad")
complexity<-complexity %>%
  mutate(Region=c("Polar","Subtropical","Templado","Polar","Templado","Subpolar","Templado",
                  "Subtropical","Templado","Templado","Templado","Templado","Tropical","Tropical",
                  "Tropical","Subpolar","Tropical","Tropical","Templado","Templado","Templado",
                  "Subtropical","Polar","Polar","Polar","Templado","Tropical","Polar")) %>%
  mutate(across(!Region, as.numeric))

complexity$Location<-rownames(complexity)


###Calculate structure properties ---- 


## Modularity
# Get largest connected component
compo <- lapply(g_all, function(x){
  components <- igraph::clusters(x, mode="weak")
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- V(x)[components$membership == biggest_cluster_id]
  g_largest <- igraph::induced_subgraph(x, vert_ids)
})

# Calculate modularity
mod_all <- lapply(compo, function(x){
  modulos <- cluster_spinglass(x)
  mti <- tibble(Name=names(modulos), Groups=length(modulos$csize), Modularity=modulos$modularity)
  bind_rows(mti, .id = "Location")
  })

mod_all_df <- bind_rows(mod_all, .id = "Location")

modu_all<-aggregate(mod_all_df$Modularity,list(mod_all_df$Location), FUN=mean)
rownames(modu_all)<-modu_all$Group.1
colnames(modu_all)<- c("Location","Modularity")


#calculate topological features (path length, trophic level, omnivory, etc)

library(multiweb)
topo_all<-lapply(g_all,function(x) {
  topo<-calc_topological_indices(x)
})
topo_all_df<-bind_rows(topo_all)
rownames(topo_all_df)<-names(g_all)
  

###Combine structural dimensions into one data frame. Add C and region for graphing


structure<-round(cbind(topo_all_df$Omnivory,topo_all_df$TLmean,topo_all_df$PathLength,
                 modu_all$Modularity),3)
colnames(structure)<-c('Omnivory','TLmean','PathLength','Modularity')
structure<-data.frame(structure)
structure$Conectividad <- complexity$Conectividad
structure$Region <- complexity$Region
structure$Location <- complexity$Location

# ###Calculate Trophic level and omnivory index for all species ----

troph_all <- lapply(g_all, function(x){
  m <- get.adjacency(x, sparse=FALSE)
  tl <- TrophInd(m)
})

troph_all_df <- bind_rows(troph_all, .id = "Location")

####Calculate degree distribution ----

library(tidyr)
library(tidyverse)

deg_all <- lapply(g_all, function(x) {
  deg <- degree(x)
})


deg_all_df <- deg_all %>%
  unlist(recursive=FALSE) %>%
  enframe() %>%
  extract(name, into = c("Location", "Species"), "^([^.]+)\\.(.*)") %>%
  rename(Degree = value)

###Add region to degree distribution plot
deg_all_df$Region <- NA
for(i in 1:nrow(deg_all_df)) {
  if (deg_all_df[i,"Location"] == "Angola") {
    deg_all_df[i, "Region"]<- "Subtropical"
  }
  if (deg_all_df[i,"Location"] == "Baltic") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "BarentsArctic") {
    deg_all_df[i, "Region"]<- "Polar"
  }
  if (deg_all_df[i,"Location"] == "BarentsBoreal") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Benguela") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Brazil") {
    deg_all_df[i, "Region"]<- "Subtropical"
  }
  if (deg_all_df[i,"Location"] == "Cadiz") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Cayman") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Celtic") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Chile") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Cuba") {
    deg_all_df[i, "Region"]<- "Tropical"
  }
  if (deg_all_df[i,"Location"] == "Florida") {
    deg_all_df[i, "Region"]<- "Tropical"
  }
  if (deg_all_df[i,"Location"] == "Jamaica") {
    deg_all_df[i, "Region"]<- "Tropical"
  }
  if (deg_all_df[i,"Location"] == "Kerguelen") {
    deg_all_df[i, "Region"]<- "Subpolar"
  }
  if (deg_all_df[i,"Location"] == "LaGuajira") {
    deg_all_df[i, "Region"]<- "Tropical"
  }
  if (deg_all_df[i,"Location"] == "Lion") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Monterey") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "NEUS") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Pacific") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Peru") {
    deg_all_df[i, "Region"]<- "Subtropical"
  }
  if (deg_all_df[i,"Location"] == "Simon") {
    deg_all_df[i, "Region"]<- "Templado"
  }
  if (deg_all_df[i,"Location"] == "Tortuga") {
    deg_all_df[i, "Region"]<- "Tropical"
  }
  if (deg_all_df[i,"Location"] == "Alaska") {
    deg_all_df[i, "Region"]<- "Polar"
  }
  if (deg_all_df[i,"Location"] == "Beagle") {
    deg_all_df[i, "Region"]<- "Subpolar"
  }
  if (deg_all_df[i,"Location"] == "Potter") {
    deg_all_df[i, "Region"]<- "Polar"
  }
  if (deg_all_df[i,"Location"] == "SanakIntertidal") {
    deg_all_df[i, "Region"]<- "Polar"
  }
  if (deg_all_df[i,"Location"] == "SanakNearshore") {
    deg_all_df[i, "Region"]<- "Polar"
  }
  if (deg_all_df[i,"Location"] == "Weddell") {
    deg_all_df[i, "Region"]<- "Polar"
  }
}

##Order degree distribution plot by connectance
deg_all_df$Conectividad <- NA
for(i in 1:nrow(deg_all_df)) {
  if (deg_all_df[i,"Location"] == "Angola") {
    deg_all_df[i, "Conectividad"]<- "0.162"
  }
  if (deg_all_df[i,"Location"] == "Baltic") {
    deg_all_df[i, "Conectividad"]<- "0.175"
  }
  if (deg_all_df[i,"Location"] == "BarentsArctic") {
    deg_all_df[i, "Conectividad"]<- "0.034"
  }
  if (deg_all_df[i,"Location"] == "BarentsBoreal") {
    deg_all_df[i, "Conectividad"]<- "0.048"
  }
  if (deg_all_df[i,"Location"] == "Benguela") {
    deg_all_df[i, "Conectividad"]<- "0.252"
  }
  if (deg_all_df[i,"Location"] == "Brazil") {
    deg_all_df[i, "Conectividad"]<- "0.041"
  }
  if (deg_all_df[i,"Location"] == "Cadiz") {
    deg_all_df[i, "Conectividad"]<- "0.232"
  }
  if (deg_all_df[i,"Location"] == "Cayman") {
    deg_all_df[i, "Conectividad"]<- "0.054"
  }
  if (deg_all_df[i,"Location"] == "Celtic") {
    deg_all_df[i, "Conectividad"]<- "0.073"
  }
  if (deg_all_df[i,"Location"] == "Chile") {
    deg_all_df[i, "Conectividad"]<- "0.121"
  }
  if (deg_all_df[i,"Location"] == "Cuba") {
    deg_all_df[i, "Conectividad"]<- "0.055"
  }
  if (deg_all_df[i,"Location"] == "Florida") {
    deg_all_df[i, "Conectividad"]<- "0.096"
  }
  if (deg_all_df[i,"Location"] == "Jamaica") {
    deg_all_df[i, "Conectividad"]<- "0.058"
  }
  if (deg_all_df[i,"Location"] == "Kerguelen") {
    deg_all_df[i, "Conectividad"]<- "0.221"
  }
  if (deg_all_df[i,"Location"] == "LaGuajira") {
    deg_all_df[i, "Conectividad"]<- "0.272"
  }
  if (deg_all_df[i,"Location"] == "Lion") {
    deg_all_df[i, "Conectividad"]<- "0.124"
  }
  if (deg_all_df[i,"Location"] == "Monterey") {
    deg_all_df[i, "Conectividad"]<- "0.058"
  }
  if (deg_all_df[i,"Location"] == "NEUS") {
    deg_all_df[i, "Conectividad"]<- "0.227"
  }
  if (deg_all_df[i,"Location"] == "Pacific") {
    deg_all_df[i, "Conectividad"]<- "0.017"
  }
  if (deg_all_df[i,"Location"] == "Peru") {
    deg_all_df[i, "Conectividad"]<- "0.042"
  }
  if (deg_all_df[i,"Location"] == "Simon") {
    deg_all_df[i, "Conectividad"]<- "0.096"
  }
  if (deg_all_df[i,"Location"] == "Tortuga") {
    deg_all_df[i, "Conectividad"]<- "0.01"
  }
  if (deg_all_df[i,"Location"] == "Alaska") {
    deg_all_df[i, "Conectividad"]<- "0.006"
  }
  if (deg_all_df[i,"Location"] == "Beagle") {
    deg_all_df[i, "Conectividad"]<- "0.053"
  }
  if (deg_all_df[i,"Location"] == "Potter") {
    deg_all_df[i, "Conectividad"]<- "0.054"
  }
  if (deg_all_df[i,"Location"] == "SanakIntertidal") {
    deg_all_df[i, "Conectividad"]<- "0.033"
  }
  if (deg_all_df[i,"Location"] == "SanakNearshore") {
    deg_all_df[i, "Conectividad"]<- "0.026"
  }
  if (deg_all_df[i,"Location"] == "Weddell") {
    deg_all_df[i, "Conectividad"]<- "0.01"
  }
}


###save lists to rda file
save(structure, 
     g_all, 
     complexity,
     deg_all_df,
     troph_all_df,
     file="../Data/.for_graphing.rda")


