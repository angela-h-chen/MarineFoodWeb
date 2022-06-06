
# Load libraries ----
load(file="../Data/.all_igraph.rda")
load(file="../Data/.for_graphing.rda")

# Degree, Closeness, and Betweenness ----

## Beagle ----
library(igraph)
library(dplyr)

b_clo <- igraph::closeness(g_all[["Beagle"]], mode = c("total"))
b_in <- degree(g_all[["Beagle"]], mode = c("in"))
b_out <- degree(g_all[["Beagle"]], mode = c("out"))
b_deg <- degree(g_all[["Beagle"]])
b_bet <- igraph::betweenness(g_all[["Beagle"]])
b_index <- cbind(b_clo,b_deg,b_bet,b_in,b_out)
colnames(b_index)<-c("Closeness","Degree","Betweenness", "InDegree", "OutDegree")

b_index_df <- data.frame(b_index)

b_index_df <- b_index_df %>% 
  mutate(rank=dense_rank(desc(-Closeness)))
names(b_index_df)[names(b_index_df) == 'rank'] <- 'C_rank'

deg_rank <- b_index_df %>% 
  mutate(rank=dense_rank(desc(Degree)))
names(deg_rank)[names(deg_rank) == 'rank'] <- 'D_rank'

bet_rank <- b_index_df %>% 
  mutate(rank=dense_rank(desc(Betweenness)))
names(bet_rank)[names(bet_rank) == 'rank'] <- 'B_rank'

b_index_df <- cbind(b_index_df,deg_rank$D_rank,bet_rank$B_rank)
names(b_index_df)[names(b_index_df) == 'deg_rank$D_rank'] <- 'D_rank'
names(b_index_df)[names(b_index_df) == 'bet_rank$B_rank'] <- 'B_rank'

# Keystone index = degree + closeness + betweenness rankings
b_index_df$Index_Keystone <- rowMeans(subset(b_index_df, select = c(C_rank, D_rank,B_rank)), na.rm = TRUE)
normfunc <- function(x){1-(x-min(x))/(max(x)-min(x))}
b_index_df$Index_norm <- normfunc(b_index_df$Index_Keystone)

# Trophic level
beag_troph <- subset(troph_all_df,Location=="Beagle")
b_index_df <- cbind(b_index_df,beag_troph$TL)
names(b_index_df)[names(b_index_df) == 'beag_troph$TL'] <- 'TL'

# Normalized trophic level
btl<-b_index_df$TL
b_index_df$TL_norm <- (btl-min(btl))/(max(btl)-min(btl))
b_index_df$TL_norm<-replace(b_index_df$TL_norm, b_index_df$TL_norm<0.0001,0) 



## Potter ----
p_clo <- igraph::closeness(g_all[["Potter"]],mode = c("total"))
p_in <- degree(g_all[["Potter"]], mode = c("in"))
p_out <- degree(g_all[["Potter"]], mode = c("out"))
p_deg <- degree(g_all[["Potter"]])
p_bet <- igraph::betweenness(g_all[["Potter"]])

p_index <- cbind(p_clo,p_deg,p_bet,p_in,p_out)
colnames(p_index)<-c("Closeness","Degree","Betweenness","InDegree","OutDegree")

p_index_df <- data.frame(p_index)

p_index_df <- p_index_df %>% 
  mutate(rank=dense_rank(desc(-Closeness)))
names(p_index_df)[names(p_index_df) == 'rank'] <- 'C_rank'


deg_rank <- p_index_df %>% 
  mutate(rank=dense_rank(desc(Degree)))
names(deg_rank)[names(deg_rank) == 'rank'] <- 'D_rank'

bet_rank <- p_index_df %>% 
  mutate(rank=dense_rank(desc(Betweenness)))
names(bet_rank)[names(bet_rank) == 'rank'] <- 'B_rank'

p_index_df <- cbind(p_index_df,deg_rank$D_rank,bet_rank$B_rank)
names(p_index_df)[names(p_index_df) == 'deg_rank$D_rank'] <- 'D_rank'
names(p_index_df)[names(p_index_df) == 'bet_rank$B_rank'] <- 'B_rank'


p_index_df$Index_Keystone <- rowMeans(subset(p_index_df, select = c(C_rank, D_rank,B_rank)), na.rm = TRUE)
normfunc <- function(x){1-(x-min(x))/(max(x)-min(x))}
p_index_df$Index_norm <- normfunc(p_index_df$Index_Keystone)


pott_troph <- subset(troph_all_df,Location=="Potter")
p_index_df <- cbind(p_index_df,pott_troph$TL)
names(p_index_df)[names(p_index_df) == 'pott_troph$TL'] <- 'TL'

ptl<-p_index_df$TL
p_index_df$TL_norm <- (ptl-min(ptl))/(max(ptl)-min(ptl))
p_index_df$TL_norm<-replace(p_index_df$TL_norm, p_index_df$TL_norm<0.0001,0) 


# Keystone species ----

## Beagle ----
b<-g_all[["Beagle"]]

### Munida gregaria ----
b_short_m <- shortest_paths(b, V(b)$name=="Munida gregaria", to=V(b), output=c("epath"))
b_short_m <- lapply(b_short_m[["epath"]], function(x) {
  a <- length(unique(x))
})

b_short_m <- t(data.frame(b_short_m))
b_name_m <- as.data.frame(vertex.attributes(b)$name)

b_short_m <-cbind(b_name_m, b_short_m)
colnames(b_short_m)<-c("Species","Path")
unique(b_short_m$Path)

b_short_m <- b_short_m %>%
  mutate(Color = case_when(Species == "Munida gregaria" ~ "#ffb400",
                           Path == 0 ~ "white",
                           Path == 1 ~ "#c23728",
                           Path == 2 ~ "#de6e56",
                           Path == 3 ~ "#e1a692",
                           Path == 4 ~ "#a7d5ed",
                           Path == 5 ~ "#63bff0"))

### Sprattus fuegensis ----
b_short_s <- shortest_paths(b, V(b)$name=="Sprattus fuegensis", to=V(b), output=c("epath"))
b_short_s <- lapply(b_short_s[["epath"]], function(x) {
  a <- length(unique(x))
})

b_short_s <- t(data.frame(b_short_s))
b_name_s <- as.data.frame(vertex.attributes(b)$name)

b_short_s <-cbind(b_name_s, b_short_s)
colnames(b_short_s)<-c("Species","Path")

b_short_s <- b_short_s %>%
  mutate(Color = case_when(Species == "Sprattus fuegensis" ~ "#ffb400",
                           Path == 0 ~ "white",
                           Path == 1 ~ "#c23728",
                           Path == 2 ~ "#de6e56"))


# Get trophic levels of all species in Beagle
library(NetIndices)
b_adj<-get.adjacency(b, sparse=FALSE)
b_tl <- round(TrophInd(b_adj), 3)

# Create layout by trophic level 
b_lay <- matrix(nrow = length(V(b)), ncol = 2)
b_lay[, 1] <- runif(length(V(b)), min=0, max=100)
b_lay[, 2] <- b_tl$TL


## Potter ----

p<-g_all[["Potter"]]

### Euphausia ----
p_short_e <- shortest_paths(p, V(p)$name=="Euphausia superba", to=V(p), output=c("epath"))
p_short_e <- lapply(p_short_e[["epath"]], function(x) {
  a <- length(unique(x))
})

p_short_e <- t(data.frame(p_short_e))
p_name <- as.data.frame(vertex.attributes(p)$name)

p_short_e<-cbind(p_name, p_short_e)
colnames(p_short_e)<-c("Species","Path")
unique(p_short_e$Path)

p_short_e <- p_short_e %>%
  mutate(Color = case_when(Species == "Euphausia superba" ~ "#ffb400",
                           Path == 0 ~ "white",
                           Path == 1 ~ "#c23728",
                           Path == 2 ~ "#de6e56",
                           Path == 3 ~ "#e1a692",
                           Path == 4 ~ "#a7d5ed",))

### Laevilacunaria antarctica ----
p_short_l <- shortest_paths(p, V(p)$name=="Laevilacunaria antarctica", to=V(p), output=c("epath"))
p_short_l <- lapply(p_short_l[["epath"]], function(x) {
  a <- length(unique(x))
})

p_short_l <- t(data.frame(p_short_l))
p_name <- as.data.frame(vertex.attributes(p)$name)

p_short_l<-cbind(p_name, p_short_l)
colnames(p_short_l)<-c("Species","Path")
unique(p_short_l$Path)

p_short_l <- p_short_l %>%
  mutate(Color = case_when(Species == "Laevilacunaria antarctica" ~ "#ffb400",
                           Path == 0 ~ "white",
                           Path == 1 ~ "#c23728",
                           Path == 2 ~ "#de6e56"))


# Get trophic levels of all species in Potter
p_adj<-get.adjacency(p, sparse=FALSE)
p_tl <- round(TrophInd(p_adj), 3)

# Create layout by trophic level
p_lay <- matrix(nrow = length(V(p)), ncol = 2)
p_lay[, 1] <- runif(length(V(p)), min=0, max=100)
p_lay[, 2] <- p_tl$TL


### .rda file for graphing
save(b_index_df, 
     p_index_df, 
     b_short_m,
     b_short_s,
     p_short_e,
     p_short_l,
     p_lay,
     b_lay,
     file="../Data/.BeaglePotter_graphing.rda")



