load(file="../Data/for_graphing.rda")

# Trophic level properties (basal, intermediate, top) ----

library(igraph)
library(tidyr)

##Percentage basal (in degree = 0)
indeg_all <- lapply(g_all, function(x) {
  deg <- degree(x,mode= c("in"))
})

indeg_all_df <- indeg_all %>%
  unlist(recursive=FALSE) %>%
  enframe() %>%
  extract(name, into = c("Location", "Species"), "^([^.]+)\\.(.*)") %>%
  rename(Indegree = value)

indeg <- subset(indeg_all_df,Indegree==0,select = c(Location,Species,Indegree))

indeg <- indeg %>%
  group_by(Location) %>%
  count(Indegree)

names(indeg)[names(indeg) == 'n'] <- 'BasalCount'
indeg$Especies <- complexity$Especies
indeg$Basal <- indeg$BasalCount/indeg$Especies


##Percentage top predators (out degree = 0)
outdeg_all <- lapply(g_all, function(x) {
  deg <- degree(x,mode= c("out"))
})

outdeg_all_df <- outdeg_all %>%
  unlist(recursive=FALSE) %>%
  enframe() %>%
  extract(name, into = c("Location", "Species"), "^([^.]+)\\.(.*)") %>%
  rename(Outdegree = value)

outdeg <- subset(outdeg_all_df,Outdegree==0,select = c(Location,Species,Outdegree))

outdeg <- outdeg %>%
  group_by(Location) %>%
  count(Outdegree) %>%
  as.data.frame()

#Adding Benguela manually (no top predators, so was initially excluded from the subset)
names(outdeg)[names(outdeg) == 'n'] <- 'TopCount'
Beng <- data.frame("Benguela","0","0")
names(Beng) <- c("Location","Outdegree","TopCount")
outdeg <- rbind(outdeg,Beng)
outdeg <- outdeg[order(outdeg$Location),]
outdeg$TopCount <- as.numeric(outdeg$TopCount)

outdeg$Especies <- complexity$Especies
outdeg$Top <- outdeg$TopCount/outdeg$Especies

## Combine basal and top to calculate % intermediate species

alldeg <- cbind(indeg, outdeg$TopCount, outdeg$Top)
names(alldeg)[names(alldeg) == '...6'] <- 'TopCount'
names(alldeg)[names(alldeg) == '...7'] <- 'Top'
alldeg$Intermediate <- 1 - (alldeg$Basal + alldeg$Top)
alldeg$IntermediateCount <- alldeg$Especies - (alldeg$BasalCount + alldeg$TopCount)
alldeg$ConsumerCount <- alldeg$IntermediateCount + alldeg$TopCount


# Primary producer/consumer for each food web ----

library(stringr)

pp <- subset(indeg_all_df, Indegree == 0, select = c(Location, Species, Indegree))

## Exclude trophic species that are basal but not primary producers 
# Start classifying spp
install.packages("taxizedb")
library(taxizedb)
install.packages("taxize")
library(taxize)

specieslist <- pp$Species
t <- tax_name(query = c(specieslist), get = c("kingdom","phylum","class"), db = "ncbi")

prim_prod <- t %>% 
  rename(Species = query) %>% 
  dplyr::select(Species, kingdom, phylum) %>% 
  left_join(pp) %>% 
  dplyr::select(Location, Species, kingdom, phylum)
prim_prod[is.na(prim_prod)] <- "ToCheck"

prim_prod_checked <- prim_prod %>% 
  dplyr::filter(!str_detect(kingdom, 'Metazoa|Fungi')) %>% 
  dplyr::filter(!str_detect(phylum, 'Ciliophora|Foraminifera')) %>% 
  dplyr::filter(str_detect(phylum, 'Rhodophyta|Streptophyta|Bacillariophyta|Chlorophyta|
                           Cyanobacteria|Haptophyta'))

prim_prod_mancheck <- prim_prod %>% 
  dplyr::filter(!str_detect(kingdom, 'Metazoa|Fungi')) %>% 
  dplyr::filter(!str_detect(phylum, 'Ciliophora|Foraminifera')) %>% 
  dplyr::filter(str_detect(Species, 'Acanthochiasma|Acantholithium|Acanthometra|Acrosiphoniaceae|
                           Adenocystis utricularis|adenocystis.utricularis|Agarum|
                           ahnfeltiopsis.spp.|Alaria|Algas|Ascophyllum.nodosum|
                           Ascoseira mirabilis|Autothroph_flagellat|Benthic Diatomea|
                           benthic_diatoms|Benthic.algae|Benthic.macrophytes|Bidulphia|
                           ceramium.spp.|Ceratium|Chlorophyta Crust|Cianobacterias|
                           codium.dimorpha|Colpomenia|colpomenia.phaeodactyla|colpomenia.sinuosa|
                           Costaria costata|Crustose coralline algae complex|Cystoseira|
                           Desmarestia|Desmarestia anceps|Desmarestia antarctica|
                           Desmarestia menziesii|Diatom|Diatomeas|diatoms...blue.green.algae|
                           diatoms.in.sand|diatoms.in.surface.film|Dinoflagelados|Dinophysis|
                           Distephanus sp.|durvillaea.antarctica|Epiphytic Diatomea|
                           Fitoplancton|Fucus|Fucus.vesiculosus|Geminocarpus geminatus|
                           Gigartacon|glossophora.kunthii|Ice_algae|Laminaria|arge.algae..Macrocystes.|
                           laurencia.chilensis|lessonia.nigrescens|Macro.epiphytes|Macroalgae|
                           Macroalgas|Micro.epiphytes|MICROALGAE_MICRYPHYTOBENTHOS|
                           Microphytobenthos|Microphytoplankton|Nereocystis luetkeana|
                           other.phytoplankton|Pastos.marinos|Peridinium|petalonia.fascia|
                           petroglossum.spp.|peysonella.spp.|Phaeophyta|Phaeophyta Crust|
                           Phaeurus antarcticus |Phytoplankton|PHYTOPLANKTON|Phytoplankton_indet|
                           Pico.nanophytoplankton|Pilayella littoralis|plocamium.cartilageneum|
                           polysiphonia_spp|porphyra_spp|Prasinophyceae|Prasinophyta|
                           prionitis.spp|ralfsia.californica|rama.novazelandensis|rhizoclonium.cilindricum|
                           Saccharina sessile|schottera.nicaensis|schyzimenia.doryophora|
                           Silicoflagellida|Sphaerozoum|Thalassoxantium|Triposolenia|
                           Ulothrix sp.|Ulva.spp.|ulvella.spp.|Unidentified algae'))

prim_prod_ok <- bind_rows(prim_prod_checked, prim_prod_mancheck) %>% 
  mutate(kingdom = case_when(kingdom == 'ToCheck' ~ 'Manually checked',
                             TRUE ~ as.character(kingdom)),
         phylum = case_when(phylum == 'ToCheck' ~ 'Manually checked',
                             TRUE ~ as.character(phylum))) 
pp_c <- prim_prod_ok %>% 
  group_by(Location) %>% 
  mutate(pp_count = n()) %>% 
  dplyr::distinct(Location, pp_count) %>% 
  left_join(alldeg) %>% 
  mutate(pp_c_ratio = pp_count / ConsumerCount) %>% 
  dplyr::select(Location, pp_count, ConsumerCount, pp_c_ratio)
  

save(t, prim_prod,file = "../Data/pp_c.rda")

#
# Some FWs don't have Primary Producers
# Check this!
#

load(file = "../Data/all_igraph.rda")
pc <- g_all[["Pacific"]]
basal_spp_pc <- as.data.frame(degree(pc, mode = "in"))
# Beagle.csv: 1st column = Predator, 2nd column = Prey
# Brazil interactions are the other way round (adj matrix)
# Celtic Sea doesn't have pp, it's fish-based
# Pacific Ocean doesn't have pp, it's fish-based



