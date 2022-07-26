
# Create a new script 'CurveBall_Analysis.R' from here including all food webs
# Curveball

# Load packages
packages <- c("dplyr", "multiweb", "ggplot2", "ggjoy")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Load data
load(file="Data/all_igraph.rda")
load(file="Results/comp_str_stab.rda")


# Calculate food web structural properties
curve <- lapply(g_all_ok, function(x) {
  col <- bind_cols(calc_topological_indices(x), calc_modularity(x), calc_QSS(x, nsim=3))
  #mu <- mutate(Network = g_all[1:29])  I deleted this line and added names manually in code below
})

fw_prop <- bind_rows(curve) %>% 
  mutate(Network = c("Angola", "Baltic Sea", "Barents Sea Arctic", "Barents Sea Boreal",
                     "Beach Peru", "Benguela", "Cayman Is", "Celtic Sea", "Chile rocky", 
                     "Cuba", "Florida", "Gulf Cadiz", "Gulf Lions", "Gulf Tortugas", 
                     "Jamaica", "Kerguelen Plateau", "La Guajira", "Monterey Bay", 
                     "NE US Shelf", "Simon Bay", "Southern Brazil", "SW Pacific Ocean", 
                     "Beagle Channel", "Gulf Alaska", "Potter Cove", "Sanak intertidal", 
                     "Sanak nearshore", "Weddell Sea")) %>% 
  dplyr::select(Network, everything())


g_list_ok <- within(g_all_ok, rm("Gulf Alaska"))  # discard Gulf Alaska (too many basal sp)
CB <- lapply(g_list_ok, function(x) {
  
  print(vcount(x))
  curve <- curveBall(x, nsim=1000, istrength = FALSE)
  IC <- bind_cols(calc_topological_indices(curve, ncores = 4), calc_modularity(curve, ncores = 4))

})

# Curveball results to data frame
all_CB <- bind_rows(CB) %>% 
  mutate(Network = case_when(Links == 127 ~ "Angola", Links == 191 ~ "Baltic Sea",
                             Links == 848 ~ "Barents Sea Arctic", Links == 1546 ~ "Barents Sea Boreal",
                             Links == 88 ~ "Beach Peru", Links == 203 ~ "Benguela", 
                             Links == 3766 ~ "Cayman Is", Links == 169 ~ "Celtic Sea", 
                             Links == 1362 ~ "Chile rocky", Links == 3874 ~ "Cuba",
                             Links == 221 ~ "Florida", Links == 410 ~ "Gulf Cadiz",
                             Links == 189 ~ "Gulf Lions", Links == 643 ~ "Gulf Tortugas",
                             Links == 4105 ~ "Jamaica", Links == 173 ~ "Kerguelen Plateau",
                             Links == 198 ~ "La Guajira", Links == 79 ~ "Monterey Bay",
                             Links == 1490 ~ "NE US Shelf", Links == 70 ~ "Simon Bay",
                             Links == 840 ~ "Southern Brazil", Links == 202 ~ "SW Pacific Ocean",
                             Links == 1115 ~ "Beagle Channel", Links == 649 ~ "Potter Cove",
                             Links == 1804 ~ "Sanak intertidal", Links == 6774 ~ "Sanak nearshore",
                             Links == 1972 ~ "Weddell Sea")) %>% 
  dplyr::select(Network, everything())
unique(all_CB$Network)  # check if all FWs are present

# Plot comparing curveball results

ggplot(all_CB, aes(x = Modularity, y = Network, color = Network, fill = Network)) +
  geom_joy() +
  theme_joy(grid = FALSE) +
  labs(x = "Modularity", y = "Food web") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        legend.position = 'none')


# Save data
save(fw_prop, g_list_ok, CB, all_CB,
     file = "Results/curveball.rda")
