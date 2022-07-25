
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
  mutate(Network = case_when(Size == 28 ~ "Angola", Size == 33 ~ "Baltic Sea",
                             Size == 159 ~ "Barents Sea Arctic", Size == 180 ~ "Barents Sea Boreal",
                             Size == 46 ~ "Beach Peru", Size == 29 ~ "Benguela", 
                             Size == 242 ~ "Cayman Is", Size == 48 ~ "Celtic Sea", 
                             Size == 106 ~ "Chile rocky", Size == 240 ~ "Cuba",
                             Size == 48 & Links == 221 ~ "Florida", Size == 42 ~ "Gulf Cadiz",
                             Size == 39 ~ "Gulf Lions", Size == 256 ~ "Gulf Tortugas",
                             Size == 249 ~ "Jamaica", Size == 28 & Links == 173 ~ "Kerguelen Plateau",
                             Size == 27 ~ "La Guajira", Size == 37 ~ "Monterey Bay",
                             Size == 81 ~ "NE US Shelf", Size == 26 ~ "Simon Bay",
                             Size == 143 ~ "Southern Brazil", Size == 108 ~ "SW Pacific Ocean",
                             Size == 145 ~ "Beagle Channel", Size == 110 ~ "Potter Cove",
                             Size == 235 ~ "Sanak intertidal", Size == 513 ~ "Sanak nearshore",
                             Size == 435 ~ "Weddell Sea")) %>% 
  dplyr::select(Network, everything())


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
