
# Create a new script 'CurveBall_Analysis.R' from here including all food webs
# Curveball


curve <- lapply(g_all, function(x) {
  col <- bind_cols(calc_topological_indices(x), calc_modularity(x), calc_QSS(x, nsim=10))
  mu <- mutate(Network = g_all[1:29])
})



CB <- lapply(g_all, function(x) {
  curve <- curveBall(x, nsim=10, istrength = FALSE)
  IC <- bind_cols(calc_topological_indices(curve),calc_modularity(curve)) %>% 
    mutate(Network = g_all[1:29]) 
})


# Bind all food web results
all_CB <- bind_rows(ptiIC, atiIC)


# Plot comparing curveball results

library(ggplot2)

ggplot(all_CB, aes(x = Modularity, color = Network, fill = Network))+
  geom_density() +
  scale_manual_viridis_d() +
  theme_bw()



