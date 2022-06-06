

#### Histogram of complexity metrics ----

#Install packages
#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("ggjoy")

#Load libraries
load(file="../Data/.for_graphing.rda")
load(file="../Data/.BeaglePotter_graphing.rda")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(reshape2)
library(ggjoy)

# Geographic location of food webs ----
library(dplyr)
library(ggplot2)
library(maps)

data <- read.csv(file = "../Data/Metadata_FW.csv")
data <- data %>% 
  dplyr::select(FoodWeb, Latitude, Longitude,Number) %>% 
  mutate_at(vars(Longitude, Latitude, Number), as.numeric)
str(data)

world_map <- map_data("world")
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

(map_data <- 
    base_world +
    geom_point(data=data, 
               aes(x=Longitude, y=Latitude), colour="Deep Pink", 
               fill="Pink",pch=21, size=8, alpha=I(0.7))+
    geom_text(data=data, 
              aes(label = Number, x=Longitude,y=Latitude),
              size=6))



# Complexity plots: C with S, L, and LD ----

complexity$Region <- factor(complexity$Region, levels = c("Polar", "Subpolar", 
                                                          "Templado", "Subtropical", 
                                                          "Tropical"))
regioncolor<-scale_color_manual(labels = c("Polar", "Subpolar", "Templado", "Subtropical",  "Tropical"), 
                                values = c("#1984c5", "#a7d5ed","gray50","#e1a692","#c23728"))

# C vs S plot
(s_all<-ggplot(complexity, aes(x=Conectividad,y=Especies)) +  
  geom_point(size=4) + 
  ylab("Especies Tróficas") +
  xlab(NULL)+
  theme_classic()+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)))

(s_reg<-ggplot(complexity, aes(x=Conectividad,y=Especies)) +  
  geom_point(aes(shape=factor(Region),color=factor(Region)),size=4) + 
  ylab("Especies Tróficas") +
  xlab(NULL)+
  theme_classic()+
  theme(legend.position = "none") +
  regioncolor+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)))

# C vs L plot
l_all<-ggplot(complexity, aes(x=Conectividad,y=Interacciones)) +  
  geom_point(size=4) + 
  ylab("Interacciones") +
  xlab(NULL)+
  theme_classic()+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))

l_reg<-ggplot(complexity, aes(x=Conectividad,y=Interacciones)) +  
  geom_point(aes(shape=factor(Region),color=factor(Region)),size=4) + 
  ylab("Interacciones") +
  xlab(NULL)+
  theme_classic() +
  theme(legend.position = "none")+
  regioncolor+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))

# C vs LD plot
ld_all<-ggplot(complexity, aes(x=Conectividad,y=Densidad)) +  
  geom_point(size=4) + 
  ylab("Densidad de interacciones") +
  xlab(NULL)+
  theme_classic()+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 16))

ld_reg<-ggplot(complexity, aes(x=Conectividad,y=Densidad)) +  
  geom_point(aes(shape=factor(Region),color=factor(Region)),size=4) +
  ylab("Densidad de interacciones") +
  xlab("Región")+
  theme_classic()+
  theme(legend.position = "none")+
  regioncolor+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 16))


##Combine C plots to panels
library(ggpubr)
(figa<-ggarrange(s_all,l_all,ld_all,s_reg,l_reg,ld_reg,
                labels = c("A","B","C","D","E","F"),
                ncol = 3, nrow = 2))



# Structure plots  ----


## Graph quantiles of trophic level by region ----
#Not included in the report

TL_quantiles <- troph_all_df %>%
  group_by(Location)%>%
  dplyr::summarize(Min = min(TL), 
                   Q1 = quantile(TL,probs=0.25),
                   Med = median(TL),
                   Q3 = quantile(TL,probs=0.75),
                   Max = max(TL))

figc<-ggplot(TL_quantiles, aes(reorder(x=Location, complexity$Conectividad), ymin=Min, lower=Q1, middle=Med, upper=Q3, ymax= Max))+
  geom_boxplot(stat="identity", aes(color=complexity$Region))+
  theme_classic()+
  ylab("Nivel Trófico")+
  xlab("Red Trófica")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  regioncolor


# Plots of connectance and structure ----

# Omnivory and connectance

omni_gen<-structure %>%
  filter(Location != "Alaska") %>%
  ggplot(aes(x=Conectividad,y=Omnivory))+
      geom_point(size=4)+
      ylab("Omnivoría")+
      theme_classic()+
      xlab(NULL)+
      theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))

omni_color<-structure %>%
  filter(Location != "Alaska") %>%
  ggplot(aes(x=Conectividad,y=Omnivory))+
  geom_point(aes(shape=factor(Region), color=Region),size=4)+
  ylab("Omnivoría")+
  xlab("Conectividad (C)")+
  guides(color=guide_legend("Region"), shape=guide_legend("Region"))+
  theme_classic()+
  theme(legend.position = "none")+
  regioncolor+
  xlab(NULL)+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))


# Path length and connectance
path_gen<-ggplot(structure, aes(x=complexity$Conectividad,y=PathLength))+
  geom_point(size=4)+
  ylab("Distancia entre especies")+
  theme_classic()+
  xlab(NULL)+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 17))

path_color<-ggplot(structure, aes(x=complexity$Conectividad,y=PathLength))+
  geom_point(aes(shape=factor(complexity$Region),color=complexity$Region),size=4)+
  ylab("Distancia entre especies")+
  guides(color=guide_legend("Region"), shape=guide_legend("Region"))+
  theme_classic()+
  theme(legend.position = "none")+
  regioncolor+
  xlab(NULL)+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 17))

#Modularity and connectance
modu_gen<-ggplot(structure, aes(x=complexity$Conectividad,y=Modularity))+
  geom_point(size=4)+
  ylab("Modularidad")+
  theme_classic()+
  xlab(NULL)+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))

modu_color<-ggplot(structure, aes(x=complexity$Conectividad,y=Modularity))+
  geom_point(aes(shape=factor(complexity$Region),color=complexity$Region),size=4)+
  ylab("Modularidad")+
  theme_classic()+
  theme(legend.position = "none")+
  regioncolor+
  xlab(NULL)+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))

##Arrange omnivory, PL, and modularity plots ito 1
(figd<-ggarrange(omni_gen,path_gen,modu_gen,
                 omni_color,path_color,modu_color,
                labels = c("A","B","C","D","E","F"),ncol = 3, nrow = 2))



# Plot degree distribution by location ----
library(ggjoy)
deg_all_df <- group_by(deg_all_df, Location) %>%
  mutate(m=max(Conectividad)) %>%
  arrange(m) %>%
  ungroup() %>%
  mutate(Location=factor(Location, unique(Location)))

(fige <- ggplot(deg_all_df,aes(x=Degree,y=Location,fill=Region))+
  geom_joy()+
  theme_joy(grid=FALSE)+
  scale_fill_manual(labels = c("Polar", "Subpolar", "Subtropical","Templado",  "Tropical"), values = c("#1984c5", "#a7d5ed","#e1a692","gray50","#c23728"))+
  labs(x="Número de interacciones", y="Red Trófica")+
  xlim(c(-10,150))+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)))

# degree distribution boxplots
# Not included in the report
q_all<-lapply(g_all,function(x) {
  deg<-degree(x)
  sum<-summary(deg)
})

q_tibble<- t(q_all %>%
               as_tibble())

df_quantiles <- as.data.frame(q_tibble)
df_quantiles <- df_quantiles %>%
  rename(Min = V1, Q1 = V2, Median = V3, Mean = V4,
         Q3 = V5, Max = V6)


df_quantiles<-cbind(complexity$Region,df_quantiles)
colnames(df_quantiles)[1]<-"Region"

df_quantiles$Region <- factor(df_quantiles$Region, levels = c("Polar", "Subpolar", "Templado", "Subtropical", "Tropical"))
row.names(df_quantiles) -> df_quantiles$Location

(figb<-ggplot(df_quantiles, aes(reorder(x=Location,complexity$Conectividad), ymin=Min, lower=Q1, middle=Median, upper=Q3, ymax= Max))+
    geom_boxplot(stat="identity", aes(color=Region))+
    theme_classic()+
    ylab("Grado")+
    xlab("Red Trófica")+
    regioncolor+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))



# Trophic level plots ----
library(multiweb)
par(mfrow=c(1,2))
beagle_tlplot <- plot_troph_level(g_all[["Beagle"]], 
                                  vertexSizeFactor = degree(g_all[["Beagle"]])*0.4)
potter_tlplot <- plot_troph_level(g_all[["Potter"]], 
                                  vertexSizeFactor = degree(g_all[["Potter"]])*0.4)


# Trophic level and keystone index ----

b_index_df$Location <- c("Beagle")   
p_index_df$Location <- c("Potter")    

both_index<-rbind(b_index_df,p_index_df)
both_index$TL_norm<- as.numeric(both_index$TL_norm)
both_index$TL_norm<-replace(both_index$TL_norm, both_index$TL_norm<0.0001,0) 

both_index<- both_index %>% 
  dplyr::select(Location, Index_norm, TL_norm)

both_index_boxplot <- both_index %>%
  pivot_longer(2:3) %>%
  rename(Index_type=name,Index_value=value) %>%
  mutate_at("Index_value", as.numeric)

# Scatter plot for Index norm vs TL norm
(figg<-ggplot(both_index, aes(x=TL_norm,y=Index_norm))+
  geom_point(aes(color=factor(Location)), size=5)+
  theme_classic()+
  labs(x="Nivel trófico normalizado", y="Índice de especie clave normalizado")+
  scale_color_discrete(name = "Red Trófica")+
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25))
      )

# Boxplot for Index norm and TL norm
(figh<-ggplot(both_index_boxplot,aes(x=Location, y=Index_value))+
  geom_boxplot(aes(fill=Index_type), size=2)+
  theme_classic()+
  labs(x="Red Trófica",y="Índice y Nivel Trófico Normalizados")+
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25)))+
        scale_fill_discrete(name = "Índice", labels = c("Especie Clave", "Nivel Trófico"))



# Outbound interactions plot for keystone species ----

## Beagle ----

### Munida gregaria ----
b<- g_all[["Beagle"]]

(b_mun <- plot(b, 
                   vertex.color=b_short_m$Color, 
                   vertex.label=NA,
                   vertex.label.cex = 2,
                   vertex.size = 10,
                   edge.arrow.size = 0.1,
                   edge.curved = 0.5,
                   layout=b_lay))

legend(x="bottomright",legend=c("0","1","2","3","4","5"),
       col=c("white",
             "#c23728",
             "#de6e56",
             "#e1a692",
             "#a7d5ed",
             "#63bff0"),
       fill=c("white",
             "#c23728",
             "#de6e56",
             "#e1a692",
             "#a7d5ed",
             "#63bff0"),
       cex=1, lwd=1, bty = "n",
       title = "# de interacciones")

### Sprattus fuegensis ----
(b_spr <- plot(b, 
                  vertex.color=b_short_s$Color, 
                  vertex.label=NA,
                  vertex.label.cex = 2,
                  vertex.size = 10,
                  edge.arrow.size = 0.1,
                  edge.curved = 0.5,
                  layout=b_lay))


## Potter ----
p <- g_all[["Potter"]]

### Euphausia superba ----
(p_eup <- plot(p, 
                   vertex.color=p_short_e$Color, 
                   vertex.label=NA,
                   vertex.label.cex = 2,
                   vertex.size = 8,
                   edge.arrow.size = 0.1,
                   edge.curved = 0.3,
                   layout=p_lay))


### Laevilacunaria antarctica ----
(p_lae <- plot(p, 
               vertex.color=p_short_l$Color, 
               vertex.label= NA,
               vertex.label.cex = 2,
               vertex.size = 8,
               edge.arrow.size = 0.05,
               edge.curved = 0.3,
               layout=p_lay))

