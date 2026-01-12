rm(list = ls())


library(indicspecies)
library(vegan)    
library(ggplot2)
################### NMDS for Ranomafana bird  s################################
data<-read.csv("NMDS_bird_ran.csv")

set.seed(123)
sp<- data[, 4:24]
fat<-data[,1:3]
sp_dist <- vegdist(sp, method = "jaccard")
nmds_result<-metaMDS(sp_dist)
print(nmds_result)

nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species_scores <- as.data.frame(scores(nmds_result, display = "species"))
nmds_scores$deployment <- fat$deployment
nmds_scores$sensor <- fat$sensor
nmds_scores$habitat <- fat$habitat
nmds_combined <- list(sites = nmds_scores, species = nmds_species_scores)

a <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = sensor, shape = habitat), size = 3) +  
  stat_ellipse(aes(group = sensor, color = sensor, linetype = sensor), 
               level = 0.95, linewidth = 1.2) +  
  scale_color_manual(values = c("Arboreal" = "black", "Terrestrial" = "gray50", "AudioMoth" = "gray80"),
                     labels= c("Arboreal"="Arboreal camera", "Terrestrial"= "Terrestrial camera")) +
  scale_linetype_manual(
    name = "Sensors",
    values = c("Arboreal" = "longdash", "Terrestrial" = "dashed", "AudioMoth" = "dotted"),
    labels= c("Arboreal"="Arboreal camera", "Terrestrial"= "Terrestrial camera"))+
  scale_shape_manual(
    values = c(
      "Forest" = 16, 
      "Natural regeneration" = 15, 
      "Restoration" = 17  
    ),
    labels = c("Restoration" = "Reforestation","Forest" ="Mature protected forest") 
  ) +
  theme_minimal() +
  labs(x = "NMDS_1",
       y = "NMDS_2",
       color = "Sensors",
       shape = "Habitat Types",
       linetype = "Sensors") +
  theme(legend.position = "right",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 14, face = "bold"))
print(a)

permanova_result <- adonis2(sp ~ sensor, data = fat, permutations = 999) 
print(permanova_result)

result <- multipatt(sp, fat$sensor, func = "IndVal.g", control = how(nperm = 999))
summary(result)



###################  NMDS for AudioMoth Schedules  #############################

data<-read.csv("NMDS_aud_AB.csv")
set.seed(123)
sp<- data[, 4:22]
fat<-data[,1:3]
sp_dist <- vegdist(sp, method = "jaccard")
nmds_result<-metaMDS(sp_dist)
print(nmds_result)      


nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species_scores <- as.data.frame(scores(nmds_result, display = "species"))
nmds_scores$deployment <- fat$deployment
nmds_scores$Schedule <- fat$Schedule
nmds_scores$Habitat <- fat$Habitat
nmds_combined <- list(sites = nmds_scores, species = nmds_species_scores)

b <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Schedule, shape = Habitat, fill = Habitat), 
             size = 2, stroke = 0.9) +  
  stat_ellipse(aes(group = Schedule, color = Schedule, linetype = Schedule), 
               level = 0.95, linewidth = 2) + 
  scale_color_manual(
    values = c("A" = "black", "B" = "black"),
    labels = c("A" = "Schedule A", "B" = "Schedule B")
  ) +
  scale_linetype_manual(
    values = c("A" = "longdash", "B" = "dotted"),
    labels = c("A" = "Schedule A", "B" = "Schedule B")
  ) +
  scale_fill_manual(
    values = c("Forest" = "black", "Restoration" = "black"),
    labels = c("Restoration" = "Reforestation", "Forest" = "Mature protected forest")
  ) +
  scale_shape_manual(
    values = c("Forest" = 16, "Restoration" = 24),
    labels = c("Restoration" = "Reforestation", "Forest" = "Mature protected forest")
  ) +
  theme_minimal() +
  labs(
    x = "NMDS_1",
    y = "NMDS_2",
    shape = "Habitat Types", 
    fill = "Habitat Types", color = "AudioMoth",     
    linetype = "AudioMoth",     
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 14, face = "bold")
  )
print(b)

permanova_result <- adonis2(sp ~ Schedule , data = fat, permutations = 999)
print(permanova_result)

result <- multipatt(sp, fat$Schedule, func = "IndVal.g", control = how(nperm = 999))
summary(result)


###################  NMDS for Arboreal and Terrestrial camera trap  ############
data<-read.csv("NMDS_cam_AT.csv")
set.seed(123)
sp<- data[, 4:53]
fat<-data[,1:3]
sp_dist <- vegdist(sp, method = "jaccard")
nmds_result<-metaMDS(sp_dist)
print(nmds_result)

nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species_scores <- as.data.frame(scores(nmds_result, display = "species"))
nmds_scores$deployment <- fat$deployment
nmds_scores$Sensors  <- fat$Sensors
nmds_scores$Habitat <- fat$Habitat
nmds_combined <- list(sites = nmds_scores, species = nmds_species_scores)

c <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Habitat, shape = Habitat), size = 3,stroke=1.2) +   
  stat_ellipse(aes(group = Sensor, linetype = Sensor),             
               level = 0.95, linewidth = 1.5, color = "grey50") +   
  scale_color_manual(
    name = "Habitat",
    values = c("Forest"="darkolivegreen", "Forest burned"="brown", "Natural regeneration"="blue", "Plantation"="yellow", "Restoration"="black"),
    labels = c("Restoration" = "Reforestation","Forest"="Mature protected forest") 
  ) +
  scale_fill_manual(
    name = "Habitat",
    values = c("Forest"="darkolivegreen", "Forest burned"="brown", "Natural regeneration"="blue", "Plantation"="yellow", "Restoration"="cyan"),
    labels = c("Restoration" = "Reforestation","Forest"="Mature protected forest")
  ) +
  scale_shape_manual(
    name = "Habitat",
    values = c("Forest"=16, "Forest burned"=15, "Natural regeneration"=15, "Plantation"=15, "Restoration"=24),
    labels = c("Restoration" = "Reforestation","Forest"="Mature protected forest")
  ) +
  scale_linetype_manual(
    name = "Sensors",
    values = c("Arboreal" = "longdash", "Terrestrial" = "dotted"),
    labels= c("Arboreal"=" Arboreal camera", "Terrestrial"= "Terrestrial camera"))+
  theme_minimal() +
  coord_cartesian(xlim = c(-2.5, 1), ylim = c(-1.5, 1.5)) +
  labs(x = "NMDS_1",
       y = "NMDS_2",
       color = "Habitat Types",
       shape = "Habitat Types",
       linetype = "Sensors") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 14, face = "bold")
  )
print(c)

permanova_result <- adonis2(sp ~ Sensors, data = fat, permutations = 999)
print(permanova_result)

result <- multipatt(sp, fat$Senor, func = "IndVal.g", control = how(nperm = 999))
summary(result)







