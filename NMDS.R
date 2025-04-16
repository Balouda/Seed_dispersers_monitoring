rm(list = ls())


library(indicspecies)
library(vegan)    
library(ggplot2)
################### NMDS for Ranomafana bird  s################################
data<-read.csv("NMDS_bird_ran.csv")

set.seed(123)
sp<- data[, 4:24]
fat<-data[,1:3]
sp_dist <- vegdist(sp, method = "bray")
nmds_result<-metaMDS(sp_dist)
print(nmds_result)

nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species_scores <- as.data.frame(scores(nmds_result, display = "species"))
nmds_scores$deployment <- fat$deployment
nmds_scores$sensor <- fat$sensor
nmds_scores$habitat <- fat$habitat
nmds_combined <- list(sites = nmds_scores, species = nmds_species_scores)

a <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = sensor, shape = habitat), size = 2) +  
  stat_ellipse(aes(group = sensor, color = sensor, linetype = sensor), 
               level = 0.95, linewidth = 1.2) +  
  scale_color_manual(values = c("Arboreal" = "black", "Terrestrial" = "gray50", "AudioMoth" = "gray80")) +
  scale_linetype_manual(values = c("Arboreal" = "longdash", "Terrestrial" = "dashed", "AudioMoth" = "dotted")) +
  theme_minimal() +
  labs(x = "NMDS_1",
       y = "NMDS_2",
       color = "Sensors",
       shape = "Habitat Types",
       linetype = "Sensors") +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))
print(a)

permanova_result <- adonis2(sp ~ sensor, data = fat, permutations = 999) 
print(permanova_result)

result <- multipatt(sp, fat$sensor, func = "IndVal")
summary(result)



###################  NMDS for AudioMoth Schedules  #############################

data<-read.csv("NMDS_aud_AB.csv")
set.seed(123)
sp<- data[, 4:22]
fat<-data[,1:3]
sp_dist <- vegdist(sp, method = "bray")
nmds_result<-metaMDS(sp_dist)
print(nmds_result)      


nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species_scores <- as.data.frame(scores(nmds_result, display = "species"))
nmds_scores$deployment <- fat$deployment
nmds_scores$Schedule <- fat$Schedule
nmds_scores$Habitat <- fat$Habitat
nmds_combined <- list(sites = nmds_scores, species = nmds_species_scores)

b <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Schedule, shape = Habitat), size = 2) +  
  stat_ellipse(aes(group = Schedule, color = Schedule, linetype = Schedule), 
               level = 0.95, linewidth = 2) + 
  scale_color_manual(values = c("A" = "black", "B" = "gray60")) +
  scale_linetype_manual(values = c("A" = "longdash", "B" = "dashed")) +
  theme_minimal() +
  labs(x = "NMDS_1",
       y = "NMDS_2",
       color = "Schedules",
       shape = "Habitat Types",
       linetype = "Schedules") +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))

print(b)

permanova_result <- adonis2(sp ~ Schedule , data = fat, permutations = 999)
print(permanova_result)

result <- multipatt(sp, fat$Schedule, func = "IndVal")
summary(result)


###################  NMDS for Arboreal and Terrestrial camera trap  ############
data<-read.csv("NMDS_cam_AT.csv")
set.seed(123)
sp<- data[, 4:53]
fat<-data[,1:3]
sp_dist <- vegdist(sp, method = "bray")
nmds_result<-metaMDS(sp_dist)
print(nmds_result)

nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_species_scores <- as.data.frame(scores(nmds_result, display = "species"))
nmds_scores$deployment <- fat$deployment
nmds_scores$Sensors  <- fat$Sensors
nmds_scores$Habitat <- fat$Habitat
nmds_combined <- list(sites = nmds_scores, species = nmds_species_scores)

c <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Sensors , shape = Habitat), size = 1)+ 
  stat_ellipse(aes(group = Sensors, color = Sensors, linetype = Sensors), 
               level = 0.95, linewidth = 2) +  # Ellipses
  scale_color_manual(values = c("Arboreal" = "black", "Terrestrial" = "gray60")) +
  scale_linetype_manual(values = c("Arboreal" = "longdash", "Terrestrial" = "dashed")) +
  theme_minimal() +
  scale_shape_manual(values = c(20, 22,3,24,25)) +
  coord_cartesian(xlim = c(-2.5, 1), ylim = c(-1.5, 1.5)) +
  labs(x = "NMDS_1",
    y = "NMDS_2",
    shape = "Habitat Types",
    linetype = "Sensors"  
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 8, face = "bold")
  )
print(c)

permanova_result <- adonis2(sp ~ Sensors, data = fat, permutations = 999)
print(permanova_result)

result <- multipatt(sp, fat$Sensors , func = "IndVal")
summary(result)






