rm(list = ls())

library(vegan)
library(ggplot2)
library(dplyr)

cl<-read.csv("cl.csv") #Cleared land camera trap data
cl$X<-NULL

fb<-read.csv("fb.csv") #Burned Forest camera trap data
fb$X<-NULL
fb_a<-read.csv("fb_a.csv") #Burned Forest arboreal camera trap data
fb_a$X<-NULL
fb_t<-read.csv("fb_t.csv")  #Burned Forest terrestrial camera trap data
fb_t$X<-NULL

forest<-read.csv("forest.csv") #Mature protected forests camera trap data
forest$X<-NULL
forest_a<-read.csv("forest_a.csv") #Mature protected forests arboreal camera trap data
forest_a$X<-NULL
forest_t<-read.csv("forest_t.csv") #Mature protected forests terrestrial camera trap data
forest_t$X<-NULL

nr<-read.csv("nr.csv") #Natural regeneration camera trap data
nr$X<-NULL
nr_a<-read.csv("nr_a.csv") #Natural regeneration  arboreal camera trap data
nr_a$X<-NULL
nr_t<-read.csv("nr_t.csv") #Natural regeneration terrestrial camera trap data
nr_t$X<-NULL

plantation<-read.csv("plantation.csv") #Plantation camera trap data
plantation$X<-NULL
plantation_a<-read.csv("plantation_a.csv") #Plantation arboreal camera trap data
plantation_a$X<-NULL
plantation_t<-read.csv("plantation_t.csv") #Plantation terrestrial camera trap data
plantation_t$X<-NULL

restoration<-read.csv("restoration.csv") #Restoration camera trap data
restoration$X<-NULL
restoration_a<-read.csv("restoration_a.csv") #Restoration arboreal camera trap
restoration_a$X<-NULL
restoration_t<-read.csv("restoration_t.csv") #Restoration terrestrial camera trap data
restoration_t$X<-NULL

forest_AB<-read.csv("SAC_audio_forest_AB.csv") #Mature protected forests AudioMoth schedule AB data
forest_AB$X<-NULL
forest_A<-read.csv("SAC_audio_forest_A.csv")  #Mature protected forests AudioMoth schedule A data
forest_A$X<-NULL
forest_B<-read.csv("SAC_audio_forest_B.csv")  #Mature protected forests AudioMoth schedule B data
forest_B$X<-NULL
restoration_AB<-read.csv("SAC_audio_restoration_AB.csv")  #Restoration AudioMoth schedule AB data
restoration_AB$X<-NULL
restoration_A<-read.csv("SAC_audio_restoration_A.csv") #Restoration AudioMoth schedule A data
restoration_A$X<-NULL
restoration_B<-read.csv("SAC_audio_restoration_B.csv") #Restoration AudioMoth schedule B data
restoration_B$X<-NULL
regeneration_B<-read.csv("SAC_audio_natural_regeneration_B.csv") #Natural regeneration AudioMoth schedule B data
regeneration_B$X<-NULL


for_ab<- specaccum(forest_AB, method = "exact",permutations = 10000)
for_ab
for_a<- specaccum(forest_A, method = "exact",permutations = 10000)
for_a
for_b<- specaccum(forest_B, method = "exact",permutations = 10000)
for_b

rest_ab<- specaccum(restoration_AB, method = "exact",permutations = 10000)
rest_ab
rest_a<- specaccum(restoration_A, method = "exact",permutations = 10000)
rest_a
rest_b<- specaccum(restoration_B, method = "exact",permutations = 10000)
rest_b

natreg_b<- specaccum(regeneration_B, method = "exact",permutations = 10000)
natreg_b

restoration<- specaccum(restoration, method = "exact",permutations = 10000)
restoration
restoration_a<- specaccum(restoration_a, method = "exact",permutations = 10000)
restoration_a
restoration_t<- specaccum(restoration_t, method = "exact",permutations = 10000)
restoration_t

plantation<- specaccum(plantation, method = "exact",permutations = 10000)
plantation
plantation_a<- specaccum(plantation_a, method = "exact",permutations = 10000)
plantation_a
plantation_t<- specaccum(plantation_t, method = "exact",permutations = 10000)
plantation_t

nr<- specaccum(nr, method = "exact",permutations = 10000)
nr
nr_a<- specaccum(nr_a, method = "exact",permutations = 10000)
nr_a
nr_t<- specaccum(nr_t, method = "exact",permutations = 10000)
nr_t

forest<- specaccum(forest, method = "exact",permutations = 10000)
forest
forest_a<- specaccum(forest_a, method = "exact",permutations = 10000)
forest_a
forest_t<- specaccum(forest_t, method = "exact",permutations = 10000)
forest_t

fb<- specaccum(fb, method = "exact",permutations = 10000)
fb
fb_a<- specaccum(fb_a, method = "exact",permutations = 10000)
fb_a
fb_t<- specaccum(fb_t, method = "exact",permutations = 10000)
fb_t

cl<- specaccum(cl, method = "exact",permutations = 10000)
cl

specaccum_to_df <- function(x, habitat, method) {
  data.frame(
    Sites = x$sites,
    Richness = x$richness,
    Lower = x$richness - x$sd,
    Upper = x$richness + x$sd,
    Method = method,
    Habitat = habitat,
    Type = ifelse(grepl("audio", method, ignore.case = TRUE), "Audio", "Camera")
  )
}
df <- bind_rows(
  specaccum_to_df(forest, "Mature protected forest", "Combined camera"),
  specaccum_to_df(forest_a, "Mature protected forest", "Arboreal camera"),
  specaccum_to_df(forest_t, "Mature protected forest", "Terrestrial camera"),
  specaccum_to_df(restoration, "Reforestation", "Combined camera"),
  specaccum_to_df(restoration_a, "Reforestation", "Arboreal camera"),
  specaccum_to_df(restoration_t, "Reforestation", "Terrestrial camera"),
  specaccum_to_df(nr, "Natural regeneration", "Combined camera"),
  specaccum_to_df(nr_a, "Natural regeneration", "Arboreal camera"),
  specaccum_to_df(nr_t, "Natural regeneration", "Terrestrial camera"),
  specaccum_to_df(plantation, "Plantation", "Combined camera"),
  specaccum_to_df(plantation_a, "Plantation", "Arboreal camera"),
  specaccum_to_df(plantation_t, "Plantation", "Terrestrial camera"),
  specaccum_to_df(fb, "Burned forest", "Combined camera"),
  specaccum_to_df(fb_a, "Burned forest", "Arboreal camera"),
  specaccum_to_df(fb_t, "Burned forest", "Terrestrial camera"),
  specaccum_to_df(cl, "Cleared land", "Terrestrial camera"),
  specaccum_to_df(for_ab, "Mature protected forest (audio)", "AudioMoth schedules A and B"),
  specaccum_to_df(for_a,  "Mature protected forest (audio)", "AudioMoth Schedule A"),
  specaccum_to_df(for_b,  "Mature protected forest (audio)", "AudioMoth Schedule B"),
  specaccum_to_df(rest_ab, "Reforestation (audio)", "AudioMoth schedules A and B"),
  specaccum_to_df(rest_a,  "Reforestation (audio)", "AudioMoth Schedule A"),
  specaccum_to_df(rest_b,  "Reforestation (audio)", "AudioMoth Schedule B"),
  specaccum_to_df(natreg_b, "Natural regeneration (audio)", "AudioMoth Schedule B")
)

cols <- c(
  "Combined camera" = "green", "Arboreal camera" = "blue", "Terrestrial camera" = "red",
  "AudioMoth schedules A and B" = "cyan", "AudioMoth Schedule A" = "yellow", "AudioMoth Schedule B" = "violet"
)
lines <- c(
  "Combined camera" = 1, "Arboreal camera" = 3, "Terrestrial camera" = 2,
  "AudioMoth schedules A and B" = 1, "AudioMoth Schedule A" = 3, "AudioMoth Schedule B" = 2
)
df$Habitat <- factor(df$Habitat, levels = c(
  "Mature protected forest",
  "Reforestation",
  "Natural regeneration",
  "Plantation",
  "Burned forest", 
  "Cleared land",
  "Mature protected forest (audio)",
  "Reforestation (audio)",
  "Natural regeneration (audio)"
))


a <- ggplot(df, aes(x = Sites, y = Richness,
                    color = Method,
                    linetype = Method,
                    fill = Method)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~Habitat, scales = "free", ncol = 3) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  labs(x = "Camera Trap Night/Hours (audio)", y = "Species richness") +
  guides(
    color = guide_legend(nrow = 1, title = NULL),
    fill  = guide_legend(nrow = 1, title = NULL),
    linetype = guide_legend(nrow = 1, title = NULL)
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",       
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray75"),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
print(a)


########################## SACs for Sites ######################################
specaccum_to_df <- function(x, site, method) {
  data.frame(
    Sites = x$sites,
    Richness = x$richness,
    Lower = x$richness - x$sd,
    Upper = x$richness + x$sd,
    Method = method,
    Site = site,
    Type = ifelse(grepl("audio", method, ignore.case = TRUE) | method %in% c("A","B","AB"),
                  "Audio", "Camera")
  )
}

analalava <- read.csv("analalava.csv"); analalava$X <- NULL
analalava_a <- read.csv("analalava_a.csv"); analalava_a$X <- NULL
analalava_t <- read.csv("analalava_t.csv"); analalava_t$X <- NULL
ranomafana <- read.csv("ranomafana.csv"); ranomafana$X <- NULL
ranomafana_a <- read.csv("ranomafana_a.csv"); ranomafana_a$X <- NULL
ranomafana_t <- read.csv("ranomafana_t.csv"); ranomafana_t$X <- NULL
ankafobe <- read.csv("ankafobe.csv"); ankafobe$X <- NULL
ankafobe_a <- read.csv("ankafobe_a.csv"); ankafobe_a$X <- NULL
ankafobe_t <- read.csv("ankafobe_t.csv"); ankafobe_t$X <- NULL
marojejy <- read.csv("marojejy.csv"); marojejy$X <- NULL
marojejy_a <- read.csv("marojejy_a.csv"); marojejy_a$X <- NULL
marojejy_t <- read.csv("marojejy_t.csv"); marojejy_t$X <- NULL
vaingandrano <- read.csv("vaingandrano.csv"); vaingandrano$X <- NULL
vaingandrano_a <- read.csv("vaingandrano_a.csv"); vaingandrano_a$X <- NULL
vaingandrano_t <- read.csv("vaingandrano_t.csv"); vaingandrano_t$X <- NULL
schedule_A <- read.csv("SAC_audio_schedule_A.csv"); schedule_A$X <- NULL
schedule_B <- read.csv("SAC_audio_schedule_B.csv"); schedule_B$X <- NULL
schedule_AB <- read.csv("SAC_audio_all_AB.csv"); schedule_AB$X <- NULL


ANA <- specaccum(analalava, method="exact", permutations=10000)
ANA_A <- specaccum(analalava_a, method="exact", permutations=10000)
ANA_T <- specaccum(analalava_t, method="exact", permutations=10000)
RAN <- specaccum(ranomafana, method="exact", permutations=10000)
RAN_A <- specaccum(ranomafana_a, method="exact", permutations=10000)
RAN_T <- specaccum(ranomafana_t, method="exact", permutations=10000)
ANK <- specaccum(ankafobe, method="exact", permutations=10000)
ANK_A <- specaccum(ankafobe_a, method="exact", permutations=10000)
ANK_T <- specaccum(ankafobe_t, method="exact", permutations=10000)
MAR <- specaccum(marojejy, method="exact", permutations=10000)
MAR_A <- specaccum(marojejy_a, method="exact", permutations=10000)
MAR_T <- specaccum(marojejy_t, method="exact", permutations=10000)
VAI <- specaccum(vaingandrano, method="exact", permutations=10000)
VAI_A <- specaccum(vaingandrano_a, method="exact", permutations=10000)
VAI_T <- specaccum(vaingandrano_t, method="exact", permutations=10000)
A <- specaccum(schedule_A, method="exact", permutations=10000)
B <- specaccum(schedule_B, method="exact", permutations=10000)
AB <- specaccum(schedule_AB, method="exact", permutations=10000)

df <- bind_rows(
  specaccum_to_df(MAR, "Marojejy", "Combined camera"),
  specaccum_to_df(MAR_T, "Marojejy", "Terrestrial camera"),
  specaccum_to_df(MAR_A, "Marojejy", "Arboreal camera"),
  specaccum_to_df(ANA, "Analalava", "Combined camera"),
  specaccum_to_df(ANA_T, "Analalava", "Terrestrial camera"),
  specaccum_to_df(ANA_A, "Analalava", "Arboreal camera"),
  specaccum_to_df(ANK, "Ankafobe", "Combined camera"),
  specaccum_to_df(ANK_T, "Ankafobe", "Terrestrial camera"),
  specaccum_to_df(ANK_A, "Ankafobe", "Arboreal camera"),
  specaccum_to_df(RAN, "Ranomafana", "Combined camera"),
  specaccum_to_df(RAN_T, "Ranomafana", "Terrestrial camera"),
  specaccum_to_df(RAN_A, "Ranomafana", "Arboreal camera"),
  specaccum_to_df(VAI, "Ankarabolava-Agnakatrika", "Combined camera"),
  specaccum_to_df(VAI_T, "Ankarabolava-Agnakatrika", "Terrestrial camera"),
  specaccum_to_df(VAI_A, "Ankarabolava-Agnakatrika", "Arboreal camera"),
  specaccum_to_df(AB, "Ranomafana (audio)", "AudioMoth Schedules A and B"),
  specaccum_to_df(A, "Ranomafana (audio)", "AudioMoth Schedule A"),
  specaccum_to_df(B, "Ranomafana (audio)", "AudioMoth Schedule B")
)

cols <- c(
  "Combined camera" = "green", "Arboreal camera" = "blue", "Terrestrial camera" = "red",
  "AudioMoth Schedules A and B" = "cyan", "AudioMoth Schedule A" = "yellow", "AudioMoth Schedule B" = "violet"
)
lines <- c(
  "Combined camera" = 1, "Arboreal camera" = 3, "Terrestrial camera" = 2,
  "AudioMoth Schedules A and B" = 1, "AudioMoth Schedule A" = 3, "AudioMoth Schedule B" = 2
)

df$Site <- factor(df$Site, levels = c(
  "Marojejy",
  "Analalava",
  "Ankafobe",
  "Ranomafana",
  "Ranomafana (audio)",
  "Ankarabolava-Agnakatrika"
))

b <- ggplot(df, aes(x = Sites, y = Richness,
                    color = Method,
                    linetype = Method,
                    fill = Method)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~Site, scales = "free", ncol = 3) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  labs(x = "Camera Trap Nights / Hours (audio)", y = "Species richness") +
  guides(
    color = guide_legend(nrow = 1, title = NULL),
    fill = guide_legend(nrow = 1, title = NULL),
    linetype = guide_legend(nrow = 1, title = NULL)
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey75"),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

print(b)

















