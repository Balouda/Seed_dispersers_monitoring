rm(list = ls())

library(vegan)
library(ggplot2)


cl<-read.csv("cl.csv")
cl$X<-NULL

fb<-read.csv("fb.csv")
fb$X<-NULL
fb_a<-read.csv("fb_a.csv")
fb_a$X<-NULL
fb_t<-read.csv("fb_t.csv")
fb_t$X<-NULL

forest<-read.csv("forest.csv")
forest$X<-NULL
forest_a<-read.csv("forest_a.csv")
forest_a$X<-NULL
forest_t<-read.csv("forest_t.csv")
forest_t$X<-NULL

nr<-read.csv("nr.csv")
nr$X<-NULL
nr_a<-read.csv("nr_a.csv")
nr_a$X<-NULL
nr_t<-read.csv("nr_t.csv")
nr_t$X<-NULL

plantation<-read.csv("plantation.csv")
plantation$X<-NULL
plantation_a<-read.csv("plantation_a.csv")
plantation_a$X<-NULL
plantation_t<-read.csv("plantation_t.csv")
plantation_t$X<-NULL

restoration<-read.csv("restoration.csv")
restoration$X<-NULL
restoration_a<-read.csv("restoration_a.csv")
restoration_a$X<-NULL
restoration_t<-read.csv("restoration_t.csv")
restoration_t$X<-NULL

forest_AB<-read.csv("SAC_audio_forest_AB.csv")
forest_AB$X<-NULL
forest_A<-read.csv("SAC_audio_forest_A.csv")
forest_A$X<-NULL
forest_B<-read.csv("SAC_audio_forest_B.csv")
forest_B$X<-NULL
restoration_AB<-read.csv("SAC_audio_restoration_AB.csv")
restoration_AB$X<-NULL
restoration_A<-read.csv("SAC_audio_restoration_A.csv")
restoration_A$X<-NULL
restoration_B<-read.csv("SAC_audio_restoration_B.csv")
restoration_B$X<-NULL
regeneration_B<-read.csv("SAC_audio_natural_regeneration_B.csv")
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


par(mfrow = c(3,3), mar = c(2, 2, 1.5, 1.5),
    cex.lab = 0.6, 
    cex.axis = 0.6, 
    cex.main = 0.8,mgp = c(1.8, 0.5, 0))

plot(forest, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", main="Mature protected forest",lty=1)
plot(forest_t, col = "red",lwd= 2,ci.lty =0, ci.type = "poly",ci.col=alpha("red", 0.1),lty=2,add=T)#dots
plot(forest_a,col = "blue",lwd= 2,ci.lty = 0, ci.type = "poly",ci.col=alpha("blue", 0.1) ,lty=3,add=T)#dash
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1) 

plot(restoration, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", main="Reforestation",lty=1)
plot(restoration_a,col = "blue",ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0,lty=3, add=T)
plot(restoration_t, col = "red",  ci.type = "poly",ci.col=alpha("red", 0.1),lwd= 2,ci.lty =0, lty=2, add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(nr, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", ylim=c(), main="Natural regeneration",lty=1)
plot(nr_t, col = "red", ci.type = "poly",ci.col=alpha("red", 0.1), lwd= 2,ci.lty =0, lty=2,add=T)
plot(nr_a,col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0, lty=3,add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(plantation, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", ylim=c(), main="Plantation",lty=1)
plot(plantation_t,col = "red",  ci.type = "poly",ci.col=alpha("red", 0.1),lwd= 2,ci.lty = 0,lty=2,add=T)
plot(plantation_a, col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty =0,lty=3, add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(fb, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",lty=1 ,xlab="",
     main="Burned forest")
plot(fb_t, col = "red", ci.type = "poly",ci.col=alpha("red", 0.1), lwd= 2,ci.lty =0,lty=2,add=T)
plot(fb_a,col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0, lty=3,add= T )
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(cl, col = "red",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("red", 0.1),ylab = "",
     xlab="", main="Cleared land",lty=2)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(for_ab, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", main="Mature protected forest audio",lty=1)
plot(for_a,col = "blue",ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0,lty=3,add=T)
plot(for_b, col = "red",  ci.type = "poly",ci.col=alpha("red", 0.1),lwd= 2,ci.lty =0, lty=2,add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(rest_ab, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", ylim=c(), main="Reforestation audio",lty=1)
plot(rest_b, col = "red", ci.type = "poly",ci.col=alpha("red", 0.1), lwd= 2,ci.lty =0, lty=2,add=T)
plot(rest_a,col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0,lty=3, add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(natreg_b, col = "red",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("red", 0.1),ylab = "",
     xlab="", ylim=c(), main="Natural regeneration audio",lty=2)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)



########################## SACs for Sites ######################################


analalava<-read.csv("analalava.csv")
analalava$X<-NULL
analalava_a<-read.csv("analalava_a.csv")
analalava_a$X<-NULL
analalava_t<-read.csv("analalava_t.csv")
analalava_t$X<-NULL

ranomafana<-read.csv("ranomafana.csv")
ranomafana$X<-NULL
ranomafana_a<-read.csv("ranomafana_a.csv")
ranomafana_a$X<-NULL
ranomafana_t<-read.csv("ranomafana_t.csv")
ranomafana_t$X<-NULL

ankafobe<-read.csv("ankafobe.csv")
ankafobe$X<-NULL
ankafobe_a<-read.csv("ankafobe_a.csv")
ankafobe_a$X<-NULL
ankafobe_t<-read.csv("ankafobe_t.csv")
ankafobe_t$X<-NULL

marojejy<-read.csv("marojejy.csv")
marojejy$X<-NULL
marojejy_a<-read.csv("marojejy_a.csv")
marojejy_a$X<-NULL
marojejy_t<-read.csv("marojejy_t.csv")
marojejy_t$X<-NULL

vaingandrano<-read.csv("vaingandrano.csv")
vaingandrano$X<-NULL
vaingandrano_a<-read.csv("vaingandrano_a.csv")
vaingandrano_a$X<-NULL
vaingandrano_t<-read.csv("vaingandrano_t.csv")
vaingandrano_t$X<-NULL

schedule_A<-read.csv("SAC_audio_schedule_A.csv")
schedule_A$X<-NULL
schedule_B<-read.csv("SAC_audio_schedule_B.csv")
schedule_B$X<-NULL
schedule_AB<-read.csv("SAC_audio_all_AB.csv")
schedule_AB$X<-NULL


#site
ANK<- specaccum(ankafobe, method = "exact",permutations = 10000)
ANK
RAN<- specaccum(ranomafana, method = "exact",permutations = 10000)
RAN
ANA<- specaccum(analalava, method = "exact",permutations = 10000)
ANA
VAI<- specaccum(vaingandrano, method = "exact",permutations = 10000)
VAI
MAR<- specaccum(marojejy, method = "exact",permutations = 10000)
MAR

#camera types
ANA_T<- specaccum(analalava_t, method = "exact",permutations = 10000)
ANA_T
ANA_A<- specaccum(analalava_a, method = "exact",permutations = 10000)
ANA_A
RAN_T<- specaccum(ranomafana_t, method = "exact",permutations = 10000)
RAN_T
RAN_A<- specaccum(ranomafana_a, method = "exact",permutations = 10000)
RAN_A
MAR_T<- specaccum(marojejy_t, method = "exact",permutations = 10000)
MAR_T
MAR_A<- specaccum(marojejy_a, method = "exact",permutations = 10000)
MAR_A
VAI_T<- specaccum(vaingandrano_t, method = "exact",permutations = 10000)
VAI_T
VAI_A<- specaccum(vaingandrano_a, method = "exact",permutations = 10000)
VAI_A
ANK_T<- specaccum(ankafobe_t, method = "exact",permutations = 10000)
ANK_T
ANK_A<- specaccum(ankafobe_a, method = "exact",permutations = 10000)
ANK_A

#audiomoths
AB<- specaccum(schedule_AB, method = "exact",permutations = 10000)
AB
A<- specaccum(schedule_A, method = "exact",permutations = 10000)
A
B<- specaccum(schedule_B, method = "exact",permutations = 10000)
B


par(mfrow = c(2,3),mar = c(3, 3, 2, 2),cex.lab = 0.6, 
    cex.axis = 0.6, 
    cex.main = 0.8,mgp = c(1.8, 0.5, 0))

#camera
plot(MAR, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "Species richeness",
     xlab="Camera Trap Nights", ylim=c(0,20), main="MAROJEJY camera trap",lty=1)
plot(MAR_T,col = "red",  ci.type = "poly",ci.col=alpha("red", 0.1),lwd= 2,ci.lty = 0,lty=2, add=T)
plot(MAR_A, col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty =0,lty=3, add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(ANA, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", main="ANALALAVA camera trap",lty=1)
plot(ANA_T, col = "red",lwd= 2,ci.lty =0, ci.type = "poly",ci.col=alpha("red", 0.1),lty=2,add=T)
plot(ANA_A,col = "blue",lwd= 2,ci.lty = 0, ci.type = "poly", ci.col=alpha("blue", 0.1),lty=3,add=T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1) 

plot(ANK, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",lty=1 ,xlab="",
     main="ANKAFOBE camera trap")
plot(ANK_T, col = "red", ci.type = "poly",ci.col=alpha("red", 0.1), lwd= 2,ci.lty =0,lty=2, add=T)
plot(ANK_A,col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0,lty=3, add= T )
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(RAN, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", main="RANOMAFANA camera trap",lty=1)
plot(RAN_A,col = "blue",ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0,lty=3,add=T)
plot(RAN_T, col = "red",  ci.type = "poly",ci.col=alpha("red", 0.1),lwd= 2,ci.lty =0, lty=2,add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

#audio
plot(AB, col = "cyan",lwd= 2,ci.lty =0, ci.type = "poly",ci.col=alpha("lightblue", 0.1),ylab = "",
     xlab="Hours",lty=1, main="RANOMAFANA acoustic data")
plot(B, col = "violet", lwd=2,ci.lty = 0, add=T,ci.type = "poly", ci.col=alpha("pink", 0.1),lty=2)
plot(A, col = "yellow",ci.type="poly", ci.col=alpha("yellow",0.1), lwd= 2,ci.lty = 0, add=T,lty=3)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)

plot(VAI, col = "green",lwd= 2,ci.lty =0, 
     ci.type = "poly",ci.col=alpha("green", 0.1),ylab = "",
     xlab="", ylim=c(), main="ANKARABOLAVA-AGNAKATRIKA camera trap",lty=1)
plot(VAI_T, col = "red", ci.type = "poly",ci.col=alpha("red", 0.1), lwd= 2,ci.lty =0,lty=2, add=T)
plot(VAI_A,col = "blue", ci.type = "poly", ci.col=alpha("blue", 0.1),lwd= 2,ci.lty = 0,lty=3, add= T)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 0.1)















