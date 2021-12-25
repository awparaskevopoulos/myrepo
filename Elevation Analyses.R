# Anna Paraskevopoulos
# Elevation Analyses

# load the required packages
library(reshape2)
library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)
library(vegan)

# read in the data
ant.data <- read.csv("GC Ant final.csv")

# data cleaning
str(ant.data)
head(ant.data)
names(ant.data)

# remove rows with no recorded ant observation
which(ant.data$Current.Species=="NA ")
omit <- which(ant.data$Current.Species=="NA ")
ant <- ant.data[-c(omit),]
which(ant$Current.Species=="NA ")

# remove row with Pogo observation not associated with sampling site
which(ant$Slope=="")
omit4 <- which(ant$Slope=="")
ant <- ant[-c(omit4),]
which(ant$Slope=="")

# getting subsets of ant data - historic vs current
hist <- subset(ant, ant$Timeframe=="Historical")
cur <- subset(ant, ant$Timeframe=="Contemporary")

# setting up as characters
cur$Current.Species <- as.character(cur$Current.Species)
hist$Current.Species <- as.character(hist$Current.Species)

# Species
unique(cur$Current.Species)
unique(hist$Current.Species)

# --------------------------------------------------------------------------- # APHAENOGASTER OCCIDENTALIS
# HAS THE ELEVATIONAL RANGE OF APHAENOGASTER OCCIDENTALIS CHANGED?

# subsets with Aphaenogaster occidentalis
h.aphocc <- subset(hist, hist$Current.Species=="Aphaenogaster occidentalis")
c.aphocc <- subset(cur, cur$Current.Species=="Aphaenogaster occidentalis")

# number of individual observations
length(h.aphocc$Elevation..m.)
length(c.aphocc$Elevation..m.)

# boxplot visualization
boxplot(h.aphocc$Elevation..m., c.aphocc$Elevation..m., 
        names=c("Browne","Current"), main="Aphaenogaster occidentalis")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.aphocc <- t.test(h.aphocc$Elevation..m., c.aphocc$Elevation..m.)
ele.aphocc
# NOT significant

# ridgeline visualization
aphocc <- subset(ant, ant$Genus=="Aphaenogaster")
ggplot(aphocc, aes(x = Elevation..m., y = Timeframe, 
                   fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")


# not sure about this yet ------------------------------------------------------------------------------ #

# numeric data for ant observations
cur$aphocc[cur$Current.Species=="Aphaenogaster occidentalis"] <- "1"
cur$aphocc[is.na(cur$aphocc)] = 0
cur$aphocc <- as.numeric(cur$aphocc)

hist$aphocc[hist$Current.Species=="Aphaenogaster occidentalis"] <- "1"
hist$aphocc[is.na(hist$aphocc)] = 0
hist$aphocc <- as.numeric(hist$aphocc)

# binomial model
plot(cur$aphocc~cur$Elevation..m.)
lm.aphocc <- glm(cur$aphocc~cur$Elevation..m., data=cur, family=binomial)
summary(lm.aphocc)

# checking the diagnostics
plot(lm.aphocc)


# --------------------------------------------------------------------------- # BRACHYMYRMEX DEPILIS
# HAS THE ELEVATIONAL RANGE OF BRACHYMYRMEX DEPILIS CHANGED?

# subsets with brachymyrmex depilis
h.bradep <- subset(hist, hist$Current.Species=="Brachymyrmex depilis")
c.bradep <- subset(cur, cur$Current.Species=="Brachymyrmex depilis")

# number of individual observations
length(h.bradep$Elevation..m.)
length(c.bradep$Elevation..m.)
# not enough historical observations to make an accurate determination of elevational change

# --------------------------------------------------------------------------- # CAMPONOTUS MODOC
# HAS THE ELEVATIONAL RANGE OF CAMPONOTUS MODOC CHANGED?

# subsets with Camponotus modoc
h.cammod <- subset(hist, hist$Current.Species=="Camponotus modoc")
c.cammod <- subset(cur, cur$Current.Species=="Camponotus modoc")

# number of individual observations
length(h.cammod$Elevation..m.)
length(c.cammod$Elevation..m.)

# boxplot visualization
boxplot(h.cammod$Elevation..m., c.cammod$Elevation..m., 
        names=c("Browne","Current"), main="Camponotus modoc")

# Welch's t-test to see if elevation has changed from 1950's
ele.cammod <- t.test(h.cammod$Elevation..m., c.cammod$Elevation..m.)
ele.cammod
# NOT sigificant

# ridgeline visualization
cammod <- subset(ant, ant$Current.Species=="Camponotus modoc")
ggplot(cammod, aes(x = Elevation..m., y = Timeframe, 
                   fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # CAMPONOTUS VICINUS
# HAS THE ELEVATIONAL RANGE OF CAMPONOTUS VICINUS CHANGED?

# subsets with Camponotus vicinus
h.camvic <- subset(hist, hist$Current.Species=="Camponotus vicinus")
c.camvic <- subset(cur, cur$Current.Species=="Camponotus vicinus")

# number of individual observations
length(h.camvic$Elevation..m.)
length(c.camvic$Elevation..m.)

# boxplot visualization
boxplot(h.camvic$Elevation..m., c.camvic$Elevation..m., 
        names=c("Browne","Current"), main="Camponotus vicinus")

# Welch's t-test to see if elevation has changed from 1950's
ele.camvic <- t.test(h.camvic$Elevation..m., c.camvic$Elevation..m.)
ele.camvic
# significant change in elevation

# ridgeline visualization
camvic <- subset(ant, ant$Current.Species=="Camponotus vicinus")
ggplot(camvic, aes(x = Elevation..m., y = Timeframe, 
                   fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # CAMPONOTUS NOVAEBORACENSIS
# HAS THE ELEVATIONAL RANGE OF CAMPONOTUS NOVAEBORACENSIS CHANGED?

# subsets 
h.camnov <- subset(hist, hist$Current.Species=="Camponotus novaeboracensis")
c.camnov <- subset(cur, cur$Current.Species=="Camponotus novaeboracensis")

# number of individual observations
length(h.camnov$Elevation..m.)
length(c.camnov$Elevation..m.)
# not enough observations to make an accurate determination

# --------------------------------------------------------------------------- # FORELIUS PRUINOSUS
# HAS THE ELEVATIONAL RANGE OF FORELIUS PRUINOSUS CHANGED?

# subsets with Forelius pruinosus
h.forpru <- subset(hist, hist$Current.Species=="Forelius pruinosus")
c.forpru <- subset(cur, cur$Current.Species=="Forelius pruinosus")

# number of individual observations
length(h.forpru$Elevation..m.)
length(c.forpru$Elevation..m.)

# boxplot visualization
boxplot(h.forpru$Elevation..m., c.forpru$Elevation..m., 
        names=c("Browne","Current"), main="Forelius pruinosus")

# Welch's t-test to see if elevation has changed from 1950's
ele.forpru <- t.test(h.forpru$Elevation..m., c.forpru$Elevation..m.)
ele.forpru
# significant change in elevation

# ridgeline visualization
forpru <- subset(ant, ant$Genus=="Forelius")
ggplot(forpru, aes(x = Elevation..m., y = Timeframe, 
                   fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # FORMICA FUSCA
# HAS THE ELEVATIONAL RANGE OF FORMICA FUSCA CHANGED?

# subsets with Formica fusca
h.forfus <- subset(hist, hist$Current.Species=="Formica fusca")
c.forfus <- subset(cur, cur$Current.Species=="Formica fusca")

# number of individual observations
length(h.forfus$Elevation..m.)
length(c.forfus$Elevation..m.)

# boxplot visualization
boxplot(h.forfus$Elevation..m., c.forfus$Elevation..m., 
        names=c("Browne","Current"), main="Formica fusca")

# Welch's t-test to see if elevation has changed from 1950's
ele.forfus <- t.test(h.forfus$Elevation..m., c.forfus$Elevation..m.)
ele.forfus
# NOT significant

# ridgeline visualization
forfus <- subset(ant, ant$Current.Species=="Formica fusca")
ggplot(forfus, aes(x = Elevation..m., y = Timeframe, 
                   fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # FORMICA neorufibarbis
# HAS THE ELEVATIONAL RANGE OF FORMICA neorufibarbis CHANGED?

# subsets with Formica neorufibarbis
h.forneo <- subset(hist, hist$Current.Species=="Formica neorufibarbis")
c.forneo <- subset(cur, cur$Current.Species=="Formica neorufibarbis")

# number of individual observations
length(h.forneo$Elevation..m.)
length(c.forneo$Elevation..m.)

# boxplot visualization
boxplot(h.forneo$Elevation..m., c.forneo$Elevation..m., 
        names=c("Browne","Current"), main="Formica neorufibarbis")

# Welch's t-test to see if elevation has changed from 1950's
ele.forneo <- t.test(h.forneo$Elevation..m., c.forneo$Elevation..m.)
ele.forneo

# ridgeline visualization
forneo <- subset(ant, ant$Current.Species=="Formica neorufibarbis")
ggplot(forneo, aes(x = Elevation..m., y = Timeframe, 
                   fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # FORMICA RAVIDA
# HAS THE ELEVATIONAL RANGE OF FORMICA RAVIDA CHANGED?

# subsets with Formica ravida
h.forrav <- subset(hist, hist$Current.Species=="Formica ravida")
c.forrav <- subset(cur, cur$Current.Species=="Formica ravida")

# number of individual observations
length(h.forrav$Elevation..m.)
length(c.forrav$Elevation..m.)


# --------------------------------------------------------------------------- # LASIUS AMERICANUS
# HAS THE ELEVATIONAL RANGE OF LASIUS AMERICANUS CHANGED?

# subsets with Lasius americanus
h.lasame <- subset(hist, hist$Current.Species=="Lasius americanus")
c.lasame <- subset(cur, cur$Current.Species=="Lasius americanus")

# number of individual observations
length(h.lasame$Elevation..m.)
length(c.lasame$Elevation..m.)
# boxplot visualization
boxplot(h.lasame$Elevation..m., c.lasame$Elevation..m., 
        names=c("Browne","Current"), main="Lasius americanus")
# Welch's t-test to see if elevation has changed from 1950's
ele.lasame <- t.test(h.lasame$Elevation..m., c.lasame$Elevation..m.)
# ridgeline visualization
lasame <- subset(ant, ant$Current.Species=="Lasius americanus")
ggplot(lasame, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")
# --------------------------------------------------------------------------- # LASIUS PALLITARSIS
#  HAS THE ELEVATIONAL RANGE OF LASIUS PALLITARSIS CHANGED?

# subsets of Lasius pallitarsis
h.laspal <- subset(hist, hist$Current.Species=="Lasius pallitarsis")
c.laspal <- subset(cur, cur$Current.Species=="Lasius pallitarsis")
length(h.laspal$Elevation..m.)
length(c.laspal$Elevation..m.)
# boxplot visualization
boxplot(h.laspal$Elevation..m., c.laspal$Elevation..m., 
        names=c("Browne","Current"),main="Lasius pallitarsis")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.laspal <- t.test(h.laspal$Elevation..m., c.laspal$Elevation..m.)
ele.laspal

# ridgeline visualization
laspal <- subset(ant, ant$Current.Species=="Lasius pallitarsis")
ggplot(laspal, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation m")+ylab("")


# --------------------------------------------------------------------------- # LEPTOTHORAX CRASSIPILIS
# HAS THE ELEVATIONAL RANGE OF LEPTOTHORAX CRASSIPILIS CHANGED?

# subsets with Leptothorax crassipilis
h.lepcra <- subset(hist, hist$Current.Species=="Leptothorax crassipilis")
c.lepcra <- subset(cur, cur$Current.Species=="Leptothorax crassipilis")
length(h.lepcra$Elevation..m.)
length(c.lepcra$Elevation..m.)

# boxplot visualization
boxplot(h.lepcra$Elevation..m., c.lepcra$Elevation..m., 
        names=c("Browne","Current"), main="Leptothorax crassipilis")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.lepcra <- t.test(h.lepcra$Elevation..m., c.lepcra$Elevation..m.)
ele.lepcra

# ridgeline visualization
lepcra <- subset(ant, ant$Current.Species=="Leptothorax crassipilis")
ggplot(lepcra, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # LIOMETOPUM APICULATUM
#  HAS THE ELEVATIONAL RANGE OF LIOMETOPUM APICULATUM CHANGED?

# subsets of Liometopum apiculatum
h.lioapi <- subset(hist, hist$Current.Species=="Liometopum apiculatum")
c.lioapi <- subset(cur, cur$Current.Species=="Liometopum apiculatum")

length(h.lioapi$Elevation..m.)
length(c.lioapi$Elevation..m.)

# boxplot visualization
boxplot(h.lioapi$Elevation..m., c.lioapi$Elevation..m., 
        names=c("Browne","Current"),main="Liometopum apiculatum")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.lioapi <- t.test(h.lioapi$Elevation..m., c.lioapi$Elevation..m.)
ele.lioapi

# ridgeline visualization
lioapi <- subset(ant, ant$Current.Species=="Liometopum apiculatum")
ggplot(lioapi, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation m")+ylab("")

# --------------------------------------------------------------------------- # LIOMETOPUM LUCTUOSUM
# HAS THE ELEVATIONAL RANGE OF LIOMETOPUM LUCTUOSUM CHANGED?

# subsets with Liometopum luctuosum
h.lioluc <- subset(hist, hist$Current.Species=="Liometopum luctuosum")
c.lioluc <- subset(cur, cur$Current.Species=="Liometopum luctuosum")
length(h.lioluc$Elevation..m.)
length(c.lioluc$Elevation..m.)

# boxplot visualization
boxplot(h.lioluc$Elevation..m., c.lioluc$Elevation..m., 
        names=c("Browne","Current"), main="Liometopum luctuosum")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.lioluc <- t.test(h.lioluc$Elevation..m., c.lioluc$Elevation..m.)
ele.lioluc

# ridgeline visualization
lioluc <- subset(ant, ant$Current.Species=="Liometopum luctuosum")
ggplot(lioluc, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # MYRMICA FRACTICORNIS
# HAS THE ELEVATIONAL RANGE OF MYRMICA FRACTICORNIS CHANGED?

# subsets with Myrmica fracticornis
h.myrfra <- subset(hist, hist$Current.Species=="Myrmica fracticornis")
c.myrfra <- subset(cur, cur$Current.Species=="Myrmica fracticornis")
length(h.myrfra$Elevation..m.)
length(c.myrfra$Elevation..m.)

# boxplot visualization
boxplot(h.myrfra$Elevation..m., c.myrfra$Elevation..m., 
        names=c("Browne","Current"), main="Myrmica fracticornis")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.myrfra <- t.test(h.myrfra$Elevation..m., c.myrfra$Elevation..m.)
ele.myrfra

# ridgeline visualization
myrfra <- subset(ant, ant$Current.Species=="Myrmica fracticornis")
ggplot(myrfra, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # MYRMICA MONTICOLA
# HAS THE ELEVATIONAL RANGE OF MYRMICA MONTICOLA CHANGED?

# subsets with Myrmica monticola
h.myrmon <- subset(hist, hist$Current.Species=="Myrmica monticola")
c.myrmon <- subset(cur, cur$Current.Species=="Myrmica monticola")
length(h.myrmon$Elevation..m.)
length(c.myrmon$Elevation..m.)

# boxplot visualization
boxplot(h.myrmon$Elevation..m., c.myrmon$Elevation..m., 
        names=c("Browne","Current"), main="Myrmica monticola")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.myrmon <- t.test(h.myrmon$Elevation..m., c.myrmon$Elevation..m.)
ele.myrmon

# ridgeline visualization
myrmon <- subset(ant, ant$Current.Species=="Myrmica monticola")
ggplot(myrmon, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # PHEIDOLE CERES
# HAS THE ELEVATIONAL RANGE OF PHEIDOLE CERES CHANGED?

# subsets with Pheidole ceres
h.phecer <- subset(hist, hist$Current.Species=="Pheidole ceres")
c.phecer <- subset(cur, cur$Current.Species=="Pheidole ceres")
length(h.phecer$Elevation..m.)
length(c.phecer$Elevation..m.)

# boxplot visualization
boxplot(h.phecer$Elevation..m., c.phecer$Elevation..m., 
        names=c("Browne","Current"), main="Pheidole ceres")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.phecer <- t.test(h.phecer$Elevation..m., c.phecer$Elevation..m.)
ele.phecer

# ridgeline visualization
phecer <- subset(ant, ant$Current.Species=="Pheidole ceres")
ggplot(phecer, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")

# --------------------------------------------------------------------------- # STENAMMA DIECKI
# HAS THE ELEVATIONAL RANGE OF STENAMMA DIECKI CHANGED?

# subsets with Stenamma diecky
h.stedie <- subset(hist, hist$Current.Species=="Stenamma diecki")
c.stedie <- subset(cur, cur$Current.Species=="Stenamma diecki")
length(h.stedie$Elevation..m.)
length(c.stedie$Elevation..m.)

# boxplot visualization
boxplot(h.stedie$Elevation..m., c.stedie$Elevation..m., 
        names=c("Browne","Current"), main="Stenamma diecki")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.stedie <- t.test(h.stedie$Elevation..m., c.stedie$Elevation..m.)
ele.stedie

# ridgeline visualization
stedie <- subset(ant, ant$Current.Species=="Stenamma diecki")
ggplot(stedie, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation (m)")+ylab("")


# --------------------------------------------------------------------------- # TAPINOMA SESSILE
#  HAS THE ELEVATIONAL RANGE OF TAPINOMA SESSILE CHANGED?

# subsets with tapinoma sessile
h.tapses <- subset(hist, hist$Current.Species=="Tapinoma sessile")
c.tapses <- subset(cur, cur$Current.Species=="Tapinoma sessile")

length(h.tapses$Elevation..m.)
length(c.tapses$Elevation..m.)

# boxplot visualization
boxplot(h.tapses$Elevation..m., c.tapses$Elevation..m., 
        names=c("Browne","Current"),main="Tapinoma sessile")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.tapses <- t.test(h.tapses$Elevation..m., c.tapses$Elevation..m.)
ele.tapses

# ridgeline visualization
tapses <- subset(ant, ant$Genus=="Tapinoma")
ggplot(tapses, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation m")+ylab("")


# --------------------------------------------------------------------------- # TEMNOTHORAX RUGATULUS
#  HAS THE ELEVATIONAL RANGE OF TEMNOTHORAX RUGATULUS CHANGED?

# subsets of Temnothorax rugatulus
h.temrug <- subset(hist, hist$Current.Species=="Temnothorax rugatulus")
c.temrug <- subset(cur, cur$Current.Species=="Temnothorax rugatulus")

length(h.temrug$Elevation..m.)
length(c.temrug$Elevation..m.)

# boxplot visualization
boxplot(h.temrug$Elevation..m., c.temrug$Elevation..m., 
        names=c("Browne","Current"),main="Temnothorax rugatulus")

# Welch's t-test to see if elevation has changed from 1950's
# use a Welch's t-test because of the varying sample sizes
ele.temrug <- t.test(h.temrug$Elevation..m., c.temrug$Elevation..m.)
ele.temrug

# ridgeline visualization
temrug <- subset(ant, ant$Current.Species=="Temnothorax rugatulus")
ggplot(temrug, aes(x = Elevation..m., y = Current.or.Historical, 
                   fill = Current.or.Historical)) + 
  geom_density_ridges(scale = 4, alpha = 0.6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation m")+ylab("")

# --------------------------------------------------------------------------- #

chele <- ant[ant$Current.Species %in% c("Camponotus vicinus", 
                                        "Forelius pruinosus", "Leptothorax crassipilis"), ]
chele

# PHENORIDGES PLOT AT CANYON LEVEL (SPECIES BY ELEVATION)
ggplot(chele, aes(x = Elevation..m., y = Current.Species, fill = Timeframe)) + 
  geom_density_ridges(scale = 2, alpha = .6) + 
  scale_fill_cyclical(values = c("darkcyan","goldenrod"), guide = "legend") +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 14)) +
  labs(x = "Elevation (m)",
       y = "Ant Species")


noch <- ant[ant$Current.Species %in% c("Aphaenogaster occidentalis", 
                                       "Brachymyrmex depilis", 
                                       "Formica fusca", "Formica neorufibarbis", 
                                       "Lasius americanus", "Lasius pallitarsis", 
                                       "Liometopum apiculatum", 
                                       "Myrmica monticola", "Pheidole ceres", 
                                       "Stenamma diecki", "Tapinoma sessile", 
                                       "Temnothorax rugatulus"), ]


# PHENORIDGES PLOT AT CANYON LEVEL (SPECIES BY ELEVATION)
ggplot(noch, aes(x = Elevation..m., y = Current.Species, 
                 fill = Timeframe)) + 
  geom_density_ridges(scale = 2, alpha = .6) + 
  scale_fill_cyclical(values = c("darkcyan","goldenrod"), guide = "legend") +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 14)) +
  ylab("Ant Species") +
  xlab("Elevation (m)")
