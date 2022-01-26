# Anna Paraskevopoulos
# Gregory Canyon Analyses - Aspect

# load the required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)

# ---------------------------------- DATA ----------------------------------- #

ant.data <- read.csv("GC Ant final.csv")

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

# ---------------------------------- DATA ----------------------------------- #

# setting up the null proportions
# based off of the number of collections by slope
# are collection better to go off of because it accounts for the sampling effort differences
slope <- table(cur$Slope)
sum(slope)
null <- c(419/3622, 1686/3622, 1517/3622)

# --------------------------------------------------------------------------- # APHAENOGASTER OCCIDENTALIS
# Do they occupy a specific slope?
# Browne & Gregg determination: BROAD DISTRIBUTION

# getting a subset of the species
aphocc <- subset(ant, ant$Genus=="Aphaenogaster")
# creating a table of the values
t.aphocc <- table(aphocc$Timeframe, aphocc$Slope)
addmargins(t.aphocc)
# running a chi-squared test
ct.aphocc <- chisq.test(y=aphocc$Slope, x=aphocc$Timeframe, correct=FALSE)
ct.aphocc
ct.aphocc$expected
# Aphaenogaster occidentalis maintains a broad distribution

# contemporary data subset
c.aphocc <- subset(cur, cur$Current.Species=="Aphaenogaster occidentalis")
table(c.aphocc$Slope)
o.aphocc <- c(2, 39, 21)
sum(o.aphocc)
e.aphocc <- c(null * sum(o.aphocc))
e.aphocc
tab.aphocc <- rbind(o.aphocc, e.aphocc)
colnames(tab.aphocc)<- c("CB","NS","SS")
tab.aphocc
chisq.test(x = o.aphocc, p = null)

# --------------------------------------------------------------------------- # BRACHYMYRMEX DEPILIS
# Do they occupy a specific slope?
# Browne & Gregg determination: NORTH SLOPE DISTRIBUTION

# getting a subset of the species
bradep <- subset(ant, ant$Genus=="Brachymyrmex")
# creating a table of the values
t.bradep <- table(bradep$Timeframe, bradep$Slope)
addmargins(t.bradep)
# Not enough samples to make an accurate determination

# contemporary data subset
c.bradep <- subset(cur, cur$Current.Species=="Brachymyrmex depilis")
table(c.bradep$Slope)
o.bradep <- c(4, 8, 18)
sum(o.bradep)
e.bradep <- c(null * sum(o.bradep))
e.bradep
tab.bradep <- rbind(o.bradep, e.bradep)
colnames(tab.bradep)<- c("CB","NS","SS")
tab.bradep
chisq.test(x = o.bradep, p = null)

# --------------------------------------------------------------------------- # CAMPONOTUS MODOC
# Do they occupy a specific slope?
# Browne & Gregg determination: NORTH SLOPE DISTRIBUTION

# getting a subset of the species
cammod <- subset(ant, ant$Current.Species=="Camponotus modoc")
# creating a table of the values
t.cammod <- table(cammod$Timeframe, cammod$Slope)
addmargins(t.cammod)
# Not enough statistical power since multiple categories are under 5 samples

# contemporary data subset
c.cammod <- subset(cur, cur$Current.Species=="Camponotus modoc")
table(c.cammod$Slope)
o.cammod <- c(5, 46, 2)
sum(o.cammod)
e.cammod <- c(null * sum(o.cammod))
e.cammod
tab.cammod <- rbind(o.cammod, e.cammod)
colnames(tab.cammod)<- c("CB","NS","SS")
tab.cammod
chisq.test(x = o.cammod, p = null)


# --------------------------------------------------------------------------- # CAMPONOTUS VICINUS
# Do they occupy a specific slope?
# Browne & Gregg determination: BROAD DISTRIBUTION

# getting a subset of the species
camvic <- subset(ant, ant$Current.Species=="Camponotus vicinus")
# creating a table of the values
t.camvic <- table(camvic$Timeframe, camvic$Slope)
addmargins(t.camvic)
# running a chi-squared test
ct.camvic <- chisq.test(y=camvic$Slope, x=camvic$Timeframe, correct=FALSE)
ct.camvic
ct.camvic$expected
# Camponotus vicinus maintains a broad distribution

# contemporary data subset
c.camvic <- subset(cur, cur$Current.Species=="Camponotus vicinus")
table(c.camvic$Slope)
o.camvic <- c(12, 54, 55)
sum(o.camvic)
e.camvic <- c(null * sum(o.camvic))
e.camvic
tab.camvic <- rbind(o.camvic, e.camvic)
colnames(tab.camvic)<- c("CB","NS","SS")
tab.camvic
chisq.test(x = o.camvic, p = null)

# --------------------------------------------------------------------------- # CAMPONOTUS NOVAEBORACENSIS
# Do they occupy a specific slope?
# Browne & Gregg determination: NORTH SLOPE DISTRIBUTION

# getting a subset of the species
camnov <- subset(ant, ant$Current.Species=="Camponotus novaeboracensis")
# creating a table of the values
t.camnov <- table(camnov$Timeframe, camnov$Slope)
addmargins(t.camnov)
# not enough observations to make an accurate determination

# --------------------------------------------------------------------------- # FORELIUS PRUINOSUS
# Do they occupy a specific slope?
# Browne & Gregg determination: SOUTH SLOPE DISTRIBUTION

# getting a subset of the species
forpru <- subset(ant, ant$Genus=="Forelius")
# creating a table of the values
t.forpru <- table(forpru$Timeframe, forpru$Slope)
addmargins(t.forpru)
# not enough observations

# contemporary data subset
c.forpru <- subset(cur, cur$Current.Species=="Forelius pruinosus")
table(c.forpru$Slope)
o.forpru <- c(50, 3, 193)
sum(o.forpru)
e.forpru <- c(null * sum(o.forpru))
e.forpru
tab.forpru <- rbind(o.forpru, e.forpru)
colnames(tab.forpru)<- c("CB","NS","SS")
tab.forpru
chisq.test(x = o.forpru, p = null)

# --------------------------------------------------------------------------- # FORMICA FUSCA
# Do they occupy a specific slope?
# Browne & Gregg determination: BROAD DISTRIBUTION

# getting a subset of the species
forfus <- subset(ant, ant$Current.Species=="Formica fusca")
# creating a table of the values
t.forfus <- table(forfus$Timeframe, forfus$Slope)
addmargins(t.forfus)
# running a chi-squared test
ct.forfus <- chisq.test(y=forfus$Slope, x=forfus$Timeframe, correct=FALSE)
ct.forfus
ct.forfus$expected

# contemporary data subset
c.forfus <- subset(cur, cur$Current.Species=="Formica fusca")
table(c.forfus$Slope)
o.forfus <- c(11, 23, 4)
sum(o.forfus)
e.forfus <- c(null * sum(o.forfus))
e.forfus
tab.forfus <- rbind(o.forfus, e.forfus)
colnames(tab.forfus)<- c("CB","NS","SS")
tab.forfus
chisq.test(x = o.forfus, p = null)

# --------------------------------------------------------------------------- # FORMICA NEORUFIBARBIS
# Do they occupy a specific slope?
# Browne & Gregg determination: NORTH SLOPE DISTRIBUTION

# getting a subset of the species
forneo <- subset(ant, ant$Current.Species=="Formica neorufibarbis")
# creating a table of the values
t.forneo <- table(forneo$Timeframe, forneo$Slope)
addmargins(t.forneo)
# running a chi-squared test
ct.forneo <- chisq.test(y=forneo$Slope, x=forneo$Timeframe, correct=FALSE)
ct.forneo
ct.forneo$expected

# --------------------------------------------------------------------------- # LASIUS AMERICANUS
# Do they occupy a specific slope?
# Browne & Gregg determination: SOUTH SLOPE DISTRIBUTION

# contemporary data subset
c.lasame <- subset(cur, cur$Current.Species=="Lasius americanus")
table(c.lasame$Slope)
o.lasame <- c(3, 3, 6)
sum(o.lasame)
e.lasame <- c(null * sum(o.lasame))
e.lasame
tab.lasame <- rbind(o.lasame, e.lasame)
colnames(tab.lasame)<- c("CB","NS","SS")
tab.lasame
chisq.test(x = o.lasame, p = null)

# --------------------------------------------------------------------------- # LASIUS PALLITARSIS
# Do they occupy a specific slope?
# Browne & Gregg determination: NORTH SLOPE DISTRIBUTION

# getting a subset of the species
laspal <- subset(ant, ant$Current.Species=="Lasius pallitarsis")
# creating a table of the values
t.laspal <- table(laspal$Timeframe, laspal$Slope)
addmargins(t.laspal)
# running a chi-squared test
ct.laspal <- chisq.test(y=laspal$Slope, x=laspal$Timeframe, correct=FALSE)
ct.laspal
ct.laspal$expected

# contemporary data subset
c.laspal <- subset(cur, cur$Current.Species=="Lasius pallitarsis")
table(c.laspal$Slope)
o.laspal <- c(50, 387, 89)
sum(o.laspal)
e.laspal <- c(null * sum(o.laspal))
e.laspal
tab.laspal <- rbind(o.laspal, e.laspal)
colnames(tab.laspal)<- c("CB","NS","SS")
tab.laspal
chisq.test(x = o.laspal, p = null)

# --------------------------------------------------------------------------- # LEPTOTHORAX CRASSIPILIS
# Do they occupy a specific slope?
# Browne & Gregg determination: NORTH SLOPE DISTRIBUTION

# getting a subset of the species
lepcra <- subset(ant, ant$Current.Species=="Leptothorax crassipilis")
# creating a table of the values
t.lepcra <- table(lepcra$Timeframe, lepcra$Slope)
addmargins(t.lepcra)
# not enough statistical power









