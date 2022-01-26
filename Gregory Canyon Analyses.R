# Anna Paraskevopoulos
# Gregory Canyon Analyses - community composition

# load the required packages
library(reshape2)
library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)
library(vegan)

# ---------------------------------- DATA ----------------------------------- #

soil <- read.csv("GC Soil Data 1958 & 2021.csv")
env <- read.csv("GC Enviro. Data working.csv")
ant.data <- read.csv("GC Ant final.csv")
w1957 <- read.csv("1957 Weather.csv")
w1958 <- read.csv("1958 Weather.csv")
w2021 <- read.csv("2021 Weather.csv")
wea <- read.csv("Site Date Weather Station.csv")
ws <- read.csv("Weather Station.csv")
weaw <- read.csv("Site Date Weather Station wide.csv")


# --------------------------------------------------------------------------- #
# ----------------------------- SOIL MOISTURE ------------------------------- #
# --------------------------------------------------------------------------- #

# data structure
str(soil)
length(soil)
summary(soil)

# making sure things are represented as categories
is.factor(soil$Locality)
soil$Locality <- factor(soil$Locality)

# --------------------------------------------------------------------------- # DOES SOIL MOISTURE VARY BETWEEN
                                                                              # THE CURRENT AND HISTORICAL SAMPLES?
# setting study timeframe (Current or Browne) as a factor
is.factor(soil$Timeframe)
soil$Timeframe <- factor(soil$Timeframe)

# getting subsets of historic vs current
# soil historic = soh
# soil current = soc
soh <- subset(soil, soil$Timeframe=="Historical")
soc <- subset(soil, soil$Timeframe=="Contemporary")

# plotting the data
hist(soh$Soil.moisture)
hist(soc$Soil.moisture)

# means of soil moisture between the historic and current datasets
mean.soh <- mean(soh$Soil.moisture)
mean.soc <- mean(soc$Soil.moisture)

# t test to compare current and historic soil moisture
soil.t1 <- t.test(soh$Soil.moisture, soc$Soil.moisture, paired=T)
soil.t1



# --------------------------------------------------------------------------- # SOIL MOISTURE COMPARISON BETWEEN 
                                                                              # SLOPES AND TIMEFRAME
# subsets of historic and current to slopes
soh.CB <- subset(soh, soh$Locality=="Canyon Bottom")
soh.SS <- subset(soh, soh$Locality=="South Slope")
soh.NS <- subset(soh, soh$Locality=="North Slope")
soc.CB <- subset(soc, soc$Locality=="Canyon Bottom")
soc.SS <- subset(soc, soc$Locality=="South Slope")
soc.NS <- subset(soc, soc$Locality=="North Slope")

# comparison of historic and current soil moisture on canyon bottom
soil.CB <- t.test(soh.CB$Soil.moisture, soc.CB$Soil.moisture, paired=T)
soil.CB

# comparison of historic and current soil moisture on south slopes
soil.SS <- t.test(soh.SS$Soil.moisture, soc.SS$Soil.moisture, paired=T)
soil.SS

# comparison of historic and current soil moisture on north slopes
soil.NS <- t.test(soh.NS$Soil.moisture, soc.NS$Soil.moisture, paired=T)
soil.NS


# box plot
ggplot(soil, aes(x=Locality, y=Soil.moisture, fill=Timeframe)) +
  geom_boxplot() + scale_fill_cyclical(values = c("darkcyan","goldenrod"), guide = "legend") +
  labs(x = "Locality", y = "% moisture in soil", 
       title = "Comparison of soil moisture by slope in Gregory Canyon")



# --------------------------------------------------------------------------- #
# ----------------------- ENVIRONMENTAL FACTORS ----------------------------- #
# --------------------------------------------------------------------------- #

str(env)
length(env)
summary(env)


# --------------------------------------------------------------------------- # COMPARING SAMPLING SEASON AMBIENT
                                                                              # TEMPERATURES FROM BOULDER WEATHER                                                                               # STATION
# BOULDER WEATHER STATION - SAMPLING SEASON TEMPERATURES
ggplot(ws, aes(x = Julian.Date, y = TOBSC, col=as.factor(Year))) +
  geom_point(size=1.5) +
  scale_color_manual(values=c("blue", "green", "black")) +
  geom_smooth(method="loess") +
  labs(x = "Date",
       y = "Temperature (C)",
       title = "Daily Temperature During Sampling Season (C)",
       subtitle = "Boulder, Colorado",
       color = "Year\n")
  
# GLM
temp.aov <- anova(lm(ws$m.obs~ws$Julian.Date*ws$Year, ws))
temp.aov
summary(temp.aov)

# Maximum temperatures
ggplot(ws, aes(x = Julian.Date, y = TMAXC, col=as.factor(Year))) +
  geom_point(size=1.5) +
  scale_color_manual(values=c("blue", "green", "black")) +
  geom_smooth(method="loess") +
  labs(x = "Date",
       y = "Temperature (C)",
       title = "Maximum Daily Temperature During Sampling Season (C)",
       subtitle = "Boulder, Colorado",
       color = "Year\n")

# Minimum temperatures
ggplot(ws, aes(x = Julian.Date, y = TMINC, col=as.factor(Year))) +
  geom_point(size=1.5) +
  scale_color_manual(values=c("blue", "green", "black")) +
  geom_smooth(method="loess") +
  labs(x = "Date",
       y = "Temperature (C)",
       title = "Maximum Daily Temperature During Sampling Season (C)",
       subtitle = "Boulder, Colorado",
       color = "Year\n")

# --------------------------------------------------------------------------- # DIFFERENCE MEASUREMENTS FOR WEATHER 
                                                                              # STATION TEMPERATURE DATA
# OBSERVED TEMPS ON SAMPLING DAYS
head(weaw)
weaw$diff <- weaw$COBSC - weaw$HOBSC
mean_diff <- mean(weaw$diff)
lower <- mean_diff - 1.96*sd(weaw$diff)
upper <- mean_diff + 1.96*sd(weaw$diff)
weaw$Site <- as.factor(weaw$Site)
weaw$Site <- factor(weaw$Site, levels = c("27", "28", "29", "30", "31", "1", 
                                          "32", "33", "2", "3", "4", "5", "6", 
                                          "7", "8", "9", "10", "11", "12", "13",
                                          "14", "15", "16", "17", "18", "19", "20"))
# plotting
ggplot(weaw, aes(x=Site, y=diff)) + 
  geom_bar(stat = 'identity', aes(fill = diff>0), position = 'dodge', col = 'transparent') + 
  geom_hline(yintercept = lower, color = "black", linetype="dashed") +
  geom_hline(yintercept = upper, color = "black", linetype="dashed") +
  theme_bw() + scale_fill_manual(values = c("blue", "red"), guide = 'none') +
  ylab("Difference in Temperature (C)") +
  xlab("Sampling Site")



#---------------------------------------------------------------------------- #
# ------------------------------ ANT DATA ----------------------------------- #
#---------------------------------------------------------------------------- #

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

unique(cur$Current.Species)
unique(cur$Genus)

#---------------------------------------------------------------------------- # DO SURFACE TEMPERATURES DIFFER
# BETWEEN SLOPES
# subset of current ant data
current <- subset(ant, ant$Timeframe=="Contemporary")
current

# NEED TO GET RID OF THE OBSERVATIONS THAT DO NOT HAVE A SURFACE TEMPERATURE
cur <- na.omit(current)
cur
is.numeric(current$Surface.Temp.)

# getting subsets of the slopes
SStem <- subset(cur, cur$Slope=="South Slope")
CBtem <- subset(cur, cur$Slope=="Canyon Bottom")
NStem <- subset(cur, cur$Slope=="North Slope")

# checking means
SSt.mean <- mean(SStem$Surface.Temp.)
CBt.mean <- mean(CBtem$Surface.Temp.)
NSt.mean <- mean(NStem$Surface.Temp.)

# run an ANOVA
# st.aov = surface temperature anova
st.aov <- aov(cur$Surface.Temp.~cur$Slope, data=cur)
summary(st.aov)

# run a Tukey test to see if soil moisture differs
TukeyHSD(st.aov)

# box plot
ggplot(ant, aes(x=Slope, y=Surface.Temp.)) +
  geom_boxplot() +
  labs(x = "Locality", y = "Surface Temperature (C)", 
       title = "Surface temperature (C) range by slope in Gregory Canyon")

#---------------------------------------------------------------------------- # DO AIR TEMPERATURES DIFFER
# BETWEEN SLOPES
# checking means
SSat.mean <- mean(SStem$Air.Temp.)
CBat.mean <- mean(CBtem$Air.Temp.)
NSat.mean <- mean(NStem$Air.Temp.)

# run an ANOVA
# at.aov = air temperature anova
at.aov <- aov(cur$Air.Temp.~cur$Slope, data=cur)
summary(at.aov)

# run a Tukey test to see if soil moisture differs
TukeyHSD(at.aov)

# box plot
ggplot(ant, aes(x=Slope, y=Air.Temp.)) +
  geom_boxplot() +
  labs(x = "Locality", y = "Air Temperature (C)", 
       title = "Air temperature (C) range by slope in Gregory Canyon")

# --------------------------------------------------------------------------- # RIDGELINE PLOT OF SPECIES AND
                                                                              # GENERA
# ridgeline plot with all species
ggplot(ant, aes(x = Elevation..m., y = Current.Species, fill = Genus)) + 
  geom_density_ridges(scale = 8, alpha = .6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation m")+ylab("")

# ridgeline plot with all genera
ggplot(ant, aes(x = Elevation..m., y = Genus, fill = Genus)) + 
  geom_density_ridges(scale = 8, alpha = .6)+theme_ridges()+
  theme(axis.title.y = element_blank())+
  xlab("Elevation m")+ylab("")


# --------------------------------------------------------------------------- # PHENORIDGE PLOT OF CURRENT VS. 
                                                                              # HISTORICAL SPECIES BASED ON 
                                                                              # ELEVATOION AND SAMPLING SITE
# PHENORIDGES PLOT AT CANYON LEVEL (SPECIES BY SITE)
ggplot(ant, aes(x = Sampling.Site, y = Current.Species, 
                fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = .6) + 
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")

# PHENORIDGES PLOT AT CANYON LEVEL (SPECIES BY ELEVATION)
ggplot(ant, aes(x = Elevation..m., y = Current.Species, 
                fill = Timeframe)) + 
  geom_density_ridges(scale = 4, alpha = .6) + 
  scale_fill_cyclical(values = c("darkcyan", "goldenrod"), guide = "legend")



# --------------------------------------------------------------------------- # SPECIES RICHNESS COMPARISON BETWEEN 
                                                                              # CURRENT AND HISTORICAL SAMPLES
# creating wide data from historic and current data subsets

hist.mat <- acast(hist, Sampling.Site ~ Current.Species, value.var= "Occurrence", 
                  fun.aggregate = sum)
cur.mat <- acast(cur, Sampling.Site ~ Current.Species, value.var= "Occurrence", 
                 fun.aggregate = sum)

hist.mat
str(hist.mat)
str(cur.mat)

# visualize difference in site level species richness
boxplot(rowSums(hist.mat), rowSums(cur.mat))

# ttest to compare species richness
t.test(rowSums(hist.mat), rowSums(cur.mat), paired = T)

# calculate observed and estimated species richness
sphist <- specpool(hist.mat) 
spcur <- specpool(cur.mat)

# inspect
sphist
spcur

# species accumulation curves
sahist <- specaccum(hist.mat)
sacur <- specaccum(cur.mat)

# plot accumulation curve with 95% confidence intervals 
plot(sahist, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="pink",
     ylim=c(0,55),xlim=c(0,30),xlab="site sampled",ylab="ant species richness")

plot(sacur, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",
     ylim=c(0,55),xlim=c(0,30),xlab="site",ylab="ant species richness", add=T)

# NMDS PLOT BETWEEN CURRENT AND HISTORIC
# creating a matrix that is only species counts for current and historical data
ants <- ant %>% group_by(Sampling.Site, Timeframe, Current.Species) %>%
  summarise(n.obs=n())
slope <- spread(key = Current.Species, value = n.obs, fill = 0, data=ants)
slope

# remove the first and second rows that specify current vs historical and slope
# odd rows are current
# even rows are historical
# rows 1 - 40 are sites 1-20, rows 41 - 54 are sites 27 - 33
slope$Timeframe <- NULL
slope$Sampling.Site <- NULL
slope

# need to standardize abundance - divide each observation by the total
slope <- slope/rowSums(slope)
slope

# plot
set.seed(2) # setting the starting point of a random number generator
nmds1 <- metaMDS(slope, k=2, trymax=500)
stressplot(nmds1)
plot(nmds1)

ordiplot(nmds1,type="n")
orditorp(nmds1,display="species",col="red",air=0.01)
orditorp(nmds1,display="sites",cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 27)
ordiplot(nmds1,type="n")
ordihull(nmds1,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds1,display="species",col="red",air=0.01)
         
colors=rep(c("gray58","darkslategray"), 27)
ordiplot(nmds1,type="n")
# Plot convex hulls with colors based on treatment
for(i in unique(treat)) {
ordihull(nmds1$point[grep(i,treat),],draw="polygon",
groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds1,display="sites",cex=1,air=0.01)
legend("topright", legend=c("Contemporary", "Historical"), pch = 19, col=c("gray58","darkslategray"),
       bty = "n", cex = 1)


# ----------------- #


# NMDS just based upon species composition - no abundance
ants <- ant %>% group_by(Sampling.Site, Timeframe, Current.Species) %>%
  summarise(n.obs=n())
ants$n.obs <- 1
as.numeric(ants$n.obs)
slope <- spread(key = Current.Species, value = n.obs, fill = 0, data=ants)
slope$Timeframe <- NULL
slope$Sampling.Site <- NULL
slope

# plot
set.seed(2) # setting the starting point of a random number generator
nmds1 <- metaMDS(slope, k=2, trymax=500)
stressplot(nmds1)
plot(nmds1)

ordiplot(nmds1,type="n")
orditorp(nmds1,display="species",col="red",air=0.01)
orditorp(nmds1,display="sites",pch = 16,cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 27)
ordiplot(nmds1,type="n")
ordihull(nmds1,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds1,display="species",col="red",air=0.01)

colors=rep(c("darkcyan","goldenrod"), 27)
ordiplot(nmds1,type="n")
# Plot convex hulls with colors based on treatment
for(i in unique(treat)) {
  ordihull(nmds1$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds1,display="sites", label = F, cex=1,air=0.01)
legend("topright", legend=c("Contemporary", "Historical"), pch = 19, col=c("darkcyan","goldenrod"),
       bty = "n", cex = 1)

# --------------------------------------------------------------------------- # SPECIES RICHNESS COMPARISON BETWEEN 
                                                                              # CURRENT AND HISTORICAL SOUTH SLOPE 
                                                                              # SAMPLES
# creating subset for historical south slope samples
hist.ss <- subset(hist, hist$Slope=="South Slope")
# creating wide data for historical south slope samples
ss.hmat <- acast(hist.ss, Sampling.Site ~ Current.Species, 
                 value.var= "Occurrence", fun.aggregate = sum)
ss.hmat

# creating subset for current south slope samples
cur.ss <- subset(cur, cur$Slope=="South Slope")
# creating wide data for current south slope samples
ss.cmat <- acast(cur.ss, Sampling.Site ~ Current.Species, 
                 value.var= "Occurrence", fun.aggregate = sum)
ss.cmat

str(ss.hmat)
str(ss.cmat)

# visualize difference in site level species richness
boxplot(rowSums(ss.hmat), rowSums(ss.cmat))

# ttest to compare species richness
t.test(rowSums(ss.hmat), rowSums(ss.cmat), paired = T)

# calculate observed and estimated species richness
sphist.ss <- specpool(ss.hmat) 
spcur.ss <- specpool(ss.cmat)

# inspect
sphist.ss
spcur.ss

# species accumulation curves
sahist.ss <- specaccum(ss.hmat)
sacur.ss <- specaccum(ss.cmat)

# plot accumulation curve with 95% confidence intervals 
plot(sahist.ss, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="pink",
     ylim=c(0,35),xlim=c(0,11),xlab="site sampled",ylab="ant species richness")

plot(sacur.ss, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",
     ylim=c(0,35),xlim=c(0,11),xlab="site",ylab="ant species richness", add=T)

# NMDS PLOT BETWEEN SOUTH SLOPE CURRENT AND HISTORIC
# creating a matrix that is only species counts for current and historical data
ant.ss <- subset(ant, ant$Slope=="South Slope")
antss <- ant.ss %>% group_by(Sampling.Site, Timeframe, Current.Species) %>%
  summarise(n.obs=n())
ss.ant <- spread(key = Current.Species, value = n.obs, fill = 0, data=antss)
ss.ant

# remove the first and second rows that specify current vs historical and slope
ss.ant$Timeframe <- NULL
ss.ant$Sampling.Site <- NULL
ss.ant

# need to standardize abundance - divide each observation by the total
ss.ant <- ss.ant/rowSums(ss.ant)
ss.ant

# ----------------- #

ant.ss <- subset(ant, ant$Slope=="South Slope")
antss <- ant.ss %>% group_by(Sampling.Site, Timeframe, Current.Species) %>%
  summarise(n.obs=n())
antss$n.obs <- 1
as.numeric(antss$n.obs)
ss.ant <- spread(key = Current.Species, value = n.obs, fill = 0, data=antss)
ss.ant

# remove the first and second rows that specify current vs historical and slope
ss.ant$Timeframe <- NULL
ss.ant$Sampling.Site <- NULL
ss.ant

# plot
set.seed(2) # setting the starting point of a random number generator
nmds2 <- metaMDS(ss.ant, k=2, trymax=500)
stressplot(nmds2)
summary(nmds2)
plot(nmds2)

ordiplot(nmds2,type="n")
orditorp(nmds2,display="species",col="red",air=0.01)
orditorp(nmds2,display="sites",cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 13)
ordiplot(nmds2,type="n")
ordihull(nmds2,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds2,display="species",col="red",air=0.01)

colors=rep(c("darkcyan","goldenrod"), 13)
ordiplot(nmds2,type="n")
# Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds2$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds2,display="sites",label = F, cex=1,air=0.01)


# --------------------------------------------------------------------------- # SPECIES RICHNESS COMPARISON BETWEEN 
                                                                              # CURRENT AND HISTORICAL NORTH SLOPE 
                                                                              # SAMPLES
# creating subset for historical south slope samples
hist.ns <- subset(hist, hist$Slope=="North Slope")
# creating wide data for historical south slope samples
ns.hmat <- acast(hist.ns, Sampling.Site ~ Current.Species, 
                 value.var= "Occurrence", fun.aggregate = sum)
ns.hmat

# creating subset for current north slope samples
cur.ns <- subset(cur, cur$Slope=="North Slope")
# creating wide data for current north slope samples
ns.cmat <- acast(cur.ns, Sampling.Site ~ Current.Species, 
                 value.var= "Occurrence", fun.aggregate = sum)
# ns.cmat = north slope current matrix
ns.cmat

str(ns.hmat)
str(ns.cmat)

# visualize difference in site level species richness
boxplot(rowSums(ns.hmat), rowSums(ns.cmat))

# ttest to compare species richness
t.test(rowSums(ns.hmat), rowSums(ns.cmat), paired = T)

# calculate observed and estimated species richness
sphist.ns <- specpool(ns.hmat) 
spcur.ns <- specpool(ns.cmat)

# inspect
sphist.ns
spcur.ns

# species accumulation curves
sahist.ns <- specaccum(ns.hmat)
sacur.ns <- specaccum(ns.cmat)

# plot accumulation curve with 95% confidence intervals 
plot(sahist.ns, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="pink",
     ylim=c(0,55),xlim=c(0,15),xlab="site sampled",ylab="ant species richness")

plot(sacur.ns, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",
     ylim=c(0,55),xlim=c(0,15),xlab="site",ylab="ant species richness", add=T)

# NMDS PLOT BETWEEN NORTH SLOPE CURRENT AND HISTORIC
# creating a matrix that is only species counts for current and historical data
ant.ns <- subset(ant, ant$Slope=="North Slope")
antns <- ant.ns %>% group_by(Sampling.Site, Timeframe, Current.Species) %>%
  summarise(n.obs=n())
ns.ant <- spread(key = Current.Species, value = n.obs, fill = 0, data=antns)
ns.ant

# remove the first and second rows that specify current vs historical and slope
ns.ant$Timeframe <- NULL
ns.ant$Sampling.Site <- NULL
ns.ant

# need to standardize abundance - divide each observation by the total
ns.ant <- ns.ant/rowSums(ns.ant)
ns.ant

# plot
set.seed(2) # setting the starting point of a random number generator
nmds3 <- metaMDS(ns.ant, k=2, trymax=500)
stressplot(nmds3)
plot(nmds3)

ordiplot(nmds3,type="n")
orditorp(nmds3,display="species",col="red",air=0.01)
orditorp(nmds3,display="sites", cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 12)
ordiplot(nmds3,type="n")
ordihull(nmds3,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds3,display="species",col="red",air=0.01)

colors=rep(c("gray58","darkslategray"), 27)
ordiplot(nmds3,type="n")
# Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds3$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds3,display="sites",label = F, cex=1,air=0.01)
legend("topright", legend=c("Contemporary", "Historical"), pch = 19, col=c("gray58","darkslategray"),
       bty = "n", cex = 1)

# -------- #
ant.ns <- subset(ant, ant$Slope=="North Slope")
antns <- ant.ns %>% group_by(Sampling.Site, Timeframe, Current.Species) %>%
  summarise(n.obs=n())
antns$n.obs <- 1
as.numeric(antns$n.obs)
ns.ant <- spread(key = Current.Species, value = n.obs, fill = 0, data=antns)
ns.ant

# remove the first and second rows that specify current vs historical and slope
ns.ant$Timeframe <- NULL
ns.ant$Sampling.Site <- NULL
ns.ant
set.seed(2) # setting the starting point of a random number generator
nmds3 <- metaMDS(ns.ant, k=2, trymax=500)
stressplot(nmds3)
plot(nmds3)

ordiplot(nmds3,type="n")
orditorp(nmds3,display="species",col="red",air=0.01)
orditorp(nmds3,display="sites",cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 14)
ordiplot(nmds3,type="n")
ordihull(nmds3,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds3,display="species",col="red",air=0.01)

colors=rep(c("darkcyan","goldenrod"), 14)
ordiplot(nmds3,type="n")
# Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds3$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds3,display="sites",label = F, cex=1,air=0.01)
legend("topright", legend=c("Contemporary", "Historical"), pch = 19, col=c("darkcyan","goldenrod"),
       bty = "n", cex = 1)
# --------------------------------------------------------------------------- # SPECIES RICHNESS COMPARISON BETWEEN 
                                                                              # CURRENT AND HISTORICAL CANYON 
                                                                              # BOTTOM SAMPLES
# creating subset for historical canyon bottom samples
hist.cb <- subset(hist, hist$Slope=="Canyon Bottom")
# creating wide data for historical canyon bottom samples
cb.hmat <- acast(hist.cb, Sampling.Site ~ Current.Species, 
                 value.var= "Occurrence", fun.aggregate = sum)
# cb.hmat = canyon bottom historic matrix
cb.hmat

# creating subset for current canyon bottom samples
cur.cb <- subset(cur, cur$Slope=="Canyon Bottom")
# creating wide data for current canyon bottom samples
cb.cmat <- acast(cur.cb, Sampling.Site ~ Current.Species, 
                 value.var= "Occurrence", fun.aggregate = sum)
# cb.cmat = canyon bottom current matrix
cb.cmat

str(cb.hmat)
str(cb.cmat)

# visualize difference in site level species richness
boxplot(rowSums(cb.hmat), rowSums(cb.cmat))

# ttest to compare species richness
t.test(rowSums(cb.hmat), rowSums(cb.cmat), paired = T)

# calculate observed and estimated species richness
sphist.cb <- specpool(cb.hmat) 
spcur.cb <- specpool(cb.cmat)

# inspect
sphist.cb
spcur.cb

# species accumulation curves
sahist.cb <- specaccum(cb.hmat)
sacur.cb <- specaccum(cb.cmat)

# plot accumulation curve with 95% confidence intervals 
plot(sahist.cb, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="pink",
     ylim=c(0,35),xlim=c(0,7),xlab="site sampled",ylab="ant species richness")

plot(sacur.cb, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",
     ylim=c(0,35),xlim=c(0,7),xlab="site",ylab="ant species richness", add=T)


# NMDS PLOT BETWEEN CANYON BOTTOM CURRENT AND HISTORIC
# creating a matrix that is only species counts for current and historical data
ant.cb <- subset(ant, ant$Slope=="Canyon Bottom")
antcb <- ant.cb %>% group_by(Sampling.Site, Timeframe, 
                             Current.Species) %>%
  summarise(n.obs=n())
cb.ant <- spread(key = Current.Species, value = n.obs, fill = 0, data=antcb)
cb.ant

# remove the first and second rows that specify current vs historical and slope
cb.ant$Timeframe <- NULL
cb.ant$Sampling.Site <- NULL
cb.ant

# need to standardize abundance - divide each observation by the total
cb.ant <- cb.ant/rowSums(cb.ant)
cb.ant

# plot
set.seed(2) # setting the starting point of a random number generator
nmds4 <- metaMDS(cb.ant, k=2, trymax=500)
stressplot(nmds4)
plot(nmds4)

ordiplot(nmds4,type="n")
orditorp(nmds4,display="species",col="red",air=0.01)
orditorp(nmds4,display="sites",cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 5)
ordiplot(nmds4,type="n")
ordihull(nmds4,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds4,display="species",col="red",air=0.01)

colors=rep(c("gray58","darkslategray"), 27)
ordiplot(nmds4,type="n")
# Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds4$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds4,display="sites",cex=1,air=0.01)
legend("topright", legend=c("Contemporary", "Historical"), pch = 19, col=c("gray58","darkslategray"),
       bty = "n", cex = 1)
#-----------#

ant.cb <- subset(ant, ant$Slope=="Canyon Bottom")
antcb <- ant.cb %>% group_by(Sampling.Site, Timeframe, 
                             Current.Species) %>% summarise(n.obs=n())
antcb$n.obs <- 1
as.numeric(antcb$n.obs)
cb.ant <- spread(key = Current.Species, value = n.obs, fill = 0, data=antcb)
cb.ant

# remove the first and second rows that specify current vs historical and slope
cb.ant$Timeframe <- NULL
cb.ant$Sampling.Site <- NULL
cb.ant
# plot
set.seed(2) # setting the starting point of a random number generator
nmds4 <- metaMDS(cb.ant, k=2, trymax=500)
stressplot(nmds4)
plot(nmds4)

ordiplot(nmds4,type="n")
orditorp(nmds4,display="species",col="red",air=0.01)
orditorp(nmds4,display="sites",cex=1.25,air=0.01)

vec <- c("Contemporary", "Historical")
treat=rep(vec, 6)
ordiplot(nmds4,type="n")
ordihull(nmds4,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds4,display="species",col="red",air=0.01)

colors=rep(c("darkcyan","goldenrod"), 6)
ordiplot(nmds4,type="n")
# Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds4$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds4,display="sites",label = F, cex=1,air=0.01)


#-----------------------------------------------------------------------------# PLOTTING SPECIES RICHNESS BETWEEN
                                                                              # HISTORIC AND CURRENT DATA
# plotting abundances (historical)
# creating wide data from historic and current data subsets
h.mat <- hist %>% group_by(Current.Species) %>%
  summarise(n.obs=n())
h.mat

# need to standardize abundance - divide each observation by the total
h.mat$abun <- NA
col.h <- sum(h.mat$n.obs)
h.mat$abun <- h.mat$n.obs/col.h
h.mat

# Increase margin size
par(mar=c(13,4,4,4))
# order from highest observance to lowest
hmat <- h.mat[order(-h.mat$n.obs),]
barplot(hmat$n.obs, las=2,
        main = "Ant Species",
        ylab = "Observed Occurrence", 
        names.arg = hmat$Current.Species)

# Increase margin size
par(mar=c(13,4,4,4))
# order from highest observance to lowest
hmat <- h.mat[order(-h.mat$abun),]
barplot(hmat$abun, las=2,
        main = "Ant Species",
        ylab = "Observed Occurrence", 
        names.arg = hmat$Current.Species)

# Increase margin size
par(mar=c(13,4,4,4))
# order from highest observance to lowest
cmat <- c.mat[order(-c.mat$n.obs),]
barplot(cmat$n.obs, las=2,
        main = "Ant Species",
        ylab = "Observed Occurrence", 
        names.arg = cmat$Current.Species)

# Increase margin size
par(mar=c(13,4,4,4))
# order from highest observance to lowest
cmat <- c.mat[order(-c.mat$abun),]
barplot(cmat$abun, las=2,
        main = "Ant Species",
        ylab = "Observed Occurrence", 
        names.arg = cmat$Current.Species)


# need to standardize abundance - divide each observation by the total
h.mat$abun <- NA
col.h <- sum(h.mat$n.obs)
h.mat$abun <- h.mat$n.obs/col.h
h.mat$Time <- "H"

# plotting abundances (current)
# creating wide data from historic and current data subsets
c.mat <- cur %>% group_by(Current.Species) %>%
  summarise(n.obs=n())
c.mat

# need to standardize abundance - divide each observation by the total
c.mat$abun <- NA
col.c <- sum(c.mat$n.obs)
c.mat$abun <- c.mat$n.obs/col.c
c.mat$Time <- "C"

total <- rbind(h.mat, c.mat)
total

# Stacked barplot with multiple groups
ggplot(data=total, aes(x=reorder(Current.Species, abun), y=abun, fill=Time)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("gray58", "darkslategray"), name="Timeframe") +
  labs(x = "Ant Species",
       y = "Percent Abundance") + coord_flip()

#-----------------------------------------------------------------------------# ANT SPECIES OCCURRENCE BY SITE

occ <- cur %>% group_by(Current.Species, Sampling.Site) %>%
  summarise(n.obs=n())
occ$n.obs <- 1
as.numeric(occ$n.obs)
occr <- occ %>% group_by(Current.Species) %>% summarise(n.obs=n())
occr$Timeframe <- "Contemporary"

och <- hist %>% group_by(Current.Species, Sampling.Site) %>%
  summarise(n.obs=n())
och$n.obs <- 1
as.numeric(och$n.obs)
occh <- och %>% group_by(Current.Species) %>% summarise(n.obs=n())
occh$Timeframe <- "Historical"

spoc <- rbind(occr, occh)
spoc

# Stacked barplot with multiple groups
ggplot(data=spoc, aes(x=reorder(Current.Species, n.obs), y=n.obs, fill=Timeframe)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("gray58", "darkslategray"), name="Timeframe") +
  labs(x = "Ant Species",
       y = "Site occurrence") + coord_flip()

# --------------------------------------------------------------------------- # PLOTTING THE CHANGE IN SPECIES 
                                                                              # RICHNESS BETWEEN CURRENT AND 
                                                                              # HISTORICAL DATA BASED ON SITES
# creating wide data from historic and current data subsets
hist.mat <- acast(hist, Sampling.Site ~ Current.Species, value.var= "Occurrence", 
                  fun.aggregate = sum)
cur.mat <- acast(cur, Sampling.Site ~ Current.Species, value.var= "Occurrence", 
                 fun.aggregate = sum)

test <- specnumber(hist.mat)
test

curtest <- specnumber(cur.mat)
treat=c(rep("Treatment1",5),rep("Treatment2",5))

abun <- data.frame(site=rep(c(1:20, 27:33), 2),
                   time=c(rep("Historical", 27), rep("Contemporary", 27)),
                   abund=c(test, curtest))
abun

abun$time <- factor(abun$time, levels = c("Historical", "Contemporary"))
ggplot(abun, aes(x = time, y = abund)) +
  geom_point(color = "darkgray", size = 1) + 
  geom_line(aes(group = site), size = 0.50, color = "darkgray") +
  labs(x = " ",
       y = "Species Richness")

# --------------------------------------------------------------------------- #
# observations with more than 10 occurrences

tants <- cur[cur$Current.Species %in% c("Aphaenogaster occidentalis", 
                                        "Brachymyrmex depilis", "Camponotus laevigatus", 
                                        "Camponotus modoc", "Dolichoderus plagiatus",
                                        "Camponotus vicinus", "Formica fusca", 
                                        "Forelius pruinosus",
                                        "Formica neorufibarbis", "Formica argentea",
                                        "Formica podzolica", "Lasius flavus", 
                                        "Lasius niger", 
                                        "Lasius americanus", "Lasius pallitarsis", 
                                        "Liometopum apiculatum", "Lasius umbratus",
                                        "Leptothorax crassipilis", 
                                        "Liometopum luctuosum", 
                                        "Myrmica fracticornis", "Solenopsis molesta",
                                        "Myrmica monticola", "Pheidole ceres", 
                                        "Temnothorax andrei",
                                        "Stenamma diecki", "Tapinoma sessile", 
                                        "Temnothorax nitens", "Tetramorium immigrans",
                                        "Temnothorax rugatulus"), ]

tants <- cur[cur$Current.Species %in% c("Brachymyrmex depilis",
                                        "Forelius pruinosus",
                                        "Liometopum luctuosum",
                                        "Stenamma diecki"), ]


# box plot of collected surface temperatures
ggplot(tants, aes(x=reorder(Current.Species, Surface.Temp.), y=Surface.Temp.)) +
  geom_boxplot(fill="gray58", color="black") +
  geom_hline(yintercept = 8.0, color = "blue", linetype="dashed") +
  geom_hline(yintercept = 40.0, color = "blue", linetype="dashed") +
  geom_hline(yintercept = 12.5, color = "red", linetype="dashed") +
  geom_hline(yintercept = 45.5, color = "red", linetype="dashed") +
  theme(axis.text = element_text(angle = 90)) +
  labs(x = "Species", y = "Surface Temperature (C)", 
       title = "Surface Temperature range for ant species")

# box plot of collected air temperatures
ggplot(tants, aes(x=reorder(Current.Species, Air.Temp.), y=Air.Temp.)) +
  geom_boxplot(fill="gray58", color="black") +
  geom_hline(yintercept = 12.0, color = "blue", linetype="dashed") +
  geom_hline(yintercept = 38.0, color = "blue", linetype="dashed") +
  geom_hline(yintercept = 16.0, color = "red", linetype="dashed") +
  geom_hline(yintercept = 40.0, color = "red", linetype="dashed") +
  theme(axis.text = element_text(angle = 90)) +
  labs(x = "Species", y = "Surface Temperature (C)", 
       title = "Air Temperature range for ant species")


# --------------------------------------------------------------------------- #

# site occurrence for species present in both studies
bants <- ant[ant$Current.Species %in% c("Aphaenogaster occidentalis", 
                                        "Brachymyrmex depilis", "Camponotus laevigatus", 
                                        "Camponotus modoc", "Camponotus vicinus", "Formica fusca", 
                                        "Forelius pruinosus","Formica neorufibarbis", 
                                        "Lasius americanus", "Lasius pallitarsis", 
                                        "Liometopum apiculatum", "Camponotus novaeboracensis",
                                        "Leptothorax crassipilis", "Lasius neoniger",
                                        "Liometopum luctuosum", "Pheidole pilifera", 
                                        "Myrmica fracticornis", "Solenopsis molesta",
                                        "Myrmica monticola", "Pheidole ceres", 
                                        "Stenamma diecki", "Tapinoma sessile", 
                                        "Temnothorax nitens", "Temnothorax rugatulus",
                                        "Solenopsis validiuscula", "Formica obscuriventris",
                                        "Formica ravida", "Camponotus novaeboracensis", "Myrmica latifrons"), ]

# getting subsets of ant data - historic vs current
hbant <- subset(bants, bants$Timeframe=="Historical")
cbant <- subset(bants, bants$Timeframe=="Contemporary")

hbant <- hbant %>% group_by(Current.Species, Sampling.Site) %>%
  summarise(n.obs=n())
hbant
hbant$n.obs <- 1
hbant
as.numeric(hbant$n.obs)
hbant <- hbant %>% group_by(Current.Species) %>% summarise(n.obs=n())
hbant$Timeframe <- "Historical"
hbant

cbant <- cbant %>% group_by(Current.Species, Sampling.Site) %>%
  summarise(n.obs=n())
cbant
cbant$n.obs <- 1
cbant
as.numeric(cbant$n.obs)
cbant <- cbant %>% group_by(Current.Species) %>% summarise(n.obs=n())
cbant$Timeframe <- "Contemporary"
cbant

bant <- rbind(hbant, cbant)
bant

# Stacked barplot with multiple groups
ggplot(data=bant, aes(x=reorder(Current.Species, n.obs), y=n.obs, fill=Timeframe)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("darkcyan", "goldenrod"), name="Timeframe") +
  theme(axis.title = element_text(size = 14)) +
  labs(x = "Ant Species",
       y = "Site occurrence") + coord_flip()


# site occurrence for species present in just one of the studies
sants <- ant[ant$Current.Species %in% c("Dolichoderus plagiatus","Formica argentea",
                                        "Formica podzolica",  "Lasius flavus", "Lasius niger", 
                                        "Lasius umbratus", "Temnothorax andrei","Tetramorium immigrans",
                                        "Formica hewitti", "Formica neoclara", "Formica planipilis",
                                        "Myrmica incompleta", "Pogonomyrmex occidentalis",
                                        "Polyergus breviceps", "Polyergus lucidus", "Temnothorax andrei",
                                        "Prenolepis imparis", "Camponotus herculeanus",
                                        "Camponotus nearcticus", "Lasius interjectus",
                                        "Lasius latipes", "Myrmica americana", "Temnothorax neomexicanus",
                                        "Temnothorax nevadensis", "Formica densiventris",
                                        "Formica lasioides", "Formica neogagates", "Leptothorax muscorum",
                                        "Monomorium minimum", "Temnothorax tricarinatus",
                                        "Lasius brevicornis", "Temnothorax furunculus",
                                        "Myrmica nearctica", "Formica aserva", "Lasius humilis",
                                        "Crematogaster emeryana", "Myrmica lobifrons",
                                        "Lasius subumbratus", "Formica pallidefulva"), ]
sant <- sants %>% group_by(Current.Species, Sampling.Site, Timeframe) %>%
  summarise(n.obs=n())
sant
sant$n.obs <- 1
sant
as.numeric(sant$n.obs)
sant <- sant %>% group_by(Current.Species, Timeframe) %>% summarise(n.obs=n())
sant


# Stacked barplot with multiple groups
ggplot(data=sant, aes(x=reorder(Current.Species, as.numeric(n.obs)), y=as.numeric(n.obs), fill=Timeframe)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("darkcyan", "goldenrod"), name="Timeframe") +
  theme(axis.title = element_text(size = 14)) +
  scale_y_continuous(limits = c(0,33), breaks=c(0, 10, 20)) +
  labs(x = "Ant Species",
       y = "Site occurrence") + coord_flip()


# --------- THE ABOVE CODE IS GOOD, BELOW IS A WORK IN PROGRESS ------------- #

# --------------------------------------------------------------------------- #

# ANALYZE DIFFERENCES IN ANT SPECIES COMPOSITION 
# presence absence data for ants
ants <- ant %>% group_by(Sampling.Site, Timeframe, 
                         Current.Species) %>%
  summarise(n.obs=n())
ants
slope <- spread(key = Current.Species, value = n.obs, fill = 0, data=ants)

# odds are current data
# evens are historic data
slope$Timeframe <- NULL
slope$Sampling.Site <- NULL
slope

# need to standardize abundance - divide each observation by the total
slope <- slope/rowSums(slope)
slope

# Does variation in ant species composition differ between historic/current
beta <- vegdist(slope, method = "bray", binary = FALSE)
hist(beta)
summary(beta)
disp <- betadisper(beta, group = ants$Timeframe)
permutest(disp)
plot(disp, main = "Ant composition")


beta <- vegdist(slope, method = "bray", binary = TRUE)
hist(beta)
summary(beta)
disp <- betadisper(beta, group = antz$Current.or.Historical)
permutest(disp)
plot(disp, main = "Ant composition")
beta


# correlate the disimilarity with temperature differences
# mantel test
# partial mantel test - 2/3 things
# generalized dismiliarity modeliing
# paper with Rob Fletcher - 2021 Landscape Ecology
# similarity of ants as a function of distance

# --------------------------------------------------------------------------- #


# ANT SPECIES RICHNESS BY SITE
h.mat <- hist %>% group_by(Sampling.Site, 
                           Current.Species) %>%
  summarise(n.obs=n())
h.mat <- spread(key = Current.Species, value = n.obs, fill = 0, data=h.mat)
h.mat

pool <- specpool(h.mat, h.mat$Sampling.Site)
pool



c.mat <- cur %>% group_by(Sampling.Site, 
                          Current.Species) %>%
  summarise(n.obs=n())
c.mat <- spread(key = Current.Species, value = n.obs, fill = 0, data=c.mat)
c.mat

pool <- specpool(c.mat, c.mat$Sampling.Site)
pool

# --------------------------------------------------------------------------- #
# PCA
rich <- read.csv("species richness.csv")
rich
sp.pca <- prcomp(rich[,c(2:5)], center = TRUE,scale. = TRUE)
summary(sp.pca)
ggbiplot(sp.pca)

# ------ #

# Environmental matrix: Keep first 2 components only
new.data <- as.data.frame(sp.pca$x[ , 1:2]) 
colnames(new.data) <- c("PC1", "PC2")

# Add spatial coordinates for the sites
new.data$sprich <- rich$Species.richness
new.data$Date <- rich$Date
new.data$Site <- rich$Sampling.Site
new.data$Slope <- rich$Slope

# Model relationships between PC1 of environment and plant richness
antfit <- lmer(sprich ~ PC1 * Date + (1 | Slope), data = new.data)
summary(antfit)

plot(antfit)


# Plot interaction: how does the effect of plant richness on ant richness vary with the environment
interplot(m = antfit, var1 = "sprich" , var2 = "PC1")

m_cyl <- lm(sprich ~ PC1 * Date, data = new.data)
summary(m_cyl)
interplot(m = m_cyl, var1 = "PC1" , var2 = "sprich")

# ------------------------------------------------------------------------------------------------------ #

css <- cur.ss %>% group_by(Current.Species) %>% summarise(n.obs=n())
cns <- cur.ns %>% group_by(Current.Species) %>% summarise(n.obs=n())
ccb <- cur.cb %>% group_by(Current.Species) %>% summarise(n.obs=n())



