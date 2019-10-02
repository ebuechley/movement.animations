##########################
#MoveVis
##########################
setwd("~/Documents/GitHub/movement.animations/")
rm(list = ls())

#install and load packages
library(moveVis)
library(move)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
require(maps)

#load data
d = read.csv("Pan-Africa Vulture Tracking.csv", header=T, na.strings=c("","NA"))
d1 = read.csv("Hooded Vulture Africa.csv", header=T, na.strings=c("","NA"))
d = rbind.fill(d, d1)
head(d)
names(d)
summary(d)

#remove any rows that don't have date, lat or long
summary(d[3:5])
summary(d[34])
d = d[complete.cases(d[,3:5]),] 
d = d[complete.cases(d[,34]),] 
d = d[!is.na(d$individual.local.identifier), ]

#rename columns
d$species = d$individual.taxon.canonical.name
d$lat = d$location.lat
d$long = d$location.long
d$id = d$individual.local.identifier

#limit data to Ethiopia
d<-d[!(d$long<=0 & d$lat<=0),]
d<-d[!(d$lat<=0),]
summary(d)
      
#remove EV
d<-d[!(d$species=="Neophron percnopterus"),]

#quick plot of dataset
tiff("Ethiopia.vulture.tracking.overview.tiff", units="cm", width=25, height=15, res=300)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() + ggtitle("Ethiopia Vulture Tracking")
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id, color = species), alpha = .5)  + xlim(32, 48) + ylim(3, 16)
map.plot
dev.off()

#subset by species
hv = subset(d, species == "Necrosyrtes monachus")
ev = subset(d, species == "Neophron percnopterus")
wbv = subset(d, species == "Gyps africanus")
rv = subset(d, species == "Gyps ruppellii")
lfv = subset(d, species == "Torgos tracheliotus")
