#Set WD
setwd("~/Documents/GitHub/movement.animations/")

###Load relevant libraries###
##install MigrateR
#install_github("dbspitz/migrateR/migrateR", build_vignettes = T)
library(devtools)
library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)
library(maps)
library(lubridate)
library(plyr)
library(data.table)
library(amt)
library(argosfilter)
library(ggplot2)
require(sp)
require(maps)
require(stringr)
require(reshape2)
require(ggthemes)
require(pander)
library(plyr)
library(lubridate)
##install MigrateR
#install.packages("devtools")
library(devtools)
#install_github("dbspitz/migrateR/migrateR", build_vignettes = T)
library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)
library(maps)
library(lubridate)
library(plyr)
library(data.table)
library(amt)
library(argosfilter)
library(ggplot2)
require(sp)
require(maps)
require(stringr)
require(reshape2)
require(ggthemes)
require(pander)
library(trip)

##Clear workspace
rm(list = ls())

########################################################
#EV
########################################################
#read data
d = read.csv("Pan-Africa Vulture Tracking.csv")
d1 = read.csv("Hooded Vulture Africa.csv")
d2 = read.csv("Egyptian Vulture in the Middle East and East Africa.csv")

#merge (vertically) the data, keeping all unique columns
d3 = rbind.fill(d,d1,d2)
names(d3)
summary(d3$timestamp)
d3$timestamp = ymd_hms(d3$timestamp)
unique(d3$individual.local.identifier) #note 64 unique id

#rename/simplify column headers
colnames(d3)[colnames(d3)=="location.long"] <- "long"
colnames(d3)[colnames(d3)=="location.lat"] <- "lat"
colnames(d3)[colnames(d3)=="tag.local.identifier"] <- "tag"
colnames(d3)[colnames(d3)=="individual.local.identifier"] <- "id"
colnames(d3)[colnames(d3)=="individual.taxon.canonical.name"] <- "species"

###Create burst by ID and tag
d3$id.tag <- c(paste(d3$id,d3$tag,sep="_")) 
d3$id.tag <- as.factor(d3$id.tag) 

#lubridate
d3$timestamp = ymd_hms(d3$timestamp)

#remove any rows that don't have date, lat or long
names(d3)
d3 = d3[complete.cases(d3[,3:5]),] 

#reorder dataframe to have x,y,date,tag as first four columns
names(d3)
d3 = d3[,c(4,5,3,47,1:2,6:46)]
names(d3)

#add ymdh
d3$year <- year(d3$timestamp)
d3$month <- month(d3$timestamp)
d3$day = day(d3$timestamp)
d3$hour <- hour(d3$timestamp)


#censor to one point per day 
#d4 = d3[!duplicated(d3[,c('id', 'year', 'month', 'day')]),]

#quick plot of data
library(ggplot2)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() +
  theme_bw() + geom_path(data = d3, aes(long,lat, group = id)) +
  labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
map.plot #notice bad fixes in dataset

#write 1ptperday dataset
#write.csv(d4, "1ptperday.csv", row.names=FALSE)

################################################################
# cleaning data
################################################################

#filter data to remove bad fixes
d = d3
unique(d$id.tag)
unique(d$study.name)
unique(d$species)
summary(d$timestamp)

#manually filter bad fixes
d<-d[!(d$long==0 & d$lat==0),]
d<-d[!(d$long>=75),]
d<-d[!(d$lat<=-50),]

#remove height above elipsoid < -100 and > 30,000
summary(d$height.above.ellipsoid)
which(d$height.above.ellipsoid < -100)
which(d$height.above.ellipsoid > 30000)
d = d[-c(which(d$height.above.ellipsoid < -100)),]
d = d[-c(which(d$height.above.ellipsoid > 30000)),]
summary(d$height.above.ellipsoid)

#remove any rows that don't have date, lat or long
summary(d[1:4])
#d = d[complete.cases(d[,1:4]),] 

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
 geom_path(data = d, aes(long,lat, group = id)) + labs(x = "longitude", y = "latitude") +
 theme(legend.title = element_blank()) 
map.plot #notice bad fixes in dataset

#speed filter from 'trip' package
#convert to 'trip'
tr = trip(d)

#plot trip
#plot(tr)
#lines(tr)
#maps::map("world", add = TRUE)

#run a speed filter and add a column to the data, max speed in km/hr
#?speedfilter
#?sda
tr$spd = speedfilter(tr, max.speed = 50)

#what % are not filtered out? (not clear how this works...)
mean(tr$spd)
summary(tr$spd)

#plot with censored
plot(tr)
plot(tr[tr$spd,], col = 'green', add = T)
#lines(tr[tr$spd,])
#maps::map("world", add = TRUE)
#axis(1)
#axis(2)

#convert to spdf for plotting
b = as(tr, "SpatialPointsDataFrame")
b2 = subset(b, b$spd == "TRUE")
plot(b2)
b1 = subset(b, b$spd == "FALSE")
plot(b1, color = "red", add = T)

#save as df
d.filtered = as.data.frame(b2)
head(d.filtered)

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + 
  coord_quickmap() + theme_bw() + geom_path(data = d.filtered, aes(long,lat, group = id.tag), alpha = .5) +
 labs(x = "longitude", y = "latitude") + theme(legend.title = element_blank()) 
map.plot

#write
write.csv(d.filtered, "pan.africa.filtered.csv", row.names=FALSE)

#####################################################
#compute movement stats in adeHabitatLT
#####################################################
d = read.csv("pan.africa.filtered.csv")
head(d)
names(d)
unique(d$population)
unique(d$id.tag) #note 64 unique id.tag

#lubridate
summary(d$timestamp)
d$timestamp = ymd_hms(d$timestamp)

#convert to ltraj
#use UTM so that distance values are calculated in meters
b = as.ltraj(xy = d[, c("long", "lat")], date = d$timestamp, id = d$id.tag)
b

#index
#dx, dy, dt == describe the distance of the x and y directions and duration in time between the relocations i and i + 1
#dist == the distance between successive relocations
#R2n == the squared distance between the first relocation of the trajectory and the current relocation (net squared displacement)

#plot ltraj
#plot.ltraj(b[4])

#to look at time interval between locations plot 
#dt is measrured in second. to conver to days == dt / 3600 / 24 
#plotltr(b[1], "dt/3600/24")
#dist is measured in meters. convert to km by == / 1000
#plotltr(b[1], which = "dist/1000")
#plot net [squared] displacement
#plotltr(b[1], which = 'sqrt(R2n)')

#save ltraj as dataframe
d1 <- do.call(rbind, b)
head(d1)

#merge ltraj calcs to full dataset
d$NSD <- d1$R2n
d$ND <- sqrt(d$NSD)
d$dist <- d1$dist
d$dt.days <- d1$dt/3600/24
head(d)
names(d)

#write
write.csv(d, "pan.africa.filtered.csv", row.names = FALSE)

########################################
#data summary
########################################
#convert to data.table to summarize
d.dt = setDT(d)

#remove rows that have NA for columns we are summarizing here 
d.dt = d.dt[!is.na(d.dt$dist),]
d.dt = d.dt[!is.na(d.dt$dt.days),]

#check data
head(d.dt)
summary(d.dt)
names(d.dt)

#summarize
d3.summary = d.dt[,.(unique(species), unique(study.name), unique(id),unique(tag), 
                        min(timestamp), max(timestamp), head(lat,1), head(long,1), 
                        tail(lat,1), tail(long,1), mean(tail(dist,10)), mean(tail(dt.days,10))),by = .(id.tag)]

#add headers
names(d3.summary) = c("id.tag", "species", "study.name", "id", "tag",  "start.date", 
                         "end.date", "start.lat", "start.long", "end.lat", "end.long", 
                         "mean.GPS.dist.last10fixes", "mean.GPS.fixrate.last10fixes")
head(d3.summary)
summary(d3.summary)

#add deployment duration
d3.summary$deployment.duration = as.numeric(difftime(d3.summary$end.date, d3.summary$start.date, units = "days"))
head(d3.summary)

#add fate = alive if still transmitting 
d3.summary = cbind(d3.summary, ifelse(d3.summary$end.date > ymd(20191101), "alive", 'NA'))
names(d3.summary)
names(d3.summary)[15] = 'fate'
d3.summary$fate = as.factor(d3.summary$fate)
head(d3.summary)
summary(d3.summary)

#add other columns, and input data from Google Sheet
d3.summary$age.at.deployment = NA

#
d3.summary$sex = NA

#
d3.summary$how.fate.determined = ifelse(d3.summary$fate == "alive", "still transmitting", "NA")
d3.summary$cause.of.death = NA
d3.summary$how.fate.determined = as.factor(d3.summary$how.fate.determined)
d3.summary$death.or.failure.date = NA
summary(d3.summary$how.fate.determined)

#add comments column
d3.summary$comments = NA

#add start / end country
require(rgdal)
world = readOGR(dsn = "./TM_WORLD_BORDERS_SIMPL-0.3/", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#convert summary to spdf
names(d3.summary)
xy.start <- d3.summary[,c(9,8)]
spdf.start <- SpatialPointsDataFrame(coords = xy.start, data = d3.summary,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.start) == proj4string(world)
plot(spdf.start)
plot(world, add = T)

#sample and append country to summary
start.country = data.frame(over(spdf.start, world[,5]))
names(start.country) = c("start.country")
start.country
d3.summary.country = cbind(d3.summary, start.country)
summary(d3.summary.country)

#
names(d3.summary.country)
xy.end = d3.summary.country[,c(11,10)]
spdf.end <- SpatialPointsDataFrame(coords = xy.end, data = d3.summary.country,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.end) == proj4string(world)
plot(spdf.end)
plot(world, add = T)

#sample and append country to summary
end.country = data.frame(over(spdf.end, world[,5]))
names(end.country) = c("end.country")
end.country
d3.summary.country = cbind(d3.summary.country, end.country)
summary(d3.summary.country)
unique(d3.summary.country$id.tag) 
d3.summary = d3.summary.country

#add number of locations column
#d3.summary = as.data.frame(d3.summary)
#summary(d3.summary)
#n.locs = count(d.dt$id.tag)
#n.locs$id.tag = n.locs$x
#n.locs$n.locs = n.locs$freq
#n.locs$freq = NULL
#n.locs$x = NULL
#d3.summary = merge(d3.summary, n.locs, by = "id.tag", all = T)

#write data summary
head(d3.summary)
summary(d3.summary)
names(d3.summary)
d3.summary = d3.summary[,c(3,2,5,4,1,18,17,16,17,6:11,22,23,12,13,18,19,20,21)]
names(d3.summary)

# Order the data frame by study
d3.summary = d3.summary[order(d3.summary$study.name),]

#write 
head(d3.summary)
summary(d3.summary$start.date)
summary(d3.summary$end.date)
write.csv(d3.summary, "pan.africa.summary.csv", row.names = FALSE)

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
  geom_path(data = d, aes(long,lat, group = id.tag), alpha = .5) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
map.plot

#################################################################################
#Individual-based summary plots
#################################################################################
#Clear workspace
rm(list = ls())

#read data
d = read.csv("pan.africa.filtered.csv")
summary(d)
names(d)
summary(d$sensor.type)
summary(d$tag)
d$timestamp = ymd_hms(d$timestamp)
summary(d$ND)
unique(d$id.tag)
names(d)

#
d$species.eng = NA
d$species.eng[d$species == "Gyps africanus"] = "White-backed vulture"
d$species.eng[d$species == "Gyps rueppellii"] = "Ruppell's vulture"
d$species.eng[d$species == "Necrosyrtes monachus"] = "Hooded vulture"
d$species.eng[d$species == "Neophron percnopterus"] = "Egyptian vulture"
d$species.eng[d$species == "Torgos tracheliotus"] = "Lappet-faced vulture"
d$species.eng = as.factor(d$species.eng)
summary(d$species.eng)

#
d$species.id<- c(paste(d$species.eng,d$id,sep=" - ")) 
d$species.id = as.factor(d$species.id)
summary(d$species.id)

#plot net displacement
tiff("./overview.plots/pan.africa.tracking.nd.overview.tiff", units="cm", width=70, height=40, res=300)
plot = ggplot(d, aes(timestamp, ND)) + geom_line() + facet_wrap(~ id.tag) +
  labs(x = "date", y = "net displacement (degrees)") + theme_bw() 
plot 
dev.off()

#plots for each id
# create for loop to produce ggplot2 graphs 
library(gridExtra)

names(d)
summary(d$species)


#plots with data summaries
for (i in unique(d$species.id)) { 
  
  #net displacement
  plot1 <- 
    ggplot(aes(timestamp, ND), data = subset(d, species.id ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "net displacement (degrees)") 
  
  #fix rate
  plot2 <- 
    ggplot(aes(timestamp, dt.days), data = subset(d, species.id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "time between fixes (days)") 
  
  #GPS tracks
  register_google(key = "AIzaSyA7S7kwtGt0Gb7So8qdGGrv83iSiyrimnU")
  bbox1 <- make_bbox(lat = lat, lon = long,  data = subset(d, species.id ==  i))
  bbox_map = get_map(location = bbox1, maptype = "terrain", source = "google", scale = "auto")

  plot3 = ggmap(bbox_map)  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d, species.id == i), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  #distance
  plot4 <- 
    ggplot(aes(timestamp, dist), data = subset(d, species.id ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "distance (degrees)") 
  
  #arrange
  plot5 = grid.arrange(plot3, plot1, plot4, plot2, ncol = 2, nrow = 2, 
                       widths = c(1,1), layout_matrix = rbind(c(1, 2), c(3,4)), top = paste(i), bottom = "Prepared by: Dr. Evan Buechley, Smithsonian Institute & HawkWatch International")
  
  ggsave(filename = sprintf('./overview.plots/%s.png', i), plot = plot5, width = 30, height = 20, units = c("cm"),dpi = 300)
}

#simple map plot
for (i in unique(d$species.id)) { 
  
  bbox1 <- make_bbox(lat = lat, lon = long,  data = subset(d, species.id ==  i))
  bbox_map = get_map(location = bbox1, maptype = "terrain", source = "google", scale = "auto")
  
  plot1 = ggmap(bbox_map)  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d, species.id ==  i), aes(long,lat)) + labs(title = paste(i), x = "longitude", y = "latitude", caption = "Prepared by: Dr. Evan Buechley, HawkWatch International") + 
    theme(plot.title = element_text(hjust = 0.5),  plot.caption = element_text(hjust = 0.5))
  
  ggsave(filename = sprintf('./overview.plots/simple.plots/%s.png', i), plot = plot1, width = 30, height = 20, units = c("cm"),dpi = 300)
}

#Ethiopia map plot
register_google(key = "AIzaSyA7S7kwtGt0Gb7So8qdGGrv83iSiyrimnU")
ETmap = get_map(location = c(40,9), maptype = "terrain", source = "google", zoom = 6)
ggmap(ETmap)

for (i in unique(d$species.id)) { 
  
  plot1 = ggmap(ETmap)  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d, species.id ==  i), aes(long,lat)) + labs(title = paste(i), x = "longitude", y = "latitude", caption = "Prepared by: Dr. Evan Buechley, HawkWatch International") + 
    theme(plot.title = element_text(hjust = 0.5),  plot.caption = element_text(hjust = 0.5))
  
  ggsave(filename = sprintf('./overview.plots/ET.plots/%s.png', i), plot = plot1, width = 30, height = 20, units = c("cm"),dpi = 300)
}


#EV
bbox1 <- make_bbox(lat = lat, lon = long,  data = subset(d, id ==  "KingTut"))
bbox1
bbox_map = get_map(location = c(20,8.5,60,42), maptype = "terrain", source = "google", scale = "auto")
ggmap(bbox_map)

unique(d$species.id)
plot1 = ggmap(bbox_map) + coord_quickmap() + theme_bw() +
  geom_path(data = subset(d, species.id ==  "Egyptian vulture - KingTut"), aes(long,lat)) + labs(title = "King Tut", subtitle = "Egyptian Vulture", x = "longitude", y = "latitude", caption = "Prepared by: Evan Buechley, HawkWatch International") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))
plot1 + scalebar(d, dist = 1000, dist_unit = "km", transform = F, model = "WGS84")

ggsave('./overview.plots/EV.plots/KingTut.png', plot = plot1, width = 30, height = 20, units = c("cm"),dpi = 300)

plot2 = ggmap(bbox_map) + coord_quickmap() + theme_bw() +
  geom_path(data = subset(d, species.id ==  "Egyptian vulture - TomPetty"), aes(long,lat)) + labs(title = "Tom Petty", subtitle = "Egyptian Vulture",x = "longitude", y = "latitude", caption = "Prepared by: Evan Buechley, HawkWatch International") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))
plot2
ggsave('./overview.plots/EV.plots/TomPetty.png', plot = plot2, width = 30, height = 20, units = c("cm"),dpi = 300)