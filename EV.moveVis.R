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
#d = read.csv("ev.tv.filtered.csv")
#d = read.csv("EV.migration.flexibility.csv")
#d = read.csv("EV.migration.flexibility.completmigrationsonly.csv")
d = read.csv("EV-CompletedMigrations-1ptperday.csv")
#d = read.csv("EGVU_Final_complete_migrations_only_1ptperday.csv")
#d = read.csv("EV-all-1ptperday-filtered-utm-NSD-season.csv")
head(d)

#quick plot of dataset
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id), alpha = .5) 
map.plot

#add population variable
unique(d$study)
d$population = NA
head(d)
d$population[d$study == 'grefa-spain'] <- 'western europe'
d$population[d$study == 'efrat-israel'] <- 'middle east'
d$population[d$study == 'oppel-balkans'] <- 'balkans'
d$population[d$study == 'buechley-mideast'] <- 'caucasus'
d$population[d$study == 'kobierzycki-france'] <- 'western europe'
d$population[d$study == 'karyakin-russia'] <- 'caucasus'
d$population[d$study == 'life.rupis-spain'] <- 'western europe'
d$population[d$study == 'terra.natura-spain'] <- 'western europe'
d$population[d$study == 'douro-spain'] <- 'western europe'
d$population[d$study == 'migra-spain'] <- 'western europe'
d$population = as.factor(d$population)
summary(d$population)

#add color variable
#colors
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
barplot(c(5,5,5,5,5,5,5,5,5,5), col=colourpalette)
d$colour[d$population == 'western europe'] <- '#377eb8'
d$colour[d$population == 'caucasus'] <- '#ff7f00'
d$colour[d$population == 'balkans'] <- '#4daf4a'
d$colour[d$population == 'middle east'] <- '#e41a1c'

# Re-store DateTime_GMT as POSIXt object
head(d)
#d$timestamp <- ymd_hms(d$date, tz='GMT')
d$timestamp <- mdy_hm(d$date, tz='GMT')
class(d$timestamp)
summary(d$timestamp)
d$year = year(d$timestamp)
d$month = month(d$timestamp)
d$day = day(d$timestamp)
d$hour = hour(d$timestamp)
summary(d)
d$year.null = 2000
d$ymd = paste(d$year.null, d$month, sep = "-")
d$ymd = paste(d$ymd, d$day, sep = "-")
d$ymd = paste(d$ymd, d$hour, sep = " ")
d$ymd = ymd_h(d$ymd)
head(d)  
class(d$ymd)

# remove id.yr with few points
summary(d$id.yr)
d1 = d %>% group_by(id.yr) %>% filter(n()>=100) %>% ungroup()

#
#unique(d$id)
#ev = subset(d, species == "Neophron percnopterus")
#Logiya = subset(d, id == "Logiya")
ev = d
summary(ev)
names(ev)

# use df2move to convert the data.frame into a moveStack
dm = df2move(ev, proj = "+proj=longlat +datum=WGS84",
             x = 'long', y = 'lat', time = 'ymd', 
             track_id = 'id.yr')

# align move_data to a uniform time scale
move_data <- align_move(dm, res = 1, digit = 0, unit = "days")
#move_data <-  align_move(dm, res = "mean")

# create spatial frames 
get_maptypes()
#extent = extent(-18,52,0,49)
#To use mapbox maps, you need to register for a free mapbox account and get a token key, which can be inserted below
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "osm", map_type = "no_labels",
                         map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         #map_service = "osm", map_type = "terrain",
                         #map_service = "carto", map_type = "light_no_labels",
                         #map_service = "osm", map_type = "terrain",
                         #map_dir = "~/Documents/MapDirectory/",
                         #ext = extent, 
                         equidistant = F,
                         path_size = 1, path_end = "round", path_join = "round", path_fade = T, 
                         #path_colours = c('red', 'green', '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120'),
                         path_colours = NA,
                         tail_length = 5, tail_size = .1, tail_colour = '#ff7f00', trace_show = T, trace_colour = '#ff7f00', 
                         path_legend = FALSE)
length(frames)

#customize frames
frames <- add_labels(frames, title = "Egyptian Vulture Migration", 
caption = "W.L. Phipps, P. López-López, E. Buechley, S. Oppel, et al. (2019)
Spatial and Temporal Variability in Migration of a Soaring Raptor Across Three Continents
Frontiers in Ecology and Evolution") 
frames <- add_scalebar(frames, height = 0.01, distance = 1500, x = -20, y = 0, label_margin = 2, colour = "black") # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
#frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[300]]

# animate frame
suggest_formats()
animate_frames(frames, out_file = "./Outputs/EgyptianVulture_MovementAnimation_MigrationFlexibility_Clean.mp4", overwrite = TRUE,
               fps = 5, end_pause = 3, res = 1000, width = 10000, height = 8000,)
