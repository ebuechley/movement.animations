##########################
#MoveVis
##########################
setwd("~/Documents/GitHub/movement.animations/")
rm(list = ls())

#install and load packages
library(moveVis)
library(move)
library(lubridate)

#load data
#d = read.csv("ev.tv.filtered.csv")
d = read.csv("EV.migration.flexibility.csv")
d = read.csv("EV.migration.flexibility.completmigrationsonly.csv")
head(d)

# Re-store DateTime_GMT as POSIXt object
#d$timestamp <- ymd_hms(d$date, tz='GMT')
d$timestamp <- ymd_hms(d$DateTime, tz='GMT')
class(d$timestamp)
summary(d$timestamp)

#
unique(d$species)
#ev = subset(d, species == "Neophron percnopterus")
#Logiya = subset(d, id == "Logiya")
ev = d
summary(ev)
names(ev)

# use df2move to convert the data.frame into a moveStack
dm = df2move(ev, proj = "+proj=longlat +datum=WGS84",
             x = 'long', y = 'lat', time = 'timestamp', 
             track_id = 'id')

# align move_data to a uniform time scale
move_data <- align_move(dm, res = 3, digit = 0, unit = "days")
#move_data <-  align_move(dm, res = "mean")

# create spatial frames 
get_maptypes()
#extent = extent(-18,52,0,49)
#To use mapbox maps, you need to register for a free mapbox account and get a token key, which can be inserted below
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "osm", map_type = "no_labels",
                         map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         #map_service = "osm", map_type = "terrain",
                         map_dir = "~/Documents/MapDirectory/",
                         #ext = extent, 
                         equidistant = F,
                         path_size = 1, path_end = "round", path_join = "round", path_fade = T, 
                         #path_colours = c('red', 'green', '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120'),
                         path_colours = NA,
                         tail_length = 50, tail_size = .2, tail_colour = "brown3", trace_show = T, trace_colour = "brown3", 
                         path_legend = FALSE)
length(frames)
#frames[100] # preview one of the frames

#customize frames
#frames <- add_labels(frames, x = "Longitude", y = "Latitude", title = "Egyptian Vulture Migrations", 
#                     subtitle = "2007-2018") 
#frames <- add_scalebar(frames, height = 0.02, distance = 2000, x = -20, y = -1) # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[1300]]

# animate frame
suggest_formats()
animate_frames(frames, out_file = "./Outputs/EgyptianVulture_MovementAnimation_MigrationFlexibility_completmigrationsonly.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 8000, height = 5000,)
