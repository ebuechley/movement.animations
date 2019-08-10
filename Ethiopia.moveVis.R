##########################
#MoveVis
##########################
setwd("~/Documents/GitHub/movement.animations/")
rm(list = ls())

#install and load packages
library(moveVis)
library(move)

#load data
d = read.csv("Pan-Africa Vulture Tracking.csv")
unique(d$individual.local.identifier)
d = d[d$individual.local.identifier!=5316, ]
d = d[d$individual.local.identifier!="Fringilla", ]
d = d[d$individual.local.identifier!="Johan", ]
d = d[d$individual.local.identifier!="Lemba", ]
d = d[d$individual.local.identifier!="Mubanga", ]
d = d[d$individual.local.identifier!="Johan", ]
d = d[d$individual.local.identifier!="Precision", ]
d = d[d$individual.local.identifier!="Timbavati", ]
d = d[d$individual.local.identifier!="Johan", ]
d = d[d$individual.local.identifier!=5329, ]
d = d[d$individual.local.identifier!=5309, ]
d = d[d$individual.local.identifier!=5315, ]
d = d[d$individual.local.identifier!=1946411, ]
d = d[d$individual.local.identifier!=1380285, ]
d = d[d$individual.local.identifier!="Lizzy", ]
d = d[d$individual.local.identifier!=5317, ]
d = d[d$individual.local.identifier!="KingTut", ]
d = d[d$individual.local.identifier!="TomPetty", ]
summary(d)

# Re-store DateTime_GMT as POSIXt object
d$timestamp <- as.POSIXct(d$timestamp, tz='GMT')
class(d$timestamp)
summary(d$timestamp)
d = subset(d, timestamp >= as.POSIXct('2018-10-01 00:00:00') &
             timestamp <= as.POSIXct('2019-08-25 00:00:00'))
summary(d$timestamp)

#subset to 1 individual
unique(d$individual.local.identifier)
hv = subset(d, d$individual.local.identifier == "Kemise")
wbv = subset(d, d$individual.local.identifier == "JT")
rv = subset(d, d$individual.local.identifier == "Stratos")
lfv = subset(d, d$individual.local.identifier == "M.C.Chops")


#########################################################################
# hooded vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
hv.m = df2move(hv, proj = "+proj=longlat +datum=WGS84",
             x = 'location.long', y = 'location.lat', time = 'timestamp', 
             track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
move_data <- align_move(hv.m, res = "mean")

# create spatial frames 
unique(hv$individual.local.identifier)
move_data
get_maptypes()
#extent = extent(32.5,48.5,2.5,15.5)
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
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
#frames <- add_scalebar(frames, height = 0.02, distance = 10, position = "bottomleft") # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[1000]]

# animate frames
suggest_formats()
?animate_frames

animate_frames(frames, out_file = "./Outputs/HoodedVulture_MovementAnimation.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = xx, height = xx,)


#########################################################################
# ruppell's vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
rv.m = df2move(rv, proj = "+proj=longlat +datum=WGS84",
               x = 'location.long', y = 'location.lat', time = 'timestamp', 
               track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
move_data <- align_move(hv.m, res = "mean")

# create spatial frames 
unique(rv$individual.local.identifier)
move_data
get_maptypes()
#extent = extent(32.5,48.5,2.5,15.5)
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "osm", map_type = "no_labels",
                         map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         #map_service = "osm", map_type = "terrain",
                         map_dir = "~/Documents/MapDirectory/",
                         #ext = extent, 
                         equidistant = F,
                         path_size = .8, path_end = "round", path_join = "round", path_fade = T, 
                         #path_colours = c('red', 'green', '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120'),
                         path_colours = '#377eb8',
                         tail_length = 50, tail_size = .1, tail_colour = "white", trace_show = T, trace_colour = "white", 
                         path_legend = FALSE)
length(frames)
#frames[[1000]] # preview one of the frames

#customoze frames
?add_labels
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
#title = "Ethiopia Vulture Tracking, February 2019",
#subtitle = 
#                       "HawkWatch International, in collaboration with 
#Hawk Mountain Sanctuary & Max Planck Institute for Ornithology
#Prepared by Evan R. Buechley, Package moveVis, maps via Mapbox" ) # add labels, e.g. axis labels
frames <- add_progress(frames, size = 2) # add a progress bar
#frames <- add_scalebar(frames, height = 0.02, distance = 10) # add a scale bar
#frames <- add_northarrow(frames, x = 48, y = 3.5) # add a north arrow
#frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames[[1000]]

# animate frames
suggest_formats()
?animate_frames
animate_frames(frames, out_file = "./Outputs/RuppellsVulture_MovementAnimation.mp4", overwrite = TRUE,
               fps = 5, end_pause = 3, res = 220)
