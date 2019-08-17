##########################
#MoveVis
##########################
setwd("~/Documents/GitHub/movement.animations/")
rm(list = ls())

#install and load packages
library(moveVis)
library(move)
library(beepr)

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
head(d)
unique(d$individual.taxon.canonical.name)
hv = subset(d, d$individual.local.identifier == "Odysseus")
#hv = subset(d, d$individual.taxon.canonical.nam == "Neophron percnopterus")
wbv = subset(d, d$individual.local.identifier == "R.Speers")
#wbv = subset(d, d$individual.taxon.canonical.name == "Gyps africanus")
rv = subset(d, d$individual.local.identifier == "Stratos")
#rv = subset(d, d$individual.taxon.canonical.name == "Gyps rueppellii")
#lfv = subset(d, d$individual.taxon.canonical.name == "Torgos tracheliotus")

#########################################################################
# hooded vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
hv.m = df2move(hv, proj = "+proj=longlat +datum=WGS84",
             x = 'location.long', y = 'location.lat', time = 'timestamp', 
             track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
#move_data <- align_move(hv.m, res = "max")
move_data <- align_move(hv.m, res = 8, digit = 0, unit = "hours")

# create spatial frames 
move_data
get_maptypes()
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "osm", map_type = "no_labels",
                         #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         map_service = "osm", map_type = "terrain",
                         map_dir = "~/Documents/MapDirectory/",
                         equidistant = F,
                         path_size = 1, path_end = "round", path_join = "round", path_fade = T, 
                         path_colours = NA,
                         tail_length = 5, tail_size = .1, tail_colour = 'red', trace_show = T, trace_colour = 'white', 
                         path_legend = F)
length(frames)
#frames[[500]]

#customize frames
frames <- add_labels(frames, title = "Hooded Vulture Movements, Ethiopia", 
                     caption = "Prepared by: Evan Buechley
Smithsonian Migratory Bird Center
HawkWatch International") 
frames <- add_scalebar(frames, height = 0.01, distance = 3, x = 39.85, y = 10.655, label_margin = 2) # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[500]]

# animate frames
suggest_formats()
animate_frames(frames, out_file = "./Outputs/HoodedVulture_MovementAnimation_Final.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 5000, height = 5000)
beep()

#########################################################################
# white-backed vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
wbv.m = df2move(wbv, proj = "+proj=longlat +datum=WGS84",
               x = 'location.long', y = 'location.lat', time = 'timestamp', 
               track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
#move_data <- align_move(wbv.m, res = "mean")
move_data <- align_move(wbv.m, res = 8, digit = 0, unit = "hours")

# create spatial frames 
#extent = extent(36.5,41.5,6.5,11.5)
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "osm", map_type = "no_labels",
                         #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         map_service = "osm", map_type = "terrain",
                         map_dir = "~/Documents/MapDirectory/",
                         #ext = extent,
                         equidistant = F,
                         path_size = 1, path_end = "round", path_join = "round", path_fade = T, 
                         path_colours = NA,
                         tail_length = 5, tail_size = .1, tail_colour = 'red', trace_show = T, trace_colour = 'white', 
                         path_legend = F)
length(frames)
#frames[[800]]

#customize frames
frames <- add_labels(frames, title = "White-backed Vulture Movements, Ethiopia", 
caption = "Prepared by: Evan Buechley
Smithsonian Migratory Bird Center
HawkWatch International") 
frames <- add_scalebar(frames, height = 0.02, distance = 120, x = 40.1, y = 8.25, label_margin = 2) # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[800]]

# animate frames
animate_frames(frames, out_file = "./Outputs/WhiteBackedVulture_MovementAnimation_Final.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 5000, height = 5000)

#########################################################################
# ruppell's vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
rv.m = df2move(rv, proj = "+proj=longlat +datum=WGS84",
                x = 'location.long', y = 'location.lat', time = 'timestamp', 
                track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
#move_data <- align_move(rv.m, res = "mean")
move_data <- align_move(rv.m, res = 8, digit = 0, unit = "hours")

# create spatial frames 
move_data
get_maptypes()
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "osm", map_type = "no_labels",
                         #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         map_service = "osm", map_type = "terrain",
                         map_dir = "~/Documents/MapDirectory/",
                         equidistant = F,
                         path_size = 1, path_end = "round", path_join = "round", path_fade = T, 
                         path_colours = NA,
                         tail_length = 5, tail_size = .1, tail_colour = 'red', trace_show = T, trace_colour = 'white', 
                         path_legend = F)
length(frames)

#customize frames
frames <- add_labels(frames, title = "Ruppell's Vulture Movements, Ethiopia", 
                     caption = "Prepared by: Evan Buechley
Smithsonian Migratory Bird Center
HawkWatch International") 
frames <- add_scalebar(frames, height = 0.01, distance = 100, x = 37, y = 8.25, label_margin = 2) # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[600]]

# animate frames
animate_frames(frames, out_file = "./Outputs/RuppellsVulture_MovementAnimation2.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 5000, height = 5000)

beep()
