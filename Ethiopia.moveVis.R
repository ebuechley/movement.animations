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
head(d)
unique(d$individual.taxon.canonical.name)
hv = subset(d, d$individual.taxon.canonical.name == "Necrosyrtes monachus")
wbv = subset(d, d$individual.taxon.canonical.name == "Gyps africanus")
rv = subset(d, d$individual.taxon.canonical.name == "Gyps rueppellii")
lfv = subset(d, d$individual.taxon.canonical.name == "Torgos tracheliotus")

#########################################################################
# hooded vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
hv.m = df2move(hv, proj = "+proj=longlat +datum=WGS84",
             x = 'location.long', y = 'location.lat', time = 'timestamp', 
             track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
move_data <- align_move(hv.m, res = "mean")
#move_data <- align_move(hv.m, res = 1, digit = 0, unit = "days")

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
                         path_colours = c('#e41a1c','#377eb8'),
                         tail_length = 50, tail_size = .2, tail_colour = '#ff7f00', trace_show = F, trace_colour = '#ff7f00', 
                         path_legend = F)
length(frames)

#customize frames
frames <- add_labels(frames, title = "Hooded Vulture Movements, Ethiopia", 
                     caption = "Prepared by: Evan Buechley, Smithsonian Migratory Bird Center / HawkWatch International") 
#frames <- add_scalebar(frames, height = 0.02, distance = 10, position = "bottomleft") # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[2000]]

# animate frames
animate_frames(frames, out_file = "./Outputs/HoodedVulture_MovementAnimation.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 3000, height = 6000,)


#########################################################################
# white-backed vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
wbv.m = df2move(wbv, proj = "+proj=longlat +datum=WGS84",
               x = 'location.long', y = 'location.lat', time = 'timestamp', 
               track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
move_data <- align_move(wbv.m, res = "mean")
#move_data <- align_move(hv.m, res = 1, digit = 0, unit = "days")

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
                         tail_length = 50, tail_size = .2, tail_colour = '#ff7f00', trace_show = F, trace_colour = '#ff7f00', 
                         path_legend = F)
length(frames)

#customize frames
frames <- add_labels(frames, title = "White-backed Vulture Movements, Ethiopia", 
                     caption = "Prepared by: Evan Buechley, Smithsonian Migratory Bird Center / HawkWatch International") 
#frames <- add_scalebar(frames, height = 0.02, distance = 10, position = "bottomleft") # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[1000]]

# animate frames
animate_frames(frames, out_file = "./Outputs/WhiteBackedVulture_MovementAnimation.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 4000, height = 3000,)

#########################################################################
# ruppell's vulture
#########################################################################
# use df2move to convert the data.frame into a moveStack
rv.m = df2move(rv, proj = "+proj=longlat +datum=WGS84",
                x = 'location.long', y = 'location.lat', time = 'timestamp', 
                track_id = 'individual.local.identifier')

# align move_data to a uniform time scale
move_data <- align_move(rv.m, res = "mean")
#move_data <- align_move(hv.m, res = 1, digit = 0, unit = "days")

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
                         tail_length = 50, tail_size = .2, tail_colour = '#ff7f00', trace_show = F, trace_colour = '#ff7f00', 
                         path_legend = F)
length(frames)

#customize frames
frames <- add_labels(frames, title = "Ruppell's Vulture Movements, Ethiopia", 
                     caption = "Prepared by: Evan Buechley, Smithsonian Migratory Bird Center / HawkWatch International") 
#frames <- add_scalebar(frames, height = 0.02, distance = 10, position = "bottomleft") # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames <- add_labels(frames, x = "Longitude", y = "Latitude") 
frames <- add_progress(frames, size = 2) # add a progress bar
frames[[1500]]

# animate frames
animate_frames(frames, out_file = "./Outputs/RuppellsVulture_MovementAnimation.mp4", overwrite = TRUE,
               fps = 10, end_pause = 3, res = 1000, width = 4000, height = 3000,)