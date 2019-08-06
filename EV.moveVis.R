##########################
#MoveVis
##########################
setwd("~/Documents/GitHub/movement.animations/")
rm(list = ls())

#install and load packages
#install.packages("devtools")
#library(devtools)
#install_github("16EAGLE/moveVis")
library(moveVis)
library(move)
library(lubridate)

#load data
d = read.csv("EV-collaboration.csv")
summary(d)

# Re-store DateTime_GMT as POSIXt object
d$timestamp <- ymd_hms(d$date, tz='GMT')
class(d$timestamp)
summary(d$timestamp)

# use df2move to convert the data.frame into a moveStack
head(d)
dm = df2move(d, proj = "+proj=longlat +datum=WGS84",
             x = 'long', y = 'lat', time = 'timestamp', 
             track_id = 'id')

# align move_data to a uniform time scale
move_data <- align_move(dm, res = 4, digit = 0, unit = "days")

# create spatial frames 
get_maptypes()
#extent = extent(-18,52,0,49)
#To use mapbox maps, you need to register for a free mapbox account and get a token key, which can be inserted below
frames <- frames_spatial(move_data, path_colours = NA,
                         map_service = "mapbox", map_type = "satellite", alpha = 1, map_res = 1,
                         margin_factor = 1.2,
                         map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         map_dir = "~/Google Drive/R projects/MapDirectory/",
                         path_legend = FALSE)
length(frames)
frames[[400]] # preview one of the frames

#customIze frames
frames <- add_labels(frames, x = "Longitude", y = "Latitude", title = "Egyptian Vulture Migrations", 
                     subtitle = "2007-2018") 
frames <- add_progress(frames) # add a progress bar
#frames <- add_scalebar(frames, height = 0.02, distance = 2000, x = -20, y = -1) # add a scale bar
#frames <- add_northarrow(frames, x = 50, y = -1) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps
frames[[400]]

# animate frames
suggest_formats()
animate_frames(frames, out_file = "EgyptianVultureCollaboration_MovementAnimation3.gif")
