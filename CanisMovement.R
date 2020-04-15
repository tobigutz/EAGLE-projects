loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; 
  library(mypkg, character.only=TRUE)  }

loadandinstall("move")
loadandinstall("moveVis")
loadandinstall("raster")
loadandinstall("ggplot2")


canis_csv <- read.csv("./movement/Alberta Grey Wolf.csv")
move_data <- move(canis_csv)

unique(canis_csv$tag.local.identifier)
sum(canis_csv$tag.local.identifier == "13791")
unique(timestamps(move_data))

unique(move_data$tag.local.identifier)
timeLag(move_data, unit = "days")
namesIndiv(move_data)

# move_data <- align_move(move_data, res = "max", digit = "max", unit = "days")
# frames <- frames_spatial(move_data, map_service = "osm", map_type = "watercolor", alpha = 0.5)
# animate_frames(frames, out_file = "example_0.gif", fps = 1)

login <- movebankLogin("t.gutzmann")
getMovebankID("ABoVE: Boutin Alberta Grey Wolf", login)
move_data2 <- getMovebankData(study= 492444603, login = login, removeDuplicatedTimestamps = T,) 

move_data3 <- align_move(move_data2, res = "min", digit = "min", unit = "hours")

frames <- frames_spatial(move_data3, map_service = "osm", map_type = "watercolor", alpha = 0.5)
length(frames)
frames <- add_labels(frames, x = "Longitude", y = "Latitude")
frames <- add_progress(frames)
frames <- add_scalebar(frames, height = 0.015)
frames <- add_northarrow(frames)
frames <- add_timestamps(frames, move_data3, type = "label")
animate_frames(frames, out_file = "example_1.gif", fps = 25, overwrite = T)