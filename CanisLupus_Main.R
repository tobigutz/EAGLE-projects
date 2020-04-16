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

unique(move_data2@data$tag_local_identifier)
timeLag(move_data, unit = "days")
namesIndiv(move_data)

# move_data <- align_move(move_data, res = "max", digit = "max", unit = "days")
# frames <- frames_spatial(move_data, map_service = "osm", map_type = "watercolor", alpha = 0.5)
# animate_frames(frames, out_file = "example_0.gif", fps = 1)

getMovebankID("ABoVE: Boutin Alberta Grey Wolf", login)
move_data2 <- getMovebankData(study= 492444603, login = login) 


move_data3 <- move_data2[move_data2$tag_local_identifier == c(13790,13791) ,]
length(move_data2)
move_data3 <- move_data2[seq(1,length(move_data2),25),]
move_data3 <- align_move(move_data3, res = "min", digit = "min", unit = "days")

frames <- frames_spatial(move_data3, map_service = "osm", map_type = "watercolor", alpha = 0.5)
# length(frames)
frames <- add_labels(frames, x = "Longitude", y = "Latitude")
# frames <- add_progress(frames)
frames <- add_scalebar(frames, height = 0.015)
# frames <- add_northarrow(frames)
# frames <- add_timestamps(frames, move_data3, type = "label")
animate_frames(frames, out_file = "CanisLupus_Movement.gif", fps = 30, overwrite = T)
