##### Fetching Spatial Data #####
#devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)
library(raster)
library(sf)
library(sp)

set_aoi()
aoi <- aoi_data[[1]]

login_CopHub(username = "tobias.gutzmann", password = "Ctionotg45610")
set_archive("/data")

## Use getSentinel_query to search for data (using the session AOI)
records <- getSentinel_query(time_range = c("2018-09-01", "2018-10-30"), 
                             platform = "Sentinel-2")

## Filter the records
colnames(records) #see all available filter attributes
unique(records$platformidentifier) #use one of the, e.g. to see available processing levels

records_filtered <- records[which(records$processinglevel == "Level-2A"),] #filter by Level
records_filtered <- records_filtered[as.numeric(records_filtered$cloudcoverpercentage) <= 20, ] #filter by clouds

View(records_filtered)
getSentinel_preview(record = records_filtered[2,])

datasets <- getSentinel_data(records = records_filtered[c(10,8,3,2), ])

ziplist <- list.files("C:/data/get_data/Sentinel-2", pattern = ".zip", full.names = T)
## Finally, define an output format and make them ready-to-use
datasets_prep <- prepSentinel(ziplist, format = "tiff")

## View the files
datasets_prep[[1]][[1]][1] #first dataset, first tile, 10 m resolution
datasets_prep[[1]][[1]][2] #first dataset, first tile, 20 m resolution
datasets_prep[[1]][[1]][3] #first dataset, first tile, 60 m resolution

## Load them directly into R
r <- stack(datasets_prep[[1]][[1]][1])
