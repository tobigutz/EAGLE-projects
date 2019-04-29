datadir <- "./data/"

library(rgdal)  
library(raster)

dirlist <- list.dirs(datadir, full.names = T)
grep("R20m", dirlist)
dirlist <- dirlist[c(7,14,21,28)]
dirlist

datelist <- list()
for (i in 1:length(dirlist)){
  S2.split <- strsplit(dirlist[i], split = "_")
  S2.split <- S2.split[[1]][4]
  S2.split <- strsplit(S2.split, split = "T")
  datelist <- append(datelist, as.integer(S2.split[[1]][1]))
}
datelist <- unlist(datelist)
#datelist <- sort(datelist)
datelist

ff_extent <- extent(354335, 364335, 5765874, 5775874)
S2.names <- c("B2.Blue", "B3.Green", "B4.Red", "B5.Rededge.1", "B6.Rededge.2", "B7.Rededge.3", 
              "B8.NIR.1", "B11.SWIR.1", "B12.SWIR.2")

for (i in 1:length(dirlist)){
  tempfile <- list.files(dirlist[i], pattern = "B", full.names = T)
  tempstack <- stack(tempfile)
  tempstack <- crop(tempstack, ff_extent)
  writeRaster(x = tempstack, filename = paste0(datadir, datelist[i],"_crop"), overwrite=T, format="GTiff")
}

##### Check if files are correct #####
par(mfrow=c(2,2))
for (i in 1:length(croplist)){
  tempstack <- stack(croplist[i])
  plotRGB(x=tempstack, 3,2,1, stretch="lin")
  legend("top", legend = NA, title = as.character(datelist[i]), bty = "n", cex = 2, text.col="white")
  #if (i == 4){
  #  scalebar(5000, type='bar', divs=4, below="Meters", cex= 0.5)
  #}
}
