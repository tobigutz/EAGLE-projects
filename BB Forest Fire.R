##### Brandenburg Forest Fires #####

# In the summer of2018 Eastern Germany experienced a severe drought with all-time low precipitation
# amounts. For example the Fläming area south of Berlin had less than 10 mm/m2 total precipitation
# in August and September, causing heavy strain on citizens, vegetation and water bodies.
# In late August two forest fires burned approx. 200 hectares of the "Zinna-Jüterbog-Keilberg 
# Natural Reserve" just north of Jüterbog, Brandenburg. Hundreds of people had to be evacuated and
# old ammunition leftovers from the former military training ground in the aera posed a threat to
# local firefighters.
#
# Further readings:
# http://www.maz-online.de/Lokales/Potsdam-Mittelmark/Treuenbrietzen/Waldbrand-bei-Treuenbrietzen-wuetet-weiter
# https://www.rbb24.de/panorama/beitrag/2018/08/hintergrund-brandenburg-wald-waldbraende.html
# 
# Goals:  1) Convert the raw Sentinel-2 data into Geotiffs (covered in getSpatialData.R)
#         2) Crop the tifs according to study area
#         3) Calculate NDVI, BAI and NBRI for study area
#         4) Run a Supervised Classification with and without indices to see
#            if the indices make a difference in SuperClass accuracy
#         5) Find areas where the land cover changed, especially in the forest
#         6) Calculate burned areas

###================ TO DOS ================###
###  1) Create time-series animations      ###
###  2) Improve SuperClass colors          ###
###  3) Better looking plots with ggplot2  ###
###  4) More advanced statistics           ###
###  5) Better file handling               ###
###========================================###

##### Preprocessing #####
datadir <- "./data/" # set data directory

library(rgdal)  
library(raster)

# list all directories in the data directory to get the Sentinel-2 folders containing the raw data
dirlist <- list.dirs(datadir, full.names = T) # list all directories
grep("R20m", dirlist) # we want only 20m resolution images, so we select only for those (10m or 60m also possible)
dirlist <- dirlist[c(7,14,21,28)] # filter for 20m folders
dirlist # check if correct folders were found

# this loop filters the acquisition dates from S2 nomenclature
dirlist[1] # check for typical patterns in S2 nomenclature
datelist <- list()
for (i in 1:length(dirlist)){
  S2.split <- strsplit(dirlist[i], split = "_") # split S2 name string
  S2.split <- S2.split[[1]][4]                  # extract date on the fourth position
  S2.split <- strsplit(S2.split, split = "T")   # S2 also gives the time of day, which we don't need
  datelist <- append(datelist, as.integer(S2.split[[1]][1])) # only use date
}
datelist <- unlist(datelist) # the loop above gives a nested list, unnest here
datelist                     # check for completeness


ff_extent <- extent(354335, 364335, 5765874, 5775874) # crop extent for the forest fire area
S2.names <- c("B2.Blue", "B3.Green", "B4.Red", "B5.Rededge1", "B6.Rededge2", "B7.Rededge3", 
              "B8.NIR", "B11.SWIR.1", "B12.SWIR.2") # band names for 20m Sentinel-2 images

# fetches and stacks all bands from one date, crops and exports them
for (i in 1:length(dirlist)){
  tempfile <- list.files(dirlist[i], pattern = "B", full.names = T) # extract only optical image bands
  tempstack <- stack(tempfile)                                      # stack current file
  tempstack <- crop(tempstack, ff_extent)                           # crop to desired extent
  writeRaster(x = tempstack, filename = paste0(datadir, datelist[i],"_crop"), 
              overwrite=T, format="GTiff")
}

croplist <- list.files(datadir, pattern = "_crop.tif$", full.names = T)
# Check if files are correct #
par(mfrow=c(2,2))
for (i in 1:length(croplist)){
  tempstack <- stack(croplist[i])
  plotRGB(x=tempstack, 3,2,1, stretch="lin")
  legend("top", legend = NA, title = as.character(datelist[i]), bty = "n", cex = 2, text.col="white")
  #if (i == 4){
  #  scalebar(5000, type='bar', divs=4, below="Meters", cex= 0.5)
  #}
}

##### Calculating BAI2, NDVI & NBRI #####
# BAI2 from Filipponi 2018
calc.BAI2 <- function(Band4, Band6, Band7, Band8, Band12){
  BAI2 <- ((1 - sqrt((Band6*Band7*Band8)/Band4)) * 
    (((Band12 - Band8) / sqrt(Band12+Band8)) + 1))
}

datelist <- sort(datelist) # sort here, given that the identifier in front of the name (line 39 ff.)
                           # may sort the files by name, not by date

# Calculate BAI, NDVI and NBRI for each raster and exports them
library(RStoolbox)
for (i in 1:length(croplist)){
  tempstack <- stack(croplist[i])
  names(tempstack) <- S2.names   # give names to temporal stack to be able to calculate every file equal
  BAI.temp <- calc.BAI2(tempstack$B4.Red, tempstack$B6.Rededge2, tempstack$B7.Rededge3,
                        tempstack$B8.NIR, tempstack$B12.SWIR.2)
  NDVI.temp <- spectralIndices(tempstack, red = "B4.Red", nir = "B8.NIR", indices = "NDVI")  # calculate NDVI
  NBRI.temp <- spectralIndices(tempstack, nir = "B8.NIR", swir3 = "B12.SWIR.2", indices = "NBRI")  # calculate NBRI
  tempstack <- addLayer(tempstack, BAI.temp, NDVI.temp, NBRI.temp) # stack all bands + indices
  names(tempstack)[10:12] <- c("BAI", "NDVI", "NBRI") # name index bands
  writeRaster(x = tempstack, filename = paste0(datadir, "/", datelist[i],"_indices"), overwrite=T, format="GTiff")
}

# load only files that also contain indices
indexlist <- list.files(datadir, pattern = "indices.tif$", full.names = T)

# Plot all indices
x11()
par(mfrow=c(4,3)) # arrange 4 dates with 3 indices apiece
for (i in 1:length(indexlist)){
  tempstack <- stack(indexlist[i])
  names(tempstack)[1:9]<- S2.names # obsolete?
  names(tempstack)[10:12] <- c("BAI", "NDVI", "NBRI")
  if (i == 1){  # do not print the name of the index every row, just for the top row
    plot(tempstack$BAI, main = paste0("BAI\n", as.character(datelist[i])), axes = F)
    plot(tempstack$NDVI, main = paste0("NDVI\n", as.character(datelist[i])), axes = F)
    plot(tempstack$NBRI, main = paste0("NBRI\n", as.character(datelist[i])), axes = F)
  }
  else{  # to identify the plots easier, we add the dates
    plot(tempstack$BAI, main = as.character(datelist[i]), axes = F)
    plot(tempstack$NDVI, main = as.character(datelist[i]), axes = F)
    plot(tempstack$NBRI, main = as.character(datelist[i]),axes = F)
  }
}
# Plot only NDVI
par(mfrow=c(1,2)) # arrange 4 dates with NDVI
for (i in 2:(length(indexlist)-1)){  # only show NDVI from 2nd and 3rd date
  tempstack <- stack(indexlist[i])
  names(tempstack)[10:12] <- c("BAI", "NDVI", "NBRI")
    plot(tempstack$NDVI, main = paste0("NDVI\n", as.character(datelist[i])))
}


library(RStoolbox)

# plot NDVI with a better looking tool called ggR

# function index.plot
#   plots a single index for a specific time
#   variables: tempstack = stacked image containing indices
#              index.name = name of the index you want to be plotted
#              date = give the name of the date to distinguish plots later
index.plot <- function(tempstack, index.name, date){  
  names(tempstack)[10:12] <- c("BAI", "NDVI", "NBRI")  # give name to the bands containing indices
  if (index.name == "BAI"){        # filter for desired index and sets the min/max value
    index.temp <- tempstack$BAI
    index.limit <- c(-2,2)
  }
  else if (index.name == "NDVI"){
    index.temp <- tempstack$NDVI
    index.limit <- c(0,1)
  }
  else if (index.name == "NBRI"){
    index.temp <- tempstack$NBRI
    index.limit <- c(0,1)
  }
  ggR(index.temp, geom_raster = T)+
  scale_fill_gradient2(low="red", mid='yellow', high="green", name = index.name, na.value = NA,
                       limits=index.limit)+                # set a gradient for index display
  ggtitle(paste0(index.name, "\n", as.character(date)))+   # plot index name and date under one another
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_x_continuous(breaks = seq(355000,370000,5000))+    # description for x/y axis
  scale_y_continuous(breaks = seq(5760000,5780000,5000))+
  xlab("")+
  ylab("")
}

### export desired index with according dates to a pdf file
library(gridExtra)
tempstackx <- stack(indexlist[1])
p1 <- index.plot(tempstackx, "NDVI", datelist[1])
tempstackx <- stack(indexlist[2])
p2 <- index.plot(tempstackx, "NDVI", datelist[2])
tempstackx <- stack(indexlist[3])
p3 <- index.plot(tempstackx, "NDVI", datelist[3])
tempstackx <- stack(indexlist[4])
p4 <- index.plot(tempstackx, "NDVI", datelist[4])

pdf("data/NDVI.pdf", width = 14, height = 8)
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()


##### Classification #####
library(rgdal)
Pre_Train.shp <- readOGR("data/Pre_Forestfire.shp")  # load in training data from before the forest fire
Post_Train.shp <- readOGR("data/Post_Forestfire.shp")  # training data from the burned areas
Post_Train.shp <- bind(Pre_Train.shp, Post_Train.shp)  # combine training data from burned and non-burned areas
summary(Post_Train.shp)  # check training classes and number of polygons per class

### 1) SuperClass for normal S2 Stack
par(mfrow=c(2,2))
for (i in 1:length(croplist)){
  if (i == 1 || i == 2){  # the first two images are pre forest fire, therefore require pre forest fire data
    training <- Pre_Train.shp
  }
  else{  # last two images are post-fire, thus require post fire data
    training <- Post_Train.shp
  }    
  tempstack <- stack(croplist[i])
  Classification <- superClass(tempstack,                # Raster to be classified
                               training,                 # Training Polygons
                               trainPartition = 0.5,     # Training/Validation ratio
                               responseCol = "MC_info",  # Attribute that the classifier uses
                               model = "rf")             # The Type of Model (here RandomForest)
  writeRaster(Classification$map, filename = paste0(datadir, "/", datelist[i],"_IndexSuperClass"), 
  overwrite=T, format="GTiff")
  plot(Classification$map, main = as.character(datelist[i]), axes = F)
  #  SuperClass also provides a validation matrix for accuracy assessment which is stored in this if/else 
  if (i == 1){
    validation.df <- Classification$validation$performance$overall  # first validation matrix is set as dataframe
  }
  else{
    validation.df <- cbind(validation.df, Classification$validation$performance$overall)  # adds later validation data
  }
}
for (i in 1:length(datelist)){
  colnames(validation.df)[i] = datelist[i]  # set dates as column names to identify them more easily
}

### 2) SuperClass for S2 Stack with indices
par(mfrow=c(2,2))
for (i in 1:length(indexlist)){
  if (i == 1 || i == 2){
    training <- Pre_Train.shp
  }
  else{
    training <- Post_Train.shp
  }    
  tempstack <- stack(indexlist[i])
  Classification <- superClass(tempstack,
                               training,
                               trainPartition = 0.5,  
                               responseCol = "MC_info", 
                               model = "rf") 
  writeRaster(Classification$map, filename = paste0(datadir, "/", datelist[i],"_IndexSuperClass"), 
              overwrite=T, format="GTiff")
  plot(Classification$map, main = as.character(datelist[i]), axes = F)
  if (i == 1){
    validation.df2 <- Classification$validation$performance$overall
  }
  else{
    validation.df2 <- cbind(validation.df2, Classification$validation$performance$overall)
  }
}
for (i in 1:length(datelist)){
  colnames(validation.df2)[i] = datelist[i]
}

# Combine validation results from both Supervised Classifications to evaluate accuracy
validation.result <- data.frame(rbind(validation.df, validation.df2))
write.csv(validation.result, file = paste0(datadir, "SuperClassValidation.csv"))


##### Accuracy Assessment #####
# Accuracy
validation.result[1,] # without indices
validation.result[8,] # with indices
rowMeans(validation.result[1,]) # mean accuracy across all dates without indices
rowMeans(validation.result[8,]) # with indices

# Kappa
validation.result[2,] # without indices
validation.result[9,] # with indices
rowMeans(validation.result[2,]) # mean Kappa across all dates without indices
rowMeans(validation.result[9,]) # with indices
# --> Accuracy and Kappa are both sightly higher when using indices


##### Postclassification Change Detection #####
# Given that SuperClass with indices improves accuracy, only those classified images will be used further
SClist <- list.files(datadir, pattern = "IndexSuperClass.tif$", full.names = T)

#Landcover_pre <- c("Agriculture", "Artificial Surface", "Barren", "Pasture", "Forest", "Pasture")
#Landcover_post <- c("Agriculture", "Forest", "Pasture", "Artificial Surface", "City", "Barren", 
#               "Fire Area", "Damaged Forest")

# set class colors for plotting. Pre for pre-fire classes (only 6), _post for post-fire classes (8)
Colors_pre <- c("gold", "darkmagenta", "khaki1", "chartreuse", "forestgreen", "green")
Colors_post <- c("gold", "darkmagenta", "khaki1", "chartreuse", "forestgreen", "green", "firebrick", "darkred")

par(mfrow=c(2,2))
for (i in 1:length(SClist)){
  tempraster <- raster(SClist[i])
  if (i == 1 || i == 2){  # for the first two dates use colors for pre-fire classes
    plotcol <- Colors_pre
  }
  else{                   # for the last two dates use colors for post-fire classes
    plotcol <- Colors_post
  }
  plot(tempraster, col = plotcol, main = as.character(datelist[i]))  ### ===> TO DO --> Why does R mix the colors?
}

### Land Cover Change map
# To see from-to land cover changes, we multiply the first raster with 10 and add the second raster
# Values with repdigits would show no land cover change
# Exports a raster with changes from date1 to date2
for (i in 1:(length(SClist)-1)){
  tempraster <- raster(SClist[i])
  tempraster <- tempraster * 10  # multiply raster values by 10
  tempraster2 <- raster(SClist[i+1])  # load second raster for land cover change
  changeraster <- tempraster + tempraster2
  writeRaster(changeraster, filename = paste0(datadir, datelist[i], "_", datelist[i+1], "_CD"), 
              overwrite=T, format="GTiff")
}

### Plot all 3 change maps
CDlist <- list.files(datadir, pattern = "_CD.tif$", full.names = T)
par(mfrow=c(1,3))
for (i in 1:length(CDlist)){
  plot(raster(CDlist[i]))
}

Forestmask <- readOGR("data/ForestMask.shp")  # add a forest mask to detect only changes in forests
Forestmask <- spTransform(Forestmask, CRS(proj4string(tempraster)))  # reproject mask to S2 reference system

### binary change map
# To create a binary change map, all unchanged pixels, i.e. repdigits, are set to 0,
# all others are changed to 1
par(mfrow=c(2,3))
for (i in 1:length(CDlist)){
  tempraster <- raster(CDlist[i])
  tempraster <- mask(tempraster, Forestmask)  # apply the mask from line 255
  tempraster[tempraster == 11] <- 0  # every non-change is set to 0
  tempraster[tempraster == 22] <- 0
  tempraster[tempraster == 33] <- 0
  tempraster[tempraster == 44] <- 0
  tempraster[tempraster == 55] <- 0
  tempraster[tempraster == 66] <- 0
  tempraster[tempraster == 77] <- 0
  tempraster[tempraster == 88] <- 0
  tempraster[tempraster != 0] <- 1  # all other pixels (i.e. changed ones) are set to 1
  #plot(tempraster, col = c("white", "red"))
  if (i == 1){
    CDstack <- tempraster
  }
  else{
    CDstack <- addLayer(CDstack, tempraster)  # stack all three change maps in one stack
  }
}




##### Under Construction #####
### create change map polygons
CD1 <- rasterToPolygons(CDstack[[1]], fun=function(x){x == 1}, dissolve = T)
CD2 <- rasterToPolygons(CDstack[[2]], fun=function(x){x == 1})
CD3 <- rasterToPolygons(CDstack[[3]], fun=function(x){x == 1})

plot(CD1)
plot(CD2)
plot(CD3)


# 1) Calculate burned area with
area(CD1)  # in m2
area(CD1)/1000000  # in km2

CD1$area_sqkm <- area(CD1)/1000000

# 2) Eliminate single pixel polygons
