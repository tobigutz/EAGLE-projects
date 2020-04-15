loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; 
  library(mypkg, character.only=TRUE)  }
loadandinstall("rms")
loadandinstall("raster")
loadandinstall("mgcv")
loadandinstall("randomForest")
loadandinstall("dismo")
loadandinstall("rgdal")
loadandinstall("ellipse")
loadandinstall("rJava")
loadandinstall("XML")
loadandinstall("rgeos")
loadandinstall("dplyr")
loadandinstall("RStoolbox")

bio <- raster::getData("worldclim", var = "bio", res = 0.5, lon = 10, lat = 50)
bio_fut <- raster::getData('CMIP5', var='tmin', res=10, rcp=85, model='AC', year=50)
plot(raster(bio, 1))

bnd_ger <- raster::getData("GADM", country = "DEU", level = 2)
# bnd_pol <- raster::getData("GADM", country = "POL", level = 0)
# bnd_cze <- raster::getData("GADM", country = "CZE", level = 0)
# bnd_ned <- raster::getData("GADM", country = "NL", level = 0)
# bnd_aut <- raster::getData("GADM", country = "AT", level = 0)
# bnd_svk <- raster::getData("GADM", country = "SVK", level = 0)
# bnd <- sp::rbind.SpatialPolygonsDataFrame(bnd_pol, bnd_cze, bnd_ned, bnd_aut, bnd_svk)


#' Crop to study area extent (with a 5 degree buffer in each direction)
biocrop <- crop(bio, extent(bnd_ger))
plot(raster(biocrop, 1))
plot(bnd_ger, add=TRUE)


# if (file.exists("./GIS/Canislupus.mif")) {
#   species <- readOGR("./GIS/Canislupus.mif", layer = "Canislupus")
# } else {
#   # Download species location data from gbif
#   species0 <- gbif('Canis', 'lupus', ext = extent(biocrop))
#   species <- subset(species0,select=c("lat","lon"))
#   species <- na.omit(species)
#   coordinates(species) <- c("lon", "lat")  # set spatial coordinates
#   
#   # Add projection information
#   proj4string(species) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#   species <- SpatialPointsDataFrame(species, species0)
#   # Save species records in mif-format (preserves full column names)
#   writeOGR(species, "./GIS", 
#            "Canislupus", driver="MapInfo File", dataset_options="FORMAT=MIF", overwrite = T)
#   write.csv(species0, "./GIS/Canislupus.csv")
# }

species0 <- gbif('Canis', 'lupus', ext = extent(biocrop))
species <- subset(species0,select=c("lat","lon"))
species <- na.omit(species)
coordinates(species) <- c("lon", "lat")  # set spatial coordinates

# Add projection information
proj4string(species) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
species <- SpatialPointsDataFrame(species, species0)

plot(raster(biocrop, 1))
plot(bnd_ger, add = TRUE)
plot(species, add = TRUE)

unique(species$scientificName)
species2 <- species[!species$scientificName == "Canis familiaris Linnaeus, 1758",]
species2 <- species2[!species2$scientificName == "Canis lupus familiaris Linnaeus, 1758",]
species2 <- species2[!species2$scientificName == "Canis lupus lycaon Schreber, 1775",]
species2 <- species2[!species2$scientificName == "BOLD:AAA1542",]
species2 <- species2[!is.na(species2$year),]
species2 <- species2[species2$year > 1990,]

species3 <- species0[!species0$scientificName == "Canis familiaris Linnaeus, 1758",]
species3 <- species3[!species3$scientificName == "Canis lupus familiaris Linnaeus, 1758",]
species3 <- species3[!species3$scientificName == "Canis lupus lycaon Schreber, 1775",]
species3 <- species3[!species3$scientificName == "BOLD:AAA1542",]
species3 <- species3[!is.na(species3$year),]
species3 <- species3[species3$year > 1990,]


plot(raster(biocrop, 1))
plot(bnd_ger, add=TRUE)
plot(species2, add = TRUE)

x11()
ggplot()+
  geom_polygon(data=bnd_ger, aes(x=long, y=lat, group=group), 
               fill=NA,color="grey50", size=1)+
  geom_point(data=species0, aes(x=lon, y=lat, colour=year))+
  scale_colour_gradient(low = "darkblue", high = "red")


##### Prediction Modeling #####

cm <- cor(getValues(bio), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

#' ### Select an uncorrelated subset of environmental variables ###
env <- subset(biocrop, c("bio1_16","bio2_16", "bio5_16", "bio6_16", "bio7_16", "bio12_16", "bio15_16"))

set.seed(2)
background <- randomPoints(biocrop, 100, species2)
#' Select only one presence record in each cell of the environmental layer
presence <- gridSample(species2, biocrop, n = 1)

#' 
#' Now we combine the presence and background points, adding a 
#' column "species" that contains the information about presence (1)
#' and background (0)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                   data = data.frame("species2" = rep(c(1,0), 
                                                                             c(nrow(presence), nrow(background)))),
                                   match.ID = FALSE,
                                   proj4string = CRS(projection(env)))
#' Add information of environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(biocrop, fulldata))

#' 
# Split data set into a training and test data set
set.seed(2)
fold <- kfold(fulldata, k = 5)
traindata <- fulldata[fold != 1, ]
testdata <- fulldata[fold == 1, ]

varnames <- c("bio1_16","bio2_16", "bio5_16", "bio6_16", "bio7_16", "bio12_16", "bio15_16")

## Generalized Linear Model

## Generalized additive models
gammodel <- gam(species2 ~ s(bio1_16) + s(bio2_16) + s(bio5_16) + s(bio6_16) + s(bio7_16) + s(bio12_16) + s(bio15_16),
                family="binomial", data=traindata)
summary(gammodel)
plot(gammodel)

gamtest <- predict(gammodel, newdata = testdata, type = "response")
val.prob(gamtest, testdata[["species2"]])

# Variable importance
source("varImpBiomod.R")
gamimp <- varImpBiomod(gammodel, varnames, traindata)
barplot(100 * gamimp/sum(gamimp), ylab = "Variable importance (%)")

# Response functions
plot(gammodel, pages = 1)

# Prediction map
gammap <- predict(biocrop, gammodel, type = "response")

plot(gammap)
plot(species2, add=T)

#wolfmap_ger <- ggR(gammap$layer, geom_raster = T, stretch = "lin")+
ggR(gammap$layer, geom_raster = T, stretch = "lin")+
  scale_fill_gradient(low = "orange", high = "darkgreen", name = "suitability")+
  xlab("longitude")+
  ylab("latitude")+
  geom_polygon(data=bnd_ger, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey30", size=1, alpha=0.1)+
  #geom_polygon(data=bnd_sx, aes(x=long, y=lat, group=group), 
  #             fill=NA,color="black", size=1)+
  geom_point(data=species3, aes(x=lon, y=lat), colour="black", size = 3)+
  geom_point(data=species3, aes(x=lon, y=lat, colour=year), size = 2)+
  scale_colour_gradient(low = "darkblue", high = "red")

pdf("Wolfkarte.pdf", width = 14, height = 8)
wolfmap_ger
dev.off()
