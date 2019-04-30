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

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("sp")
loadandinstall("raster")
loadandinstall("RStoolbox")
loadandinstall("rgdal")
loadandinstall("gdalUtils")
loadandinstall("ggplot2")
loadandinstall("lattice")
loadandinstall("gridExtra")

setwd("C:/Users/Tobias/Desktop/MB1 - Paper/")

croplist
S2.names <- c("B2.Blue", "B3.Green", "B4.Red", "B5.Rededge1", "B6.Rededge2", "B7.Rededge3", 
              "B8.NIR", "B11.SWIR.1", "B12.SWIR.2")

stack_1 <- stack(croplist[1])
names(stack_1) <- S2.names
stack_2 <- stack(croplist[2])
names(stack_2) <- S2.names
stack_3 <- stack(croplist[3])
names(stack_3) <- S2.names
stack_4 <- stack(croplist[4])
names(stack_4) <- S2.names

preFCC <- ggRGB(stack_2, r="B12.SWIR.2",g="B11.SWIR.1",b="B8.NIR", stretch = "lin")+
  ggtitle("FCC pre-fire")+
  labs(x="", y="")+
  theme(axis.text.y = element_text(angle = 90))
postFCC <- ggRGB(stack_3, r="B12.SWIR.2",g="B11.SWIR.1",b="B8.NIR", stretch = "lin")+
  ggtitle("FCC post-fire")+
  labs(x="", y="")+
  theme(axis.text.y = element_text(angle = 90))
pdf("1_Fire_FCC.pdf", width=14, height=8)
grid.arrange(preFCC, postFCC, ncol=2)
dev.off()

############### Calculate burned areas ############### 

nbr <- function(img,k,i){
  bk <- img[[k]]
  bi <-img[[i]]
  nbr <- (bk-bi)/(bk+bi)
  return(nbr)
}

NBR_prefire <- nbr(stack_2,"B8.NIR", "B12.SWIR.2")
NBR_postfire <- nbr(stack_3,"B8.NIR", "B12.SWIR.2")
dNBR <- NBR_prefire - NBR_postfire
dNBR <- reclassify(dNBR,cbind(0,+Inf,0))
dNBR <- dNBR * (-1)

NBR1 <- ggR(NBR_prefire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high = "darkred", name="NBR")+
  ggtitle("NBR pre-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

NBR2 <- ggR(NBR_postfire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high = "darkred", name="NBR")+
  ggtitle("NBR post-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

NBR3 <- ggR(dNBR, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high ="darkred", name="NBR")+
  ggtitle("dNBR")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

pdf("2_NBR.pdf", width=14, height=8)
grid.arrange(NBR1, NBR2, NBR3, ncol=3)
dev.off()

############################################################

mirbi <- function(img,k,i){
  bk <- img[[k]]
  bi <- img[[i]]
  mirbi <- (10*bi-9.8*bk+2)
  return(mirbi)
}

MIRBI_prefire <- mirbi(stack_2,"B11.SWIR.1", "B12.SWIR.2")
MIRBI_postfire <- mirbi(stack_3,"B11.SWIR.1", "B12.SWIR.2")
dMIRBI <- MIRBI_prefire - MIRBI_postfire
dMIRBI <- dMIRBI / 10000
plot(dMIRBI)

MIRBI1 <- ggR(MIRBI_prefire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high = "darkred", name="MIRBI")+
  ggtitle("MIRBI pre-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

MIRBI2 <- ggR(MIRBI_postfire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high = "darkred", name="MIRBI")+
  ggtitle("MIRBI post-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

MIRBI3 <- ggR(dMIRBI, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high ="darkred", name="MIRBI")+
  ggtitle("dMIRBI")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

pdf("3_MIRBI.pdf", width=14, height=8)
grid.arrange(MIRBI1, MIRBI2, MIRBI3, ncol=3)
dev.off()

burned_dNBR <- reclassify(dNBR,cbind(-Inf,0.2,NA))
plot(burned_dNBR,col= "red",main="Burned area (dNBR)")
B_dNBR <- ggR(burned_dNBR, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="red", high ="darkred", name="dNBR")+
  ggtitle("dMIRBI")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))
ggsave("1_BdNBR.png",B_dNBR)

burned_dMIRBI <- reclassify(dMIRBI,cbind(-Inf,1,NA))
burned_dMIRBI_resc <- burned_dMIRBI - 0.8 # to make both scales comparable
plot(burned_dMIRBI_resc,col= "red",main="Burned area (dMIRBI)")

###############################################

BAI <- function(img,k,i){
  bk <- img[[k]] # red
  bi <- img[[i]] # NIR
  bai <- (1 / ((0.1 + bk)^2 + (0.06 + bi)))
  return(bai)
}

BAI_prefire <- BAI(stack_2,"B4.Red", "B8.NIR")
BAI_postfire <- BAI(stack_3,"B4.Red", "B8.NIR")
dBAI <- BAI_prefire - BAI_postfire

BAI1 <- ggR(BAI_prefire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high = "darkred", name="BAI")+
  ggtitle("BAI pre-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

BAI2 <- ggR(BAI_postfire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high = "darkred", name="BAI")+
  ggtitle("BAI post-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

BAI3 <- ggR(dBAI, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="green", high ="darkred", name="BAI")+
  ggtitle("dBAI")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

pdf("4_BAI.pdf", width=14, height=8)
grid.arrange(BAI1, BAI2, BAI3, ncol=3)
dev.off()

####################################################################

NDVI <- function(img,k,i){
  bk <- img[[k]]
  bi <-img[[i]]
  ndvi <- (bi-bk)/(bi+bk)
  return(ndvi)
}

NDVI_prefire <- NDVI(stack_2,"B4.Red", "B8.NIR")
NDVI_postfire <- NDVI(stack_3,"B4.Red", "B8.NIR")
dNDVI <- NDVI_prefire - NDVI_postfire

NDVI1 <- ggR(NDVI_prefire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="red", high = "darkgreen", name="NDVI")+
  ggtitle("NDVI pre-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

NDVI2 <- ggR(NDVI_postfire, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="red", high = "darkgreen", name="NDVI")+
  ggtitle("NDVI post-fire")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

NDVI3 <- ggR(dNDVI, geom_raster = T, stretch = "lin")+
  scale_fill_gradient2(low="red", high ="darkgreen", name="NDVI")+
  ggtitle("dNDVI")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

pdf("5_NDVI.pdf", width=14, height=8)
grid.arrange(NDVI1, NDVI2, NDVI3, ncol=3)
dev.off()

pdf("6_Burnindices1.pdf", width=14, height=8)
grid.arrange(NBR1, NBR2, NBR3, MIRBI1, MIRBI2, MIRBI3, nrow=2, ncol=3)
dev.off()

pdf("7_Burnindices2.pdf", width=14, height=8)
grid.arrange(BAI1, BAI2, BAI3, NDVI1, NDVI2, NDVI3, nrow=2, ncol=3)
dev.off()

pdf("8_Diff_Burnindices.pdf", width=14, height=8)
grid.arrange(NBR3, MIRBI3, BAI3, NDVI3, nrow=2, ncol=2)
dev.off()

burned_dNBR <- reclassify(dNBR,cbind(-Inf,0.2,NA))
plot(burned_dNBR,col= "red",main="Burned area (dNBR)")
#writeRaster(burned_dNBR, filename="09_Burned area mask (dNBR)",format = "GTiff", overwrite = T)

# from dMIRBI, threshold: 1 (according to Lu et al. 2016)
burned_dMIRBI <- reclassify(dMIRBI,cbind(-Inf,1,NA))
burned_dMIRBI_resc <- burned_dMIRBI - 0.8 # to make both scales comparable
plot(burned_dMIRBI_resc,col= "red",main="Burned area (dMIRBI)")
#writeRaster(burned_dMIRBI_resc, filename="10_Burned area mask (dMIRBI)",format = "GTiff", overwrite = T)

Forestmask <- readOGR("data/ForestMask.shp")  # add a forest mask to detect only changes in forests
Forestmask <- spTransform(Forestmask, CRS(proj4string(stack_2)))  # reproject mask to S2 reference system

######

par(mfrow=c(2,2))
burned_dNBR <- reclassify(dNBR,cbind(-Inf,0.44,NA))
burned_dNBR <- mask(burned_dNBR, Forestmask)
plot(burned_dNBR,col= "red",main="Burned area (dNBR)", axes = F, legend = F)

burned_dMIRBI <- reclassify(dMIRBI,cbind(-Inf,1,NA))
burned_dMIRBI_resc <- burned_dMIRBI - 0.8
burned_dMIRBI_resc <- mask(burned_dMIRBI_resc, Forestmask)
plot(burned_dMIRBI_resc,col= "red",main="Burned area (dMIRBI)", axes = F, legend = F)

burned_dBAI <- reclassify(dBAI,cbind(-Inf,0.44,NA))
burned_dBAI_resc <- mask(burned_dBAI_resc, Forestmask)
plot(burned_dBAI_resc,col= "red",main="Burned area (dBAI)", axes = F, legend = F)

burned_dNDVI <- reclassify(dNDVI,cbind(-Inf,0.75,NA))
burned_dNDVI_resc <- mask(burned_dNDVI_resc, Forestmask)
plot(burned_dNDVI_resc,col= "red",main="Burned area (dNDVI)", axes = F, legend = F)

burned_dNBR_cl <- reclassify(dNBR, c(-Inf,0.27,NA, 0.27,0.44,1, 0.44,0.66,2, 0.66,Inf,3))
burned_dNBR_cl <- mask(burned_dNBR_cl, Forestmask)
burned_dMIRBI_cl <- reclassify(dMIRBI, c(-Inf,1,NA, 1,1.17,1, 1.17,1.4,2, 1.4,Inf,3))
burned_dMIRBI_cl <- mask(burned_dMIRBI_cl, Forestmask)
burned_dBAI_cl <- reclassify(dBAI, c(-Inf,1,NA, 1,1.17,1, 1.17,1.4,2, 1.4,Inf,3))


miat <- c(1,2,3,4)
myColorkey <- list(at = miat,labels = list(labels = c("Low-Moderate", "Moderate-High","High"), at = miat+0.5))
BdNBR <- spplot(burned_dNBR_cl, maxpixels=1000000, main='Burn severity classes (dNBR)',colorkey= myColorkey)
BdMIRBI <- spplot(burned_dMIRBI_cl, maxpixels=1000000, main='Burn severity classes (dMIRBI)',colorkey= myColorkey)

pdf("9_Binary_FF.pdf", width=14, height=8)
grid.arrange(BdNBR, BdMIRBI, ncol=2)
dev.off()
#

calc_pixels1 <- cellStats(burned_dNBR, stat='sum',na.rm=T)
calc_pixels2 <- cellStats(burned_dMIRBI, stat='sum',na.rm=T)

# totally burned area in km2 -> multiply with 20^2 (pixel size)
burned_area1 <- calc_pixels1*20^2/1000000
burned_area2 <- calc_pixels2*20^2/1000000

area_df <- data.frame(matrix(ncol = 2, nrow = 2))
names(area_df) <- c("total_burned_area(m2)", "total_burned_area(km2)")
row.names(area_df) <- c("dNBR", "dMIRBI")
area_df$`total_burned_area(km2)` <- c(burned_area1,burned_area2)
area_df$`total_burned_area(m2)` <- c(calc_pixels1,calc_pixels2)
write.csv(area_df, file = "burned_area.csv")

BdNBR.shp <- rasterToPolygons(burned_dNBR_cl, dissolve = T)
BdMIRBI.shp <- rasterToPolygons(burned_dMIRBI_cl, dissolve = T)

BdNBR.shp <- spTransform(BdNBR.shp, CRS(proj4string(stack_2)))
BdMIRBI.shp <- spTransform(BdMIRBI.shp, CRS(proj4string(stack_2)))

writeOGR(BdNBR.shp, ".", "BSC_dNBR", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(BdMIRBI.shp, ".", "BSC_dMIRBR", driver="ESRI Shapefile", overwrite_layer=TRUE)  

ggRGB(stack_2, r="B4.Red",g="B3.Green",b="B2.Blue", stretch = "lin")+
  ggtitle("True Color Pre-fire")+
  labs(x="", y="")+
  theme(axis.text.y = element_text(angle = 90))+
  geom_polygon(col = 'red', data = BdNBR.shp, aes(x=long, y=lat))



#


#############################################################

stack_2_indices <- addLayer(stack_2, NBR_prefire, MIRBI_prefire, BAI_prefire, NDVI_prefire)
names(stack_2_indices)[10:13] <- c("NBRI", "MIRBI", "BAI", "NDVI")
stack_3_indices <- addLayer(stack_3, NBR_postfire, MIRBI_postfire, BAI_postfire, NDVI_postfire)
names(stack_3_indices)[10:13] <- c("NBRI", "MIRBI", "BAI", "NDVI")


writeRaster(stack_3_indices, "2_PostFire.tif", format="GTiff", overwrite=TRUE)
#################################################

Pre_Train.shp <- readOGR("data/Pre_Forestfire.shp")  # load in training data from before the forest fire
Post_Train.shp <- readOGR("data/Post_Forestfire.shp")  # training data from the burned areas
Post_Train.shp <- bind(Pre_Train.shp, Post_Train.shp)

par(mfrow=c(2,2))
Classification <- superClass(stack_2_indices,                # Raster to be classified
                             Pre_Train.shp,                 # Training Polygons
                             trainPartition = 0.5,     # Training/Validation ratio
                             responseCol = "MC_info",  # Attribute that the classifier uses
                             model = "rf")             # The Type of Model (here RandomForest)
#validation.df <- Classification$validation$performance$overall

Classification2 <- superClass(stack_3_indices,                # Raster to be classified
                             Post_Train.shp,                 # Training Polygons
                             trainPartition = 0.5,     # Training/Validation ratio
                             responseCol = "MC_info",  # Attribute that the classifier uses
                             model = "rf")             # The Type of Model (here RandomForest)
#validation.df2 <- Classification2$validation$performance$overall

#validation.result <- data.frame(rbind(validation.df, validation.df2))
#write.csv(validation.result, file = paste0(datadir, "SuperClassValidation.csv"))


#par(mfrow=c(2,1))
Classification <- superClass(stack_2,                # Raster to be classified
                             Pre_Train.shp,                 # Training Polygons
                             trainPartition = 0.5,     # Training/Validation ratio
                             responseCol = "MC_info",  # Attribute that the classifier uses
                             model = "rf")             # The Type of Model (here RandomForest)
plot(Classification$map)
validation.df <- Classification$validation$performance$overall

Classification2 <- superClass(stack_3,                # Raster to be classified
                              Post_Train.shp,                 # Training Polygons
                              trainPartition = 0.5,     # Training/Validation ratio
                              responseCol = "MC_info",  # Attribute that the classifier uses
                              model = "rf")             # The Type of Model (here RandomForest)
plot(Classification2$map)
#validation.df2 <- Classification2$validation$performance$overall

#validation.result <- data.frame(rbind(validation.df, validation.df2))
#write.csv(validation.result, file = paste0(datadir, "SuperClassValidation2.csv"))

par(xpd = TRUE)
plot(Classification$map, legend = FALSE,
  col = c("khaki1", "khaki1", "khaki1", "khaki1", "darkgreen", "forestgreen"),
  xaxt = 'n', yaxt = 'n'
)
par(xpd = TRUE)
legend( "bottom",
  legend = c("Agriculture", "City", "Barren Land", "City", "Forest", "Pasture"),
  fill = c("khaki1", "khaki1", "khaki1", "khaki1", "darkgreen", "forestgreen"),
  horiz = TRUE, inset = -0.175
)

SCmap1 <- ggR(Classification$map, geom_raster = T, forceCat = T)+
  scale_fill_manual(values=c("khaki1", "khaki1", "khaki1", "khaki1", "darkgreen", "forestgreen"))+
  ggtitle("Supervised Classification")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

SCmap2 <- ggR(Classification2$map, geom_raster = T, forceCat = T)+
  scale_fill_manual(values=c("khaki1", "khaki1", "khaki1", "khaki1", "firebrick", "red", "darkgreen", "forestgreen"))+
  ggtitle("Supervised Classification")+
  labs(x="", y="")+
  theme(axis.text.x = element_text(angle = 45, size = 8))+
  theme(axis.text.y = element_text(angle = 90, size = 8))+
  scale_y_continuous(breaks=seq(5760000,5780000,5000))

pdf("10_SuperClassComp_Index.pdf", width=14, height=8)
grid.arrange(SCmap1, SCmap2, ncol=2)
dev.off()
###############################################################################

