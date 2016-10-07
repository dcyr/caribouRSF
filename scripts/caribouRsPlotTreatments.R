rm(list = ls())
setwd("~/Travail/SCF/CBFA/caribou")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

caribouRsMeanFinal <- get(load("../processedOutputs/caribouRsMeanFinal.RData"))
caribouRsMeanFinal <- projectRaster(caribouRsMeanFinal, crs = CRS("+init=epsg:4326"))
plot(caribouRsMeanFinal)
require(raster)
### fortify for ggplot2

caribouRsMeanFinalDF <- rasterToPoints(caribouRsMeanFinal)

require(reshape2)
caribouRsMeanFinalDF <- melt(caribouRsMeanFinalDF, id = c("x", "y"), value.name = "RS_Call")
head(caribouRsMeanFinalDF)
### plot using facet grid (scen ~ fire, scen ~ harv, harv ~ fire)

plot(caribouRsMeanFinal)
