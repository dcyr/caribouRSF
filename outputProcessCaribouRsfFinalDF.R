rm(list = ls())
setwd("~/Travail/SCF/CBFA/caribou")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(rgdal)
require(dplyr)
require(RColorBrewer)
require(reshape2)


# ############################################################
# ############################################################
# #########
# #########   Extracting average final conditions rasters, all replicates
# #########
# ts <- 100 ## final timesteps, simulation is omitted if timestep unavailable
# simDir <- ifelse(Sys.info()[["nodename"]] == "dcyr-desktop",
#                      "/media/dcyr/Data/caribouRsOutputs/",
#                      "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/")
# ecodistricts <- raster("../gis/ecodistricts.tif")
# ############################################################
# ############################################################
# 
# simOutputs <- list.files(simDir, full.names = T)
# caribouRsTreatments <- unique(substr(simOutputs, 1, nchar(simOutputs)-8 ))
# 
# for (i in seq_along(caribouRsTreatments)) {
#     treat <- caribouRsTreatments[i]
#     tName <- basename(treat)
#     fName <- simOutputs[grep(paste(paste(treat, "_", c(1:5), sep = ""), collapse = "|"), simOutputs)]
#     simInfo <- unlist(strsplit(tName, "_"))
# 
#     scenName  <- simInfo[3]
#     harvName <- ifelse(grepl("0.5", simInfo[4]), "harvest50", "harvest100")
#     fireName <- ifelse(grepl("Baseline", simInfo[4]), "fireBaseline", "fireProjected")
# 
#     print(paste("processing", tName))
#     for (j in seq_along(fName)) {
#         caribouRS <- get(load(fName[j]))
# 
#         x <- which(names(caribouRS) == paste0("caribouRS_", ts))
#         ## check if layer exists
#         if(!length(x) == 1) next
#         x <- caribouRS[[x]]
# 
#         if(j == 1) {
#             tmp <- x
#             layerName <- names(x)
# 
#         }
#         else {
#             tmp <- stack(tmp, x)
#         }
#         print(paste("replicate", j, "of", length(fName)))
#     }
# 
#     layerName <- gsub("caribouRS_", "caribouRsMean_Y", layerName)
#     layerName <- paste(layerName, scenName, fireName, harvName, sep = "_")
#     tmp <- mean(tmp)
#     names(tmp) <- layerName
#     if(i == 1) {
#         caribouRsMeanFinal <-  tmp
#     } else {
#         caribouRsMeanFinal <- stack(caribouRsMeanFinal, tmp)
#     }
# }
# #########
# save(caribouRsMeanFinal, file = "caribouRsMeanFinal.RData")
# ############################################################



# ####################################################################################
# ####################################################################################
# #########
# #########   Storing average final conditions (and differences) into tidy dataframes, all replicates
# #########
# ts <- 100 ## final timesteps, simulation is omitted if timestep unavailable
# simDir <- ifelse(Sys.info()[["nodename"]] == "dcyr-desktop",
#                      "/media/dcyr/Data/caribouRsOutputs/",
#                      "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/")
# ecodistricts <- raster("../gis/ecodistricts.tif")
# ############################################################
# ############################################################
# caribouRsMeanFinal <- get(load("../processedOutputs/caribouRsMeanFinal.RData"))
# caribouRsInit <- get(load("../processedOutputs/caribouRsInit.RData"))
# ecodistricts <- raster("../gis/ecodistricts.tif")
# ####################
# 
# ecodistricts <- projectRaster(ecodistricts, crs = CRS("+init=epsg:4326"))
# ecodistrictsDF <- rasterToPoints(ecodistricts)
# ecodistrictsDF <- data.frame(ecodistrictsDF)
# colnames(ecodistrictsDF)[1:2] <- c("longitude", "latitude")
# 
# ### computing variation between initial and final conditions
# caribouRsMeanFinalDiff <- caribouRsMeanFinal - caribouRsInit
# names(caribouRsMeanFinalDiff) <- gsub("Mean", "MeanDiff", names(caribouRsMeanFinal))
# 
# # ### computing focal statistics (moving averages)
# # w <- matrix(1/49, nrow = 7, ncol = 7)
# #
# # for (i in 1:nlayers(caribouRsMeanFinalDiff)) {
# #     x <- caribouRsMeanFinalDiff[[i]]
# #     tmp <- focal(x, w)
# #     names(tmp) <- names(x)
# #     if (i == 1) {
# #         r <- tmp
# #     } else {
# #         r <- stack(r, tmp)
# #     }
# # }
# 
# r <- projectRaster(caribouRsMeanFinalDiff, crs = CRS("+init=epsg:4326"))
# caribouRsMeanFinal <- projectRaster(caribouRsMeanFinal, crs = CRS("+init=epsg:4326"))
# caribouRS_Diff <- rasterToPoints(r)
# caribouRsMeanFinalDF <- rasterToPoints(caribouRsMeanFinal)
# df <- data.frame(caribouRS_Diff)
# caribouRsMeanFinalDF <- data.frame(caribouRsMeanFinalDF)
# colnames(df)[1:2] <- colnames(caribouRsMeanFinalDF)[1:2] <- colnames(ecodistrictsDF)[1:2]
# 
# 
# 
# ## merging ecodistrict DF
# df <- merge(df, ecodistrictsDF)
# caribouRsMeanFinalDF <- merge(caribouRsMeanFinalDF, ecodistrictsDF)
# 
# 
# df <- melt(df, id = c("longitude", "latitude", "ecodistricts"))
# caribouRsMeanFinalDF <- melt(caribouRsMeanFinalDF, id = c("longitude", "latitude", "ecodistricts"))
# df$variable <- as.character(df$variable)
# caribouRsMeanFinalDF$variable <- as.character(caribouRsMeanFinalDF$variable)
# 
# ## adding tidy treatment factors to diff df
# simInfo <- strsplit(df$variable, "_")
# fire <- lapply(simInfo, function(x) x[4])
# fire <- ifelse(fire == "fireBaseline", "Fire regime: baseline", "Fire regime: projected")
# harvest <- lapply(simInfo, function(x) x[5])
# harvest <- ifelse(harvest == "harvest50", "Harvesting level: 50%", "Harvesting level: 100%")
# harvest <- factor(harvest, levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
# scenario <- as.character(lapply(simInfo, function(x) x[3]))
# caribouRsFinalDiffDF <- data.frame(df, scenario = as.factor(scenario), fire = as.factor(fire), harvest = as.factor(harvest))
# 
# ## adding tidy treatment factors to diff df
# simInfo <- strsplit(caribouRsMeanFinalDF$variable, "_")
# fire <- lapply(simInfo, function(x) x[4])
# fire <- ifelse(fire == "fireBaseline", "Fire regime: baseline", "Fire regime: projected")
# harvest <- lapply(simInfo, function(x) x[5])
# harvest <- ifelse(harvest == "harvest50", "Harvesting level: 50%", "Harvesting level: 100%")
# harvest <- factor(harvest, levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
# scenario <- as.character(lapply(simInfo, function(x) x[3]))
# caribouRsMeanFinalDF <- data.frame(caribouRsMeanFinalDF, scenario = as.factor(scenario), fire = as.factor(fire), harvest = as.factor(harvest))
# 
# 
# #############################################
# save(caribouRsFinalDiffDF, file = "caribouRsFinalDiffDF.RData")
# save(caribouRsMeanFinalDF, file = "caribouRsMeanFinalDF.RData")
# ###########################################



