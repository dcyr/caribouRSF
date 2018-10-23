##################################################################
##################################################################
#### Dominic Cyr
rm(list =ls())
setwd("~/Travail/SCF/CBFA/caribou")
##################################################################
wwd <- (paste(getwd(), Sys.Date(), sep ="/"))
dir.create(wwd)
setwd(wwd)
outputDir <- ifelse(Sys.info()[["nodename"]] == "dcyr-desktop",
                    "/media/dcyr/Data/Sims/caribouRsOutputs",
                    "/media/dcyr/Seagate Backup Plus Drive/Sync/Sims/caribouRsOutputs")
require(raster)
require(rgdal)
require(dplyr)
require(RColorBrewer)
require(reshape2)
ecodistricts <- raster("../gis/ecodistricts.tif")

# ############################################################
# ############################################################
# #########
# #########   Extracting average final conditions rasters, all replicates
# #########
# ts <- 100 ## final timesteps, simulation is omitted if timestep unavailable
# ############################################################
# ############################################################
# 
# simOutputs <- list.files(outputDir, full.names = T)
# caribouRsTreatments <- unique(substr(simOutputs, 1, nchar(simOutputs)-8 ))
# 
# for (i in seq_along(caribouRsTreatments)) {
#     treat <- caribouRsTreatments[i]
#     tName <- basename(treat)
#     fName <- simOutputs[grep(paste(paste(treat, "_", c(1:5), sep = ""), collapse = "|"), simOutputs)]
#     simInfo <- unlist(strsplit(tName, "_"))
# 
#     scenName  <- simInfo[3]
#     harvName <- ifelse(grepl("Harvest",simInfo[4]),
#                              ifelse(grepl("0.5", simInfo[4]), "harvest50", "harvest100"),
#                        "harvest0")
#     fireName <- ifelse(grepl("BaselineFire", simInfo[4]), "fireBaseline",
#                        ifelse(grepl("ProjectedFire", simInfo[4]), "fireProjected", "fireNone"))
#     growthName <- ifelse(grepl("Growth", simInfo[4]), "growthProjected", "growthBaseline")
#     budwormName <- ifelse(grepl("Budworm", simInfo[4]), "budwormTrue", "budwormFalse")
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
#     layerName <- paste(layerName, scenName, growthName, fireName, harvName, budwormName, sep = "_")
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



####################################################################################
####################################################################################
#########
#########   Storing average final conditions (and differences) into tidy dataframes, all replicates
#########
ts <- 100 ## final timesteps, simulation is omitted if timestep unavailable
############################################################
############################################################
caribouRsMeanFinal <- get(load("../processedOutputs/caribouRsMeanFinal.RData"))
caribouRsInit <- get(load("../processedOutputs/caribouRsInit.RData"))
####################

ecodistricts <- projectRaster(ecodistricts, crs = CRS("+init=epsg:4326"))
ecodistrictsDF <- rasterToPoints(ecodistricts)
ecodistrictsDF <- data.frame(ecodistrictsDF)
colnames(ecodistrictsDF)[1:2] <- c("longitude", "latitude")

### computing variation between initial and final conditions
caribouRsMeanFinalDiff <- caribouRsMeanFinal - caribouRsInit
names(caribouRsMeanFinalDiff) <- gsub("Mean", "MeanDiff", names(caribouRsMeanFinal))


r <- projectRaster(caribouRsMeanFinalDiff, crs = CRS("+init=epsg:4326"))
caribouRsMeanFinal <- projectRaster(caribouRsMeanFinal, crs = CRS("+init=epsg:4326"))
caribouRS_Diff <- rasterToPoints(r)
caribouRsMeanFinalDF <- rasterToPoints(caribouRsMeanFinal)
df <- data.frame(caribouRS_Diff)
caribouRsMeanFinalDF <- data.frame(caribouRsMeanFinalDF)
colnames(df)[1:2] <- colnames(caribouRsMeanFinalDF)[1:2] <- colnames(ecodistrictsDF)[1:2]



## merging ecodistrict DF
df <- merge(df, ecodistrictsDF)
caribouRsMeanFinalDF <- merge(caribouRsMeanFinalDF, ecodistrictsDF)


df <- melt(df, id = c("longitude", "latitude", "ecodistricts"))
caribouRsMeanFinalDF <- melt(caribouRsMeanFinalDF, id = c("longitude", "latitude", "ecodistricts"))
df$variable <- as.character(df$variable)
caribouRsMeanFinalDF$variable <- as.character(caribouRsMeanFinalDF$variable)

## adding tidy treatment factors to final df
simInfo <- strsplit(df$variable, "_")
growth <- as.character(lapply(simInfo, function(x) x[4]))
growth <- ifelse(growth == "growthBaseline", "Growth: baseline", "Growth: projected")
fire <- as.character(lapply(simInfo, function(x) x[5]))
fire <- ifelse(fire == "fireBaseline", "Fire regime: baseline",
               ifelse(fire == "fireProjected", "Fire regime: projected", "No Fires"))
harvest <- as.character(lapply(simInfo, function(x) x[6]))
harvest <- ifelse(harvest == "harvest0", "Harvesting level: 0%",
                  ifelse(harvest == "harvest50", "Harvesting level: 50%", "Harvesting level: 100%"))
harvest <- factor(harvest, levels = c("Harvesting level: 0%", "Harvesting level: 50%", "Harvesting level: 100%"))
scenario <- as.character(lapply(simInfo, function(x) x[3]))
budworm <- as.character(lapply(simInfo, function(x) x[7]))
budworm <- ifelse(budworm == "budwormTrue", T,
                  ifelse(budworm == "budwormFalse", F, NA))

caribouRsFinalDiffDF <- data.frame(df,
                                   scenario = as.factor(scenario),
                                   growth = as.factor(growth),
                                   fire = as.factor(fire),
                                   harvest = as.factor(harvest),
                                   budworm = budworm)
#############################################
save(caribouRsFinalDiffDF, file = "caribouRsFinalDiffDF.RData")
rm(caribouRsFinalDiffDF)
###########################################



## adding tidy treatment factors to final df
simInfo <- strsplit(caribouRsMeanFinalDF$variable, "_")
growth <- as.character(lapply(simInfo, function(x) x[4]))
growth <- ifelse(growth == "growthBaseline", "Growth: baseline", "Growth: projected")
fire <- as.character(lapply(simInfo, function(x) x[5]))
fire <- ifelse(fire == "fireBaseline", "Fire regime: baseline",
               ifelse(fire == "fireProjected", "Fire regime: projected", "No Fires"))
harvest <- as.character(lapply(simInfo, function(x) x[6]))
harvest <- ifelse(harvest == "harvest0", "Harvesting level: 0%",
                  ifelse(harvest == "harvest50", "Harvesting level: 50%", "Harvesting level: 100%"))
harvest <- factor(harvest, levels = c("Harvesting level: 0%", "Harvesting level: 50%", "Harvesting level: 100%"))
scenario <- as.character(lapply(simInfo, function(x) x[3]))
budworm <- as.character(lapply(simInfo, function(x) x[7]))
budworm <- ifelse(budworm == "budwormTrue", T,
                  ifelse(budworm == "budwormFalse", F, NA))

# ##########################
# fire <- factor(rep("No fires", nrow(coverTypesTotal)), levels = c("No fires", "Fire regime: baseline", "Fire regime: projected"))
# fire[grep("BaselineFire", coverTypesTotal$treatment)] <- "Fire regime: baseline"
# fire[grep("ProjectedFire", coverTypesTotal$treatment)] <- "Fire regime: projected"
# growth <- factor(rep("Growth: baseline", nrow(coverTypesTotal)), levels = c("Growth: baseline", "Growth: projected"))
# growth[grep("Growth", coverTypesTotal$treatment)] <- "Growth: projected"
# harvest <- factor(rep("No Harvesting", nrow(coverTypesTotal)), levels = c("No Harvesting", "Harvesting level: 50%", "Harvesting level: 100%"))
# harvest[grep("Harvest", coverTypesTotal$treatment)] <- "Harvesting level: 100%"
# harvest[grep("0.5", coverTypesTotal$treatment)] <- "Harvesting level: 50%"
# ################


caribouRsMeanFinalDF <- data.frame(caribouRsMeanFinalDF,
                                   scenario = as.factor(scenario),
                                   growth = as.factor(growth),
                                   fire = as.factor(fire),
                                   harvest = as.factor(harvest),
                                   budworm = budworm)
#############################################
save(caribouRsMeanFinalDF, file = "caribouRsMeanFinalDF.RData")
rm(caribouRsMeanFinalDF)
###########################################



