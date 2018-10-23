##################################################################
##################################################################
#### Dominic Cyr
rm(list =ls())
setwd("/home/dcyr/Travail/SCF/CBFA/caribou")
##################################################################
wwd <- (paste(getwd(), Sys.Date(), sep ="/"))
dir.create(wwd)
setwd(wwd)
##################################################################

########################################################################################
########################################################################################
### computing ecodistrict averages, storing into tidy data frame
require(doSNOW)
require(raster)
require(reshape2)
outputDir <- ifelse(Sys.info()["nodename"] == "dcyr-ThinkPad-X220",
                    "/media/dcyr/Seagate Backup Plus Drive/Sims/caribouRsOutputs/",
                    "/media/dcyr/Data/Sims/caribouRsOutputs/")
outputFiles <- list.files(outputDir)
simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")
ecodistricts <- raster("../gis/ecodistricts.tif")



######################################################################################
######################################################################################
nClusters <- 10 ## 10 treads max with 32Gb of RAM
cl <- makeCluster(nClusters, outfile = "")
registerDoSNOW(cl)
caribouRS_ecodist_mean <- foreach(i = seq_along(outputFiles), .combine = "rbind")  %dopar% {#
    require(raster)
    require(reshape2)
    a <- simInfo[[i]][2]
    s <- simInfo[[i]][3]
    t <- simInfo[[i]][4]
    r <- as.numeric(simInfo[[i]][5])

    ### loading raster stack
    caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))

    v <- values(caribouRS)
    ecoValues <- values(ecodistricts)

    threshold <- quantile(v[,1], c(0.75, 0.90), na.rm = T)
    npixel <- freq(ecodistricts)
    npixel <- npixel[complete.cases(npixel),]
    
    ### computing zonal averages
    x <- as.data.frame(zonal(caribouRS, ecodistricts, fun = 'mean'))
    names(x)[1] <- "ecodistrict"
    ### computing proportions of HQH
    h75 <- apply(v, 2, function(x) by(x>threshold["75%"], ecoValues, mean))
    h75 <- data.frame(ecodistrict = as.numeric(rownames(h75)), h75)
    #
    h90 <- apply(v, 2, function(x) by(x>threshold["90%"], ecoValues, mean))
    h90 <- data.frame(ecodistrict = as.numeric(rownames(h90)), h90)
    ### melting and merging
    x <- melt(x, id = "ecodistrict", variable.name = "time", value.name = "caribouRS_mean")
    h75 <- melt(h75, id = "ecodistrict", variable.name = "time", value.name = "h75")
    x <- merge(x, h75)
    h90 <- melt(h90, id = "ecodistrict", variable.name = "time", value.name = "h90")
    x <- merge(x, h90)
    
    
    x$time <- as.numeric(gsub("[^0-9]", "", x$time))
    x <- data.frame(area = a,
                    scenario = s,
                    treatment = t,
                    replicate = r,
                    x)
    print(paste(i, "of", length(outputFiles)))
    return(x)
}
stopCluster(cl)
### a little tyding up before saving to file
caribouRS_ecodist_mean$ecodistrict <- as.factor(caribouRS_ecodist_mean$ecodistrict)
caribouRS_ecodist_mean$replicate <- as.factor(caribouRS_ecodist_mean$replicate)
df <- caribouRS_ecodist_mean
fire <- factor(rep("No fires", nrow(df)), levels = c("No fires", "Fire regime: baseline", "Fire regime: projected"))
fire[grep("ProjectedFire", df$treatment)] <- "Fire regime: projected"
fire[grep("BaselineFire", df$treatment)] <- "Fire regime: baseline"
    
growth <- factor(rep("Growth: baseline", nrow(df)), levels = c("Growth: baseline", "Growth: projected"))
growth[grep("Growth", df$treatment)] <- "Growth: projected"
harvest <- factor(rep("Harvesting level: 0%", nrow(df)), levels = c("Harvesting level: 0%", "Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("Harvest", df$treatment)] <- "Harvesting level: 100%"
harvest[grep("0.5", df$treatment)] <- "Harvesting level: 50%"
budworm <- factor(rep("No budworm", nrow(df)), levels = c("No budworm", "Budworm"))
budworm[grep("Budworm", df$treatment)] <- "Budworm"

caribouRS_ecodist_mean <- data.frame(caribouRS_ecodist_mean, fire, harvest, growth, budworm)
########################################################################################
save(caribouRS_ecodist_mean, file = "caribouRS_ecodist_mean_DF.RData")
########################################################################################
########################################################################################


########################################################################################
########################################################################################
### computing ecodistrict averages, storing into tidy data frame
require(doSNOW)
require(raster)
require(reshape2)

outputFiles <- list.files(outputDir)
simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")
ecodistricts <- raster("../gis/ecodistricts.tif")
########################################################################################
nClusters <- 10 ## takes less memory than for ecodistricts
cl <- makeCluster(nClusters, outfile = "")
registerDoSNOW(cl)

caribouRS_mean <- foreach(i = seq_along(outputFiles), .combine = "rbind")  %dopar% {#
    require(raster)
    require(reshape2)
    a <- simInfo[[i]][2]
    s <- simInfo[[i]][3]
    t <- simInfo[[i]][4]
    r <- as.numeric(simInfo[[i]][5])

    ### loading raster stack
    caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))
    time <- as.numeric(gsub("[^0-9]", "", names(caribouRS)))
    x <- list()
    for (l in 1:nlayers(caribouRS)) {
        v <- values(caribouRS[[l]])
        v <- v[!is.na(v)]
        if(l == 1) {
            threshold <- quantile(v, c(0.75, 0.90))
            npixel <- length(v)
        }

        x[[l]] <- data.frame(area = a,
                        scenario = s,
                        treatment = t,
                        time = time[l],
                        replicate = r,
                        caribouRS_mean = mean(v),
                        h75= sum(v>threshold[1])/npixel,
                        h90 = sum(v>threshold[2])/npixel)

    }
    x <- do.call("rbind", x)
    print(paste(i, "of", length(outputFiles)))
    return(x)
}
stopCluster(cl)
### a little tyding up before saving to file

caribouRS_mean$replicate <- as.factor(caribouRS_mean$replicate)
df <- caribouRS_mean

fire <- factor(rep("No fires", nrow(df)), levels = c("No fires", "Fire regime: baseline", "Fire regime: projected"))
fire[grep("ProjectedFire", df$treatment)] <- "Fire regime: projected"
fire[grep("BaselineFire", df$treatment)] <- "Fire regime: baseline"

growth <- factor(rep("Growth: baseline", nrow(df)), levels = c("Growth: baseline", "Growth: projected"))
growth[grep("Growth", df$treatment)] <- "Growth: projected"
harvest <- factor(rep("Harvesting level: 0%", nrow(df)), levels = c("Harvesting level: 0%", "Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("Harvest", df$treatment)] <- "Harvesting level: 100%"
harvest[grep("0.5", df$treatment)] <- "Harvesting level: 50%"
budworm <- factor(rep("No budworm", nrow(df)), levels = c("No budworm", "Budworm"))
budworm[grep("Budworm", df$treatment)] <- "Budworm"
caribouRS_mean <- data.frame(caribouRS_mean, fire, harvest, growth, budworm)

########################################################################################
save(caribouRS_mean, file = "caribouRS_mean_DF.RData")
########################################################################################
########################################################################################








