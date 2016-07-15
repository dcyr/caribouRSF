# ###
# ### fetch online reference files
# require(RCurl)
# readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
# sppTraits <- read.csv(text = getURL(paste(readURL, "speciesTraits.csv", sep="/")))
# sppSoftwood <- sppTraits[which(sppTraits$HardSoft == "soft"), "LandisCode"]
# sppSoftwood <- as.character(unique(sppSoftwood))
# 
# 
# 
# setwd("/media/dcyr/Seagate Backup Plus Drive/LSJ/baseline/climate_harvest_fire_budworm/1")
# biomassDir <- "./output/biomass"

softHardFnc <- function(biomassDir = "./output/biomass", sppSoftwood, timesteps = "all") {
    require(raster)
    x <- list.files(biomassDir)
    spp <- gsub("bio|[0-9]|_|.tif", "", x) 
    # identifying softwood rasters
    softwoodIndex <- which(spp %in% sppSoftwood)
    # time steps
    ts <- as.numeric(gsub("[^0-9]", "", x))
    if ((timesteps == "all")[1]) {
        uniqueTS <- unique(ts)
        uniqueTS <- uniqueTS[order(uniqueTS)]
    } else {
        uniqueTS <- timesteps
    }
    for (i in uniqueTS) {
        tsIndex <- which(ts == i)
        index <- intersect(softwoodIndex, tsIndex)
        
        # softwood biomass
        y <- stack(paste(biomassDir, x[index], sep = "/"))
        y <- sum(y)
        # total biomass
        total <- x[intersect(grep("TotalBiomass", x), tsIndex)]
        total <- raster(paste(biomassDir, total, sep = "/"))
        y <- (y/total)>0.5
        if (i == uniqueTS[1]) {
            softwood <- y
        } else {
            softwood <- stack(softwood, y)
        }
    }
    names(softwood) <- paste0("sw_", uniqueTS)
    return(softwood)
}
