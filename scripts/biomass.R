
biomassFnc <- function(biomassDir = "./output/biomass", timesteps = "all", crsRef = NULL) {
    require(raster)
    x <- list.files(biomassDir)
    # identifying total biomass rasters
    x  <- x[grep("TotalBiomass", x)]
    # time steps
    ts <- as.numeric(gsub("[^0-9]", "", x))
    # reordering files
    x <- x[order(ts)]
    if (!(timesteps == "all")[1]) {
        ts <- as.numeric(gsub("[^0-9]", "", x))
        x <- x[which(ts %in% timesteps)]
    } 
    totalBiomass <- stack(paste(biomassDir, x, sep = "/"))
    # convert g/sq-meter in tons/ha
    totalBiomass <- totalBiomass/100
    
    if(!is.null(crsRef)) {
        extent(totalBiomass) <- extent(crsRef)
        crs(totalBiomass) <- crs(crsRef)
    }
    return(totalBiomass)
}
