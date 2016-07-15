
### A function that computes time since fire for all time steps from Landis outputs
tsfFnc <- function(fireDir = "./fire", initTSF, timesteps = "all") {
    require(raster)
    x <- list.files(fireDir)
    x <- x[grep("severity", x)]
    # reordering files based on time steps
    ts <- as.numeric(gsub("[^0-9]", "", x))
    x <- x[order(ts)]
    ts <- ts[order(ts)]
    #
    delta <- diff(c(0,ts))
    tsf <- initTSF
    # stacking fire severity rasters
    x <- stack(paste(fireDir, x, sep="/"))
    # setting extent, and CRS
    extent(x) <- extent(tsf)
    crs(x) <- crs(tsf)
    # sequentially compute time since fire
    for (i in 1:nlayers(x)) {
        y <- tsf[[i]] + delta[i]
        y[x[[i]]>1] <- 0
        tsf <- stack(tsf, y)
    }
    ts <- c(0, ts)
    if ((timesteps != "all")[1]) {
        tsf <- tsf[[which(ts %in% timesteps)]]
        ts <-  timesteps
    }
    names(tsf) <- paste0("tsf_", ts)
    return(tsf)
}
 

### A function that computes time since harvesting for all time steps from Landis outputs
### At this time, any prescription is counted as 'stand initiating'
tshFnc <- function(harvDir = "./harvest", initTSH, timesteps = "all") {
    require(raster)
    x <- list.files(harvDir)
    x <- x[grep("prescript", x)]
    # reordering files based on time steps
    ts <- as.numeric(gsub("[^0-9]", "", x))
    x <- x[order(ts)]
    ts <- ts[order(ts)]
    #
    delta <- diff(c(0,ts))
    tsh <- initTSH
    # stacking harvest prescription rasters
    x <- stack(paste(harvDir, x, sep="/"))
    # setting resolution, extent, and CRS
    extent(x) <- extent(tsh)
    crs(x) <- crs(tsh)
    # sequentially compute time since last harvest
    for (i in 1:nlayers(x)) {
        y <- tsh[[i]] + delta[i]
        y[x[[i]]>1] <- 0
        tsh <- stack(tsh, y)
    }
    ts <- c(0, ts)
    if ((timesteps != "all")[1]) {
        tsh <- tsh[[which(ts %in% timesteps)]]
        ts <-  timesteps
    }
    names(tsh) <- paste0("tsh_", ts)
    return(tsh)
}
 

### A function that fetch and format the maximum cohort age
maxAgeFnc <- function(maxAgeFolder = "./output/cohort-stats", crsRef = NULL, timesteps = "all") {
    require(raster)
    x <- list.files(maxAgeFolder)
    x <- x[grep("AGE-MAX", x)]
    # reordering files based on time steps
    ts <- as.numeric(gsub("[^0-9]", "", x))
    x <- x[order(ts)]
    ts <- ts[order(ts)]
    if ((timesteps != "all")[1]) {
        x <- x[which(ts %in% timesteps)]
        ts <-  timesteps
    }
    # stacking harvest prescription rasters
    x <- stack(paste(maxAgeFolder, x, sep="/"))
    # setting resolution, extent, and CRS
    extent(x) <- extent(crsRef)
    crs(x) <- crs(crsRef)
    cohortAgeMax <- x
    cohortAgeMax[] <- values(x) # just to tidy up the original encoding 
    names(cohortAgeMax) <- paste0("cohortAgeMax_", ts)
    return(cohortAgeMax)
}

### A function that computes the time since last disturbance
coverAgeFnc <- function(tsf, tsh, cohortAgeMax) {
    require(raster)
    # fetching time steps
    tsFire <- as.numeric(gsub("[^0-9]", "", names(tsf)))
    tsHarv <- as.numeric(gsub("[^0-9]", "", names(tsh)))
    tsAge <- as.numeric(gsub("[^0-9]", "", names(cohortAgeMax)))
    if (!all.equal(tsFire, tsHarv, tsAge)) {
        stop("Time steps don't match")
    }
    ts <- as.numeric(gsub("[^0-9]", "", names(cohortAgeMax) ))
    for (i in seq_along(ts)) {
        y <- stack(tsf[[i]], tsh[[i]], cohortAgeMax[[i]])
        y <- min(y, na.rm = TRUE)
        if (i == 1) {
            ageMax <- y
        } else {
            ageMax <- stack(ageMax, y)
        }
    }
    names(ageMax) <- paste0("ageMax_", ts)
    return(ageMax)
}

