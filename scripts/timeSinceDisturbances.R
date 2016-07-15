
### A function that computes time since fire for all time steps from Landis outputs
tsfFnc <- function(fireDir = "./fire", initTSF, timesteps = "all", defaultTS = 10) {
    require(raster)
    tsf <- initTSF

    if(!is.null(fireDir)) {
        x <- list.files(fireDir)
        x <- x[grep("severity", x)]
        # reordering files based on time steps
        ts <- as.numeric(gsub("[^0-9]", "", x))
        x <- x[order(ts)]
        ts <- ts[order(ts)]
    } else {
        if(is.numeric(timesteps)) {
            ts <- seq(defaultTS, max(timesteps), defaultTS)
        } else {
            stop("No explicit timesteps provided with no output folder to infer them.")
        }
    }
    if(identical(timesteps, "all")) {
        timesteps <- c(0, ts)
    }
    index <- which(ts<=max(timesteps))

    delta <- diff(c(0,ts[index]))
    
    if(!identical(0, timesteps)) { ## skip if only initTSF is needed
        if(!is.null(fireDir)) {
            # stacking fire severity rasters
            x <- stack(paste(fireDir, x[index], sep="/"))
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
            }
        } else { ## compute tsf for all required timesteps in absence of fire outputs
            for (i in timesteps[timesteps!=0]) {
                tsf <- stack(tsf, tsf[[1]]+i)
            }
        }
            

    }
    names(tsf) <- paste0("tsf_", timesteps)
    return(tsf)
}
 

### A function that computes time since harvesting for all time steps from Landis outputs
### At this time, any prescription is counted as 'stand initiating'
tshFnc <- function(harvDir = "./harvest", initTSH, timesteps = "all", defaultTS = 10) {
    require(raster)
    tsh <- initTSH
    if(!is.null(harvDir)) {
        x <- list.files(harvDir)
        x <- x[grep("prescript", x)]
        # reordering files based on time steps
        ts <- as.numeric(gsub("[^0-9]", "", x))
        x <- x[order(ts)]
        ts <- ts[order(ts)]
    } else {
        if(is.numeric(timesteps)) {
            ts <- seq(defaultTS, max(timesteps), defaultTS)
        } else {
            stop("No explicit timesteps provided with no output folder to infer them.")
        }
    }
    if(identical(timesteps, "all")) {
        timesteps <- c(0, ts)
    }
    index <- which(ts<=max(timesteps))
    
    delta <- diff(c(0,ts[index]))
   
    if(!identical(0, timesteps)) { ## skip if only initTSF is needed
        if(!is.null(harvDir)) {
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
            }
        } else { ## compute tsh for all required timesteps in absence of harvest outputs
            for (i in timesteps[timesteps!=0]) {
                tsh <- stack(tsh, tsh[[1]]+i)
            }
        }
    }
    names(tsh) <- paste0("tsh_", timesteps)
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
    if(!is.null(tsf)) {
        tsFire <- as.numeric(gsub("[^0-9]", "", names(tsf)))
    }
    if(!is.null(tsh)) {
        tsHarv <- as.numeric(gsub("[^0-9]", "", names(tsh)))
    }
    tsAge <- as.numeric(gsub("[^0-9]", "", names(cohortAgeMax)))
    ts <- as.numeric(gsub("[^0-9]", "", names(cohortAgeMax) ))
    for (i in seq_along(ts)) {
        y <-  cohortAgeMax[[i]]
        if(!is.null(tsf)) {
            y <- stack(y, tsf[[i]])
        }
        if(!is.null(tsh)) {
            y <- stack(y, tsh[[i]])
        }
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

