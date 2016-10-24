##################################################################
##################################################################
#### Dominic Cyr
##################################################################
wwd <- (paste(getwd(), Sys.Date(), sep ="/"))
dir.create(wwd)
setwd(wwd)
##################################################################
# sourcing function, loading libraries
source("../scripts/timeSinceDisturbances.R")
source("../scripts/forestCoverReclass.R")
source("../scripts/biomass.R")
source("../scripts/caribouRSF.R")
source("../scripts/softHard.R")
source("../scripts/simInfo.R")
require(RCurl)
require(raster)
require(parallel)
require(doSNOW)
require(reshape2)

##################################################################
### User defined variables
##################################################################
simRoot <- "/media/dcyr/Data/Sims/LSJ" ## where the simulations are
#simRoot <- "/media/dcyr/Seagate Backup Plus Drive/Sims/LSJ" ## where the simulations are
### might have to specify working directory if you don't run the script from the command line
# setwd("/home/dcyr/Travail/SCF/CBFA/caribou")
area <- "LSJ"
timesteps <- c(0,100)#"all"#c(0, 50, 100, 150, 200, 250, 300)#"all"# ### can either be 'all' or a vector of user-selected timesteps
ncores <- floor(0.9*detectCores())  ### will use 90% of cores, rounded to the lower integer
### ... unless that is 0, then it is set to 1
ncores <- ifelse(ncores == 0, 1, ncores) 
##################################################################


### fetch online reference files (spp list, contains softwood/hardwood info)
### May have to be modified
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
sppTraits <- read.csv(text = getURL(paste(readURL, "speciesTraits.csv", sep="/")))
sppSoftwood <- sppTraits[which(sppTraits$HardSoft == "soft"), "LandisCode"]
sppSoftwood <- as.character(unique(sppSoftwood)) # list of softwood spp (all others are considered hardwoods)

### loading initial conditions & other static inputs
# initial time since fire (TSF)
initTSF <- raster("../initRasters/initTSF.tif")
# initial time since harvest (TSH)
initTSH <- raster("../initRasters/initTSH.tif")
areaExtent <- raster("../initRasters/areaExtentCaribou.tif")
ecodistricts <- raster("../gis/ecodistricts.tif")
initTSF[is.na(areaExtent)] <- NA
initTSH[is.na(areaExtent)] <- NA


### fetching sim info (folder path, treatments, scenarios, etc...)
simInfo <- simInfoFnc(simRoot)
simDir <- simInfo$simDir



### initiating cluster
sysName <- Sys.info()["sysname"]
if (sysName=="Windows") {
    cl = makeCluster(ncores, rscript="Rscript.exe", type='SOCK')
}
if (sysName=="Linux") {
    cl = makeCluster(ncores)
}
registerDoSNOW(cl)
### looping through sim folders
coverTypes <- foreach(i=seq_along(simDir), .combine="rbind") %dopar%  {
    require(reshape2)
    a <- area[i]
    s <- as.character(simInfo$scenario[i])
    t <- as.character(simInfo$treatment[i])
    t <- gsub("_", "-", t)
    r <- as.numeric(simInfo$replicate[i])
    
    ### simulation folder
    simFolder <- paste0(simRoot, gsub("\\.", "", simDir[i]))
    
    ### fetching biomass info
    biomass <- biomassFnc(biomassDir = paste(simFolder, "output/biomass", sep = "/"),
                          crsRef = areaExtent, timesteps = timesteps)
    # replace "all" by numerical vector
    timesteps <- as.numeric(gsub("[^0-9]", "", names(biomass)))
    
    ### Softwood / Hardwoods
    softwoods <- softHardFnc(biomassDir = paste(simFolder, "output/biomass", sep = "/"), sppSoftwood,
                             crsRef = areaExtent, timesteps = timesteps)
    hardwoods <- softwoods == 0
    names(hardwoods) <- gsub("s", "h", names(softwoods))
    ### fetching age info
    outputDirs <- basename(list.dirs(simFolder, recursive = F))
    # tsf
    if ("fire" %in% outputDirs) {
        fireDir <- paste(simFolder, "fire", sep = "/")
    } else {
        fireDir <- NULL
    }
    tsf <- tsfFnc(fireDir = fireDir,
                      initTSF = initTSF, timesteps = timesteps)
    # tsh
    if ("harvest" %in% outputDirs) {
        harvDir <- paste(simFolder, "harvest", sep = "/")
    } else {
        harvDir <- NULL
    }
    tsh <- tshFnc(harvDir = harvDir,
                      initTSH = initTSH, timesteps = timesteps)

    maxAge <- maxAgeFnc(maxAgeFolder = paste(simFolder, "output/cohort-stats", sep = "/"),
                        crsRef = areaExtent, timesteps = timesteps)
    #
    age <- coverAgeFnc(tsf, tsh, cohortAgeMax = maxAge)
    
    ### age reclassification matrix
    m <- c(0, 10, 1,
           10, 50, 2,
           50, 90, 3,
           90, Inf, 4)
    m <- matrix(m, ncol = 3, byrow = T)
    ageClass <- reclassify(age, rcl = m, include.lowest = T, right = F)
    tsfClass <- reclassify(tsf, rcl = m, include.lowest = T, right = F)
    tshClass <- reclassify(tsh, rcl = m, include.lowest = T, right = F)
    rm(list = c("age", "maxAge"))

    ### age class 0-9 
    # fire
    fire0_9 <- tsfClass == 1 
    
        
    # harvest
    harvest0_9 <- tshClass == 1
    harvest0_9[fire0_9] <- 0
    
    ### age class 10-49
    # fire
    fire10_49 <- tsfClass == 2
    fire10_49[tsh < tsf] <- 0
    
    # harvest
    harvest10_49 <- tshClass == 2
    harvest10_49[tsf <= tsh] <- 0
    
    rm(list = c("tsf", "tsh"))
    # other
    other10_49 <- ageClass == 2
    other10_49[fire10_49] <- 0
    other10_49[harvest10_49] <- 0
    
    # cover type
    decidHarv10_49 <- conifHarv10_49 <- harvest10_49
    rm("harvest10_49")
    decidHarv10_49[softwoods] <- 0
    conifHarv10_49[decidHarv10_49] <- 0
    decidOther10_49 <- other10_49
    rm("other10_49")
    decidOther10_49[softwoods] <- 0
    ### age class 50-89
    decid50_89 <- conif50_89 <- ageClass == 3
    conif50_89[hardwoods] <- 0
    decid50_89[softwoods] <- 0
    ### age class 90+
    conif90 <- ageClass == 4
    conif90[hardwoods] <- 0

    df <- list()
    ### computing zonal statistics
    df[["fire0_9"]] <- data.frame(zonal(fire0_9, ecodistricts, "sum"), variable = "fire0_9")
    df[["harvest0_9"]] <- data.frame(zonal(harvest0_9, ecodistricts, "sum"), variable = "harvest0_9")
    df[["fire10_49"]] <- data.frame(zonal(fire10_49, ecodistricts, "sum"), variable = "fire10_49")
    df[["decidHarv10_49"]] <- data.frame(zonal(decidHarv10_49, ecodistricts, "sum"), variable = "decidHarv10_49")
    df[["conifHarv10_49"]] <- data.frame(zonal(conifHarv10_49, ecodistricts, "sum"), variable = "conifHarv10_49")
    df[["decidOther10_49"]] <- data.frame(zonal(decidOther10_49, ecodistricts, "sum"), variable = "decidOther10_49")
    df[["conif50_89"]] <- data.frame(zonal(conif50_89, ecodistricts, "sum"), variable = "conif50_89")
    df[["decid50_89"]] <- data.frame(zonal(decid50_89, ecodistricts, "sum"), variable = "decid50_89")
    df[["conif90"]] <- data.frame(zonal(conif90, ecodistricts, "sum"), variable = "conif90")
    #
    df <- do.call("rbind", df)
    
    df <- melt(df, id = c("zone", "variable"), variable.name = "timestep")
    df$timestep <- timesteps[as.numeric(df$timestep)]
    
    df <- data.frame(area = a,
                     scenario = s,
                     treatment = t,
                     replicate  = r,
                     ecodistricts = df$zone,
                     coverType = df$variable,
                     timestep = df$timestep,
                     area_ha = df$value*6.25)
    return(df)
}
stopCluster(cl)
save(coverTypes, file = "caribou_coverType_ecodistrict.RData")
