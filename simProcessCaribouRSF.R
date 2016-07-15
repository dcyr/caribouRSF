##################################################################
##################################################################
#### A script to prepare raster
#### for non forested cover types
#### 
#### Dominic Cyr
##################################################################

##################################################################
### User defined variables
##################################################################
simRoot <- "/media/dcyr/Data/Sims/LSJ" ## where the simulations are
### might have to specify working directory if you don't run the script from the command line
# setwd("/home/dcyr/Travail/SCF/CBFA/caribou")
area <- "LSJ"
timesteps <- "all"# ### can either be 'all' or a vector of user-selected timesteps
ncores <- floor(0.9*detectCores())  ### will use 90% of cores, rounded to the lower integer
### ... unless that is 0, then it is set to 1
ncores <- ifelse(ncores == 0, 1, ncores) 
##################################################################


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
initTSF[is.na(areaExtent)] <- NA
initTSH[is.na(areaExtent)] <- NA
# other static inputs (roads and non-forest cover types)
roadsDecayDist <- raster("../initRasters/roadsDecayingDist.tif")
roadsDecayDist <- roadsDecayDist
waterAndNonFor <- raster("../initRasters/waterAndNonFor.tif")
wetlands <- raster("../initRasters/wetlands.tif")


### fetching sim info (folder path, treatments, scenarios, etc...)
simInfo <- simInfoFnc(simRoot)
simDir <- simInfo$simDir

for (i in seq_along(simDir)) {
    print("#################################################################")
    print("#################################################################")
    print(paste("Processing simulation", i, "of", length(simDir)))
    print("#################################################################")
    print("#################################################################")
    print("Preparing inputs for Caribou RSF")
    t1 <- Sys.time()
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

    ### Some tidying up necessary to eliminate a few remaining aberration 
    ### due some Landis active site being considered as wetlands
    ###
    # replaced some active stands by wetlands (simpler than the opposite)
    conif90[wetlands] <- 0
    conif50_89[wetlands] <- 0
    decid50_89[wetlands] <- 0
    rm("ageClass")
    
    print(paste("Computing RSF using", ncores, "cores (may take up to several minutes by timestep)"))
    f <-  function(x) {param[1] +
            param[2]*x[1] +
            param[3]*x[2] +
            param[4]*x[3] +
            param[5]*x[4] +
            param[6]*x[5] +
            param[7]*x[6] +
            param[8]*x[7] +
            param[9]*x[8] +
            param[10]*x[9] +
            param[11]*x[10] +
            param[12]*x[11] +
            param[13]*x[12] +
            param[14]*x[13] +
            param[15]*x[14] +
            param[16]*(x[12]*x[13]) +
            param[17]*(x[12]*x[14])}
    
    param <- c(-0.3263,
               -0.5202, 
               -0.6377,
               -0.0699,
               -3.6234,
               -2.1789,
               -0.2291,
               -0.6255,
               0.1171,
               0.0061,
               0.0732,
               -0.5349,
               0.7727,
               0.0461,
               -0.0004,
               -0.0733,
               0.0006)
    
    caribouRS <- caribouRSF(fire0_9 = fire0_9,
                            harvest0_9 = harvest0_9,
                            fire10_49 = fire10_49,
                            decidHarv10_49 = decidHarv10_49,
                            conifHarv10_49 = conifHarv10_49,
                            decidOther10_49= decidOther10_49,
                            conifOther = NULL,
                            decid50_89 = decid50_89,
                            conif50_89 = conif50_89,
                            conif90 = conif90,
                            wetlands = wetlands,
                            roadsDecayDist = roadsDecayDist,
                            waterAndNonFor = waterAndNonFor,
                            biomass = biomass,
                            ncores = ncores)
    names(caribouRS) <- paste("caribouRS", timesteps, sep = "_")
    fName <- paste0("caribouRS_", a, "_", s, "_", t, "_", r, ".RData")
    print(paste0("Writing outputs to file: ", paste(getwd(), fName, sep ="/")))
    save(caribouRS, file = fName)
    rm(list = c("caribouRS", "fire0_9", "harvest0_9", "decidHarv10_49", "conifHarv10_49", "decidOther10_49",
                "decid50_89", "conif50_89", "conif90", "wetlands", "roadsDecayDist", "waterAndNonFor",
                "biomass"))
    print("Removing temporary raster files")
    removeTmpFiles(h=0.1)
    t2 <- Sys.time()
    print(round(t2-t1, 2))
}