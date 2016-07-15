
### caribou resource selection function
caribouRSF <- function(fire0_9 = fire0_9,
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
                       ncores = NULL
                       ) {
    require(raster)
    require(parallel)
    
    biomassSq <- biomass^2
    beginCluster(ncores)
    for (i in 1:nlayers(biomass)) {
        x <- stack(fire0_9[[i]], #1
                   harvest0_9[[i]], #2
                   fire10_49[[i]], #3
                   decidHarv10_49[[i]], #4
                   decidOther10_49[[i]], #5
                   conifHarv10_49[[i]], #6
                   decid50_89[[i]], #7
                   conif50_89[[i]], #8
                   conif90[[i]], #9
                   wetlands, #10
                   waterAndNonFor, #11
                   roadsDecayDist, #12
                   biomass[[i]], #13
                   biomassSq[[i]] #14
                   )
        
        x[is.na(x)] <- 0
        x[is.na(areaExtent)] <- NA
 
        # function 'f' and 'param' must be present in the global environment
        caribouRsProbLogit <- clusterR(x, calc, args=list(fun=f), export='param')
        
        x <- exp(caribouRsProbLogit)/(1+exp(caribouRsProbLogit))
        if (i == 1) {
            caribouRsProb <- x   
        } else {
            caribouRsProb <- stack(caribouRsProb, x)
        }
    }
    endCluster()
    return(caribouRsProb)
}
