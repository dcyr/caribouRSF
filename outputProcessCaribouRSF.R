##################################################################
##################################################################
#### Dominic Cyr - Sig Prep
setwd("/home/dcyr/Travail/SCF/CBFA/caribou")
##################################################################
wwd <- (paste(getwd(), Sys.Date(), sep ="/"))
dir.create(wwd)
setwd(wwd)
##################################################################
# sourcing function, loading libraries
require(raster)
require(reshape2)

outputDir <- "../outputs"
outputFiles <- list.files(outputDir)
simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")


# loading raster(s) for zonal statistics
ecodistricts <- raster("../gis/ecodistricts.tif")

for (i in seq_along(outputFiles)) {
    a <- simInfo[[i]][2]
    s <- simInfo[[i]][3]
    t <- simInfo[[i]][4]
    r <- as.numeric(simInfo[[i]][5])
    
    ### loading raster stack
    caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))
    
    ### computing zonal averages
    x <- as.data.frame(zonal(caribouRS, ecodistricts, fun = 'mean'))
    x <- melt(x, id = "zone", variable.name = "time", value.name = "caribouRS_mean")
    x$time <- as.numeric(gsub("[^0-9]", "", x$time))

    x <- data.frame(area = a,
                    scenario = s,
                    treatment = t,
                    replicate = r,
                    x)
    x <- caribouRS_ecodist_mean
    
}
    
### rewrite when data.frame is complete
require(ggplot2)   
    

ggplot(x, aes(x = time, y = caribouRS_mean, group = zone, col = zone)) +
    geom_line()



### high quality habitat
### defined as 4th (best) quartile of initial conditions

qualityThresh <- quantile(values(caribouRS[[1]]), 0.75, na.rm =T)

