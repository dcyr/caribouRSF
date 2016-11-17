rm(list = ls())
setwd("~/Travail/SCF/CBFA/caribou")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
# require(ggmap)
require(rgdal)
require(dplyr)
# require(RColorBrewer)
require(reshape)
# require(maptools)
# require(ggplot2)

ecodistricts <- raster("../gis/ecodistricts.tif")

ecodistrictAreaHa <- as.data.frame(zonal(is.na(ecodistricts)==F, ecodistricts, sum)) %>%
    mutate(ecodistricts = zone,
           areaEcodistrict_ha = value * 6.25) %>%
    select(ecodistricts, areaEcodistrict_ha)
areaTotal <- sum(ecodistrictAreaHa$areaEcodistrict_ha)


coverTypes <- get(load("../processedOutputs/caribou_coverType_ecodistrict.RData"))
coverTypes <- merge(coverTypes, ecodistrictAreaHa)

coverTypes <- coverTypes %>%
    mutate(propEcodistrict = area_ha / areaEcodistrict_ha)


##############################################################################
##############################################################################
########  cover types

### initial conditions
coverTypesInit <- coverTypes %>%
    filter(scenario == "baseline",
           treatment == "BudwormBaselineFireHarvest",
           timestep == 0,
           replicate == 1) %>%
    select(ecodistricts, coverType, area_ha, propEcodistrict)

coverTypesInitTotal <- coverTypesInit %>%
    group_by(coverType) %>%
    summarise(areaTotal_ha = sum(area_ha)) %>%
    mutate(propTotal = areaTotal_ha / areaTotal)


### summary statistics (for Table 1)
# # as.data.frame(coverTypesInitTotal (forest cover types, except "conifOther10-49")
# wetlands <- raster("../initRasters/wetlands.tif")
# plot(wetlands)
# sum(values(wetlands), na.rm =T)*6.25/areaTotal
# waterAndNonFor <- raster("../initRasters/waterAndNonFor.tif")
# plot(waterAndNonFor)
# sum(values(waterAndNonFor), na.rm =T)*6.25/areaTotal   
# nearestRoad <- raster("../initRasters/intermediate/dNearestRoad.tif")
# biomassPerEcodistrict <- get(load(file.choose()))
# summary(biomassPerEcodistrict)
# foo <- biomassPerEcodistrict %>%
#     filter(Time == 0, Scenario == "baseline", Replicate == 1, Treatment == "BudwormBaselineFireHarvest") %>%
#     group_by(Species) %>%
#     summarise(totalBiomass = sum(biomassEcodistrictTotal_Tons),
#               totalArea = sum(areaEcodistrictTotal_ha)) %>%
#     summarise(meanBiomass = sum(totalBiomass/totalArea))


### creating data.frames with final and initial conditions
coverTypes <- coverTypes %>%
    filter(timestep != 0)

coverTypes <- merge(coverTypes, coverTypesInit,
                    by = c("ecodistricts", "coverType"),
                    suffixes = c("", ".init"))

coverTypesTotal <- coverTypes %>%
    group_by(scenario, treatment, timestep, coverType, replicate) %>%
    summarise(areaTotal_ha = sum(area_ha)) %>%
    mutate(propTotal = areaTotal_ha / areaTotal) %>%
    group_by(scenario, treatment, timestep, coverType) %>%
    summarise(areaTotal_ha = mean(areaTotal_ha),
              propTotal = mean(propTotal)) 

coverTypes <- coverTypes %>%
    group_by(scenario, treatment, ecodistricts, coverType) %>%
    summarise(propEcodistrictDiff = mean(propEcodistrict - propEcodistrict.init))

coverTypesTotal <- merge(coverTypesTotal, coverTypesInitTotal, by = "coverType", suffixes = c("", ".init"))
 
coverTypesTotal <- coverTypesTotal %>%
    mutate(propTotalDiff = propTotal - propTotal.init)

### creating individual factors  for treatments + colors

# total area
fire <- factor(rep("Fire regime: baseline", nrow(coverTypesTotal)), levels = c("Fire regime: baseline", "Fire regime: projected"))
fire[grep("Projected", coverTypesTotal$treatment)] <- "Fire regime: projected"
harvest <- factor(rep("Harvesting level: 100%", nrow(coverTypesTotal)), levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("0.5", coverTypesTotal$treatment)] <- "Harvesting level: 50%"
coverTypesTotal <- data.frame(coverTypesTotal, fire, harvest)

# by ecodistrict
fire <- factor(rep("Fire regime: baseline", nrow(coverTypes)), levels = c("Fire regime: baseline", "Fire regime: projected"))
fire[grep("Projected", coverTypes$treatment)] <- "Fire regime: projected"
harvest <- factor(rep("Harvesting level: 100%", nrow(coverTypes)), levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("0.5", coverTypes$treatment)] <- "Harvesting level: 50%"
coverTypes <- data.frame(coverTypes, fire, harvest)

coverTypes$fire




xRange <- 100*ceiling(max(abs(range(coverTypesTotal$propTotalDiff)))*20)/20
xRange <- c(-xRange, xRange)
# defining colors
col1 <- col2rgb("#4477AA")/255 # nice blue
col2 <- col2rgb("white")/255 
col3 <- col2rgb("#BB4444")/255 # nice red


coverNames <- c(fire0_9 = "fire 0-9",
                harvest0_9 = "harv 0-9",
                fire10_49 = "fire 10-49",
                conifHarv10_49 = "conif harv 10-49",
                decidHarv10_49 = "decid harv 10-49",
                decidOther10_49 = "decid other 10-49",
                conif50_89 = "conif 50-89",
                decid50_89 = "decid 50-89",
                conif90 = "conif 90+")           

    
n <- length(unique(paste(coverTypesTotal$fire, coverTypesTotal$scenario)))


png(filename = paste0("coverTypeDiff.png"),
    width = 500 + ((n-1)/2)*500, height = 1200, units = "px", res = 300, pointsize = 6,
    bg = "white")

w <- c(1, rep(0.5, (n-1)))

layout(matrix(seq(1,n), 1, n, byrow = TRUE), 
       widths = w, heights = 1 )

for (s in c("baseline", "RCP26", "RCP45", "RCP85")) {
    require(plotrix)
    
    df <- coverTypesTotal %>%
        filter(scenario == s) %>%
        select(coverType, harvest, fire, propTotalDiff)
    df <- droplevels(df)
    
    fireLevels <- rev(unique(df$fire))

    
    for (f in seq_along(fireLevels)) {
        
        df2 <- df %>%
            filter(fire  == fireLevels[f]) %>%
            select(coverType, harvest, propTotalDiff)
        df2 <- unstack(df2, propTotalDiff ~ harvest)
        df2 <- df2 *100
        rownames(df2) <- unique(df$coverType)
        df2 <- df2[names(coverNames),]
        # colors
        colors <- as.matrix(df2)
        colors[df2<=0] <- color.scale(df2[df2<=0], c(col1[1], col2[1]), c(col1[2], col2[2]), c(col1[3], col2[3]), xrange = c(xRange[1], 0))
        colors[df2>=0] <- color.scale(df2[df2>=0], c(col2[1], col3[1]), c(col2[2], col3[2]), c(col2[3], col3[3]), xrange = c(0, xRange[2]))
    
        
        library(plotrix)

        if (f == 1) {
            if (s == "baseline") {
                par(mar = c(1, 13, 9, 1.25))
            } else {
                par(mar = c(1, 1, 9, 0.25))
            }
            
        } else {
            par(mar = c(1, 0.5, 9, 1.25))
        }
        color2D.matplot(df2, 
                        show.values = 1,
                        axes = F,
                        show.legend = F,
                        xlab = "",
                        ylab = "",
                        border = "white",
                        vcex = 1.5,
                        vcol = "black",
                        #extremes = c("red", "yellow")),
                        cellcolors = colors,
                        bty = "n")
        box(lwd=3, col = "white")
        axis(3, at = seq_len(ncol(df2)) - 0.5,
             labels = c("50%","100%"), tick = FALSE, cex.axis = 1.5, col = "white")
        if(f == 1 & s == "baseline") {
            axis(2, at = seq_len(nrow(df2)) -0.5,
                 labels = rev(coverNames), tick = FALSE, las = 1, cex.axis = 1.5)   
        }
        mtext("Harvesting level", side = 3, line = 2.5, cex = 1)
        mtext(gsub(": ", "\n", fireLevels[f]), side = 3, line = 5, cex = 1)
        
    }

}

dev.off()


# while plotrix is loaded anyway:
# set colors with color.scale
# need data as matrix*

colors <- as.matrix(df2)
col1 <- col2rgb("#4477AA")/255
col2 <- col2rgb("white")/255
col3 <- col2rgb("#BB4444")/255
mm[df2<=0] <- color.scale(df2[df2<=0], c(col1[1], col2[1]), c(col1[2], col2[2]), c(col1[3], col2[3]))
mm[df2>=0] <- color.scale(df2[df2>=0], c(col2[1], col3[1]), c(col2[2], col3[2]), c(col2[3], col3[3]))





# go from green through yellow to red with no blue
x<-rnorm(20)
y<-rnorm(20)
# use y for the color scale
plot(x,y,col=color.scale(y,c(0,1,1),c(1,1,0),0),main="Color scale plot",
     pch=16,cex=2)
plot(1:10,rep(1:3,length.out=10),axes=FALSE,type="n",xlim=c(0,11),ylim=c(0,4),
     main="Test of RGB, HSV and HCL",xlab="",ylab="Color specification")
axis(2,at=1:3,labels=c("HCL","HSV","RGB"))
points(1:10,rep(1,10),pch=19,cex=8,col=color.scale(1:10,c(0,300),35,85,
                                                   color.spec="hcl"))
points(1:10,rep(2,10),pch=19,cex=8,col=color.scale(1:10,c(0,1),
                                                   0.8,1,color.spec="hsv"))
points(1:10,rep(3,10),pch=19,cex=8,col=color.scale(1:10,c(1,0.5,0),
                                                   c(0,0.5,0),c(0,0,1),color.spec="rgb"))