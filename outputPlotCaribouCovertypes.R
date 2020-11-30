rm(list = ls())
setwd("E:/SCF/CBFA/caribou")
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

### loading cover types results

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

## adding tidy treatment factors to final df
#fire <- factor(rep("Fire regime: baseline", nrow(coverTypesTotal)), levels = c("Fire regime: baseline", "Fire regime: projected"))
fire <- factor(rep("No fires", nrow(coverTypesTotal)), levels = c("No fires", "Fire regime: baseline", "Fire regime: projected"))
fire[grep("BaselineFire", coverTypesTotal$treatment)] <- "Fire regime: baseline"
fire[grep("ProjectedFire", coverTypesTotal$treatment)] <- "Fire regime: projected"
growth <- factor(rep("Growth: baseline", nrow(coverTypesTotal)), levels = c("Growth: baseline", "Growth: projected"))
growth[grep("Growth", coverTypesTotal$treatment)] <- "Growth: projected"
harvest <- factor(rep("No Harvesting", nrow(coverTypesTotal)), levels = c("No Harvesting", "Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("Harvest", coverTypesTotal$treatment)] <- "Harvesting level: 100%"
harvest[grep("0.5", coverTypesTotal$treatment)] <- "Harvesting level: 50%"

coverTypesTotal <- data.frame(coverTypesTotal, growth, fire, harvest)


# by ecodistrict
fire <- factor(rep("No fires", nrow(coverTypes)), levels = c("No fires", "Fire regime: baseline", "Fire regime: projected"))
fire[grep("BaselineFire", coverTypes$treatment)] <- "Fire regime: baseline"
fire[grep("ProjectedFire", coverTypes$treatment)] <- "Fire regime: projected"
growth <- factor(rep("Growth: baseline", nrow(coverTypes)), levels = c("Growth: baseline", "Growth: projected"))
growth[grep("Growth", coverTypes$treatment)] <- "Growth: projected"
harvest <- factor(rep("No Harvesting", nrow(coverTypes)), levels = c("No Harvesting", "Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("Harvest", coverTypes$treatment)] <- "Harvesting level: 100%"
harvest[grep("0.5", coverTypes$treatment)] <- "Harvesting level: 50%"

coverTypes <- data.frame(coverTypes, growth, fire, harvest)

df <- coverTypesTotal %>%
    filter(((scenario == "baseline" & growth == "Growth: baseline") |
               scenario %in% c("RCP26", "RCP45", "RCP85") &
               growth == "Growth: projected") &
               fire != "No fires" &
               treatment %in% treatment[grep("Budworm", treatment)]&
               treatment != "Control")


xRange <- 100*ceiling(max(abs(range(df$propTotalDiff)))*20)/20
xRange <- c(-xRange, xRange)
# defining colors
col1 <- col2rgb("#4477AA")/255 # nice blue
col2 <- col2rgb("white")/255 
col3 <- col2rgb("#BB4444")/255 # nice red

coverNames <- c(fire0_9 = "Fire_0-9",
                harvest0_9 = "Cut_0-9",
                fire10_49 = "Fire_10-49",
                conifHarv10_49 = "ConifCut_10-49",
                decidHarv10_49 = "DecidCut_10-49",
                decidOther10_49 = "DecidOth_10-49",
                conif50_89 = "Conif_50-89",
                decid50_89 = "Decid_50-89",
                conif90 = "Conif_90+")           

    
n <- length(unique(paste(df$fire, df$scenario)))


png(filename = paste0("coverTypeDiff.png"),
    width = 500 + ((n-1)/2)*500, height = 1200, units = "px", res = 300, pointsize = 6,
    bg = "white")

w <- c(1, rep(0.5, (n-1)))

layout(matrix(seq(1,n), 1, n, byrow = TRUE), 
       widths = w, heights = 1 )

for (s in c("baseline", "RCP26", "RCP45", "RCP85")) {
    require(plotrix)
    
    df2 <- df %>%
        filter(scenario == s,
               harvest %in% c("No Harvesting", "Harvesting level: 50%", "Harvesting level: 100%")) %>%
        select(coverType, harvest, fire, propTotalDiff) %>%
        arrange(coverType, harvest, fire)
    df2 <- droplevels(df2)
    
    fireLevels <- unique(df2$fire)

    
    for (f in seq_along(fireLevels)) {
        
        df3 <- df2 %>%
            filter(fire  == fireLevels[f]) %>%
            select(coverType, harvest, propTotalDiff)
        df3 <- as.data.frame(unstack(df3, propTotalDiff ~ harvest))
        df3 <- df3 *100
        rownames(df3) <- unique(df2$coverType)
        df3 <- df3[names(coverNames),]
        # colors
        colors <- as.matrix(df3)
        colors[df3<=0] <- color.scale(df3[df3<=0], c(col1[1], col2[1]), c(col1[2], col2[2]), c(col1[3], col2[3]), xrange = c(xRange[1], 0))
        colors[df3>=0] <- color.scale(df3[df3>=0], c(col2[1], col3[1]), c(col2[2], col3[2]), c(col2[3], col3[3]), xrange = c(0, xRange[2]))
    

        if (f == 1) {
            if (s == "baseline") {
                par(mar = c(1, 13, 9, 1.25))#c(1, 13, 9, 1.25)
            } else {
                par(mar = c(1, 1, 9, 0.25))
            }
            
        } else {
            par(mar = c(1, 0.5, 9, 1.25))
        }
        color2D.matplot(df3, 
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
        axis(3, at = seq_len(ncol(df3)) - 0.5,
             labels = c("0%", "50%", "100%"), tick = FALSE, cex.axis = 1.25, col = "white")#"50%",
        if(f == 1 & s == "baseline") {
            axis(2, at = seq_len(nrow(df3)) -0.5,
                 labels = rev(coverNames), tick = FALSE, las = 1, cex.axis = 1.5)   
        }
        mtext("Harvesting level", side = 3, line = 2.5, cex = 1)
        mtext(gsub(": ", "\n", fireLevels[f]), side = 3, line = 5, cex = 1)
        
    }

}
dev.off()
