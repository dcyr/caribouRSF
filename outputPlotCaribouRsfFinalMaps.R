rm(list = ls())
setwd("~/Travail/SCF/CBFA/caribou")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(ggmap)
require(rgdal)
require(dplyr)
require(RColorBrewer)
require(reshape2)
require(maptools)

figRatio <- 111/71

##################################################################################################################
##################################################################################################################
########  Plotting initial conditions
caribouRsInit <- get(load("../processedOutputs/caribouRsInit.RData"))
r <- projectRaster(caribouRsInit, crs = CRS("+init=epsg:4326"))
df <- rasterToPoints(r)
df <- data.frame(df)
names(df)[c(1,2)] <- c("longitude", "latitude")
studyArea <- readOGR(dsn = "../data", layer = "LSJ_Caribou_Extent")
require(ggplot2)
###################################################################################################################
###################################################################################################################
studyArea <- spTransform(studyArea, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyArea)

p <- ggplot(data = df, aes_string("longitude", "latitude", fill = colnames(df)[3])) +
    theme_bw() +
    theme(legend.position="top", legend.direction="horizontal") +
    geom_raster() +
    scale_fill_gradientn(name = "RS call", limits = c(0,1),
                         colours = c("white", "lightblue", "seagreen4", "gold2", "darkred"),
                         values = c(0,  0.25, 0.5, 0.75, 1)) +

    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'grey25', fill = NA, alpha = 1, size = .1) +
    coord_fixed(ratio = figRatio)


png(filename = paste("caribouRS_Init.png"),
    width = 1000, height = 1200, units = "px", res = 300, pointsize = 8,
    bg = "white")

    print(p + ggtitle(paste0("Caribou habitat - Initial condtions")) +

              theme(plot.title = element_text(size = rel(0.8)),
                    axis.title.x = element_text(size = rel(0.7)),
                    axis.title.y = element_text(size = rel(0.7)),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5)),
                    legend.text = element_text(size = rel(0.4)),
                    legend.title = element_text(size = rel(0.7))))
dev.off()



###################################################################################################################
###################################################################################################################
########  Plotting final conditions
caribouRsMeanFinalDF <- get(load("../processedOutputs/caribouRsMeanFinalDF.RData"))
studyArea <- readOGR(dsn = "../data", layer = "LSJ_Caribou_Extent")
require(ggplot2)
###################################################################################################################
###################################################################################################################
studyArea <- spTransform(studyArea, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyArea)

for (s in levels(caribouRsMeanFinalDF$scenario)) {

    df <- filter(caribouRsMeanFinalDF, scenario == s)
    df <- droplevels(df)
    pWidth  <- 1400
    pHeight <- ifelse(length(levels(df$fire)) == 1, 1200, 850*length(levels(df$fire)))

    p <- ggplot(data = df, aes_string("longitude", "latitude", fill = colnames(df)[5])) +
        theme_bw() +
        theme(legend.position="top", legend.direction="horizontal") +
        geom_raster() +
        facet_grid(fire ~ harvest) +
        scale_fill_gradientn(name = "RS call", limits = c(0,1),
                             colours = c("white", "lightblue", "seagreen4", "gold2", "darkred"),
                             values = c(0,  0.25, 0.5, 0.75, 1)) +
        coord_fixed(ratio = figRatio) +
        geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                     colour = 'grey25', fill = NA, alpha = 1, size = .1)


    png(filename = paste0("caribouRS_MeanFinalAbs_", s, ".png"),
        width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
        bg = "white")

    print(p + ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate")) +

              theme(plot.title = element_text(size = rel(0.8)),
                    axis.title.x = element_text(size = rel(0.7)),
                    axis.title.y = element_text(size = rel(0.7)),
                    #axis.ticks = element_blank(),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5)),
                    #strip.background = element_blank(),
                    strip.text.y = element_text(size = rel(0.7)),
                    strip.text.x = element_text(size = rel(0.7)),
                    legend.text = element_text(size = rel(0.4)),
                    legend.title = element_text(size = rel(0.7))))
    #strip.text.x = element_text(size=6)))
    dev.off()
}




###################################################################################################################
###################################################################################################################
########  Plotting final rasters - Average differences between final and initial conditions
caribouRsFinalDiffDF <- get(load("../processedOutputs/caribouRsFinalDiffDF.RData"))
studyArea <- readOGR(dsn = "../data", layer = "LSJ_Caribou_Extent")
require(ggplot2)
###################################################################################################################
###################################################################################################################
studyArea <- spTransform(studyArea, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyArea)

for (s in levels(caribouRsFinalDiffDF$scenario)) {

    df <- filter(caribouRsFinalDiffDF, scenario == s)
    df <- droplevels(df)
    pWidth  <- 1400
    pHeight <- ifelse(length(levels(df$fire)) == 1, 1200, 850*length(levels(df$fire)))

    p <- ggplot(data = df, aes_string("longitude", "latitude", fill = colnames(df)[5])) +
        theme_bw() +
        theme(legend.position="top", legend.direction="horizontal") +
        geom_raster() +
        facet_grid(fire ~ harvest) +
        # ?geom_text(data = NULL, x = maxX, y = maxY,
        #           label = paste0("Time =", ts)) +
        # annotate("text", label = paste0("Time = ", ts), x = rangeX[2], y = rangeY[2], hjust = 1, size = 3, colour = "darkred") +
        # annotate("text", label = paste0("Climate: baseline"), x = rangeX[2], y = rangeY[1]+0.96*(rangeY[2]-rangeY[1]), hjust = 1, size = 2, colour = "black") +
        # annotate("text", label = paste0("Fire regime: baseline"), x = rangeX[2], y = rangeY[1]+0.93*(rangeY[2]-rangeY[1]), hjust = 1, size = 2, colour = "black") +
        # annotate("text", label = paste0("Harvest level: 100%"), x = rangeX[2], y = rangeY[1]+0.9*(rangeY[2]-rangeY[1]), hjust = 1, size = 2, colour = "black") +
        scale_fill_gradientn(name = "RS call\n(variation between inital\nconditions and 2100)", limits = c(-.7,.7),

                             colours = c("darkblue", "darkblue", "lightblue","white", "gold2", "darkred", "darkred")) +
                             #colours = c("darkred", "darkred", "gold2", "white", "palegreen3", "darkgreen", "darkgreen")) +#,#c("white", "lightblue", "seagreen4", "gold2", "darkred"),
        #colours = c(matlab.like2(5)),
        #values = c(-0.6  -0.3, 0, 0.3, 0.6)) +
        coord_fixed(ratio = figRatio) +
        geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                     colour = 'grey25', fill = NA, alpha = 1, size = .1)




    png(filename = paste0("caribouRS_MeanFinalDiff_", s, ".png"),
        width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
        bg = "white")

        print(p + ggtitle(paste0("Evolution of caribou habitat from initial conditions to 2100 - ", s, " climate")) +

                              theme(plot.title = element_text(size = rel(0.8)),
                                    axis.title.x = element_text(size = rel(0.7)),
                                    axis.title.y = element_text(size = rel(0.7)),
                                    #axis.ticks = element_blank(),
                                    axis.text.x = element_text(size = rel(0.5)),
                                    axis.text.y =  element_text(size = rel(0.5)),
                                    #strip.background = element_blank(),
                                    strip.text.y = element_text(size = rel(0.7)),
                                    strip.text.x = element_text(size = rel(0.7)),
                                    legend.text = element_text(size = rel(0.4)),
                                    legend.title = element_text(size = rel(0.5))))
              #strip.text.x = element_text(size=6)))
    dev.off()
}

##############################################################################
##############################################################################
########  Plotting final RS call by ecodistrict
########  ZONAL stats
require(maptools)
caribouRS_ecodist_mean <- get(load("../processedOutputs/caribouRS_ecodist_mean_DF.RData"))
studyArea <- readOGR(dsn = "../data", layer = "LSJ_Caribou_Extent")
ecodistricts <- readOGR(dsn = "../gis/Ecodistricts", layer = "ecodistricts")
## projecting in lat long if necessary
studyArea <- spTransform(studyArea, CRS("+init=epsg:4326"))
ecodistricts <- spTransform(ecodistricts, CRS("+init=epsg:4326"))
## cropping ecodistricts 
ecodistricts <- crop(ecodistricts, studyArea)
#ecodistricts <- gSimplify(ecodistricts, tol = 0.001, topologyPreserve = T)
## fortifying for ggplot2
ecodistrictF <- fortify(ecodistricts, region  = "ECODISTRIC")
ecodistrictF[, "ecodistrict"] <- ecodistrictF$id
studyAreaF <- fortify(studyArea)


### computing values 
require(dplyr)
df <- caribouRS_ecodist_mean %>%
    group_by(scenario, ecodistrict, fire, harvest, time) %>%
    summarise(caribouRS_mean = mean(caribouRS_mean),
              h50 = mean(h50),
              h75 = mean(h75),
              h90 = mean(h90),
              h95 = mean(h95)) %>%
    ungroup()

initDf <- df %>% 
    filter(time == 0, scenario == "baseline", fire == "Fire regime: baseline", harvest == "Harvesting level: 100%") %>%
    select(ecodistrict, caribouRS_mean, h50, h75, h90, h95)

ts <- 100
df <- df %>% 
    filter(time == ts)
df <- as.data.frame(df)

## selecting variables to plot
variables <- colnames(df)[c(8:9)]
names(variables) <- c("best 25%", "best 10%")

df <- merge(df, initDf, by =  "ecodistrict", suffixes = c("",".init"))


for (i in seq_along(variables)) {
    # breaks <- c(0.25, 0.75, 1.25, 2, Inf) * (100 - as.numeric(gsub("[^0-9]", "", variables[i]))) / 100
    # breaks <- c(0, breaks[breaks<1], 1)
    # value <- cut(df[,colIndex],
    #              breaks= breaks,
    #              labels = c("less than 25%", "less than 75%", "stable", "more than 125%", "more than 200%")[1:(length(breaks)-1)],
    #              include.lowest = T)
    # cols <- rev(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA") )
    value <- 100*(df[,variables[i]] - df[,paste0(variables[i], ".init")])
    colScale <- scale_fill_gradient2(name = paste0("Percent change in abundance of HQH\n(comparable to initial ",
                                                  names(variables)[i], ")"),
                                     low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0, limits = c(-75, 75))

    
    dfTmp <- data.frame(df[,c("scenario", "ecodistrict", "fire", "harvest")], value = value)
    dfTmp <- merge(ecodistrictF, dfTmp)
    colnames(dfTmp)[2:3] <- c("longitude", "latitude")

    for (s in c("baseline", "RCP26", "RCP45", "RCP85")) {
        
        x <- filter(dfTmp, scenario == s)
        x$fire <- droplevels(x$fire)
        
        pWidth  <- 1400
        pHeight <- ifelse(length(levels(x$fire)) == 1, 1200, 850*length(levels(x$fire)))
        
        g <- ggplot(data = x, aes(x = longitude, y = latitude, group = group, fill = value)) +
            theme_bw() +
            geom_polygon(colour = 'grey25', alpha = 1, size = .1) +
            colScale +
            theme_bw() +
            theme(legend.position="top", legend.direction="horizontal") +
            facet_grid(fire ~ harvest) +
            coord_fixed(ratio = figRatio)
        
        png(filename = paste0("caribouRS_HQHtrends_", variables[i], "_", s , ".png"),
            width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
            bg = "white")
        
            print(g + labs(title = paste0("Projected variations in abundance of Caribou high quality habitats (HQH)\nFrom 2010 to 2100 - ", s, " climate")) +
                      #ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate\n", names(variables[i]))) +
                      
                      theme(plot.title = element_text(size = rel(0.7)),
                            axis.title.x = element_text(size = rel(0.7)),
                            axis.title.y = element_text(size = rel(0.7)),
                            #axis.ticks = element_blank(),
                            axis.text.x = element_text(size = rel(0.5)),
                            axis.text.y =  element_text(size = rel(0.5)),
                            #strip.background = element_blank(),
                            strip.text.y = element_text(size = rel(0.7)),
                            strip.text.x = element_text(size = rel(0.7)),
                            legend.title = element_text(size = rel(0.5)),
                            legend.text = element_text(size = rel(0.4))))
            #strip.text.x = element_text(size=6)))
        dev.off()
    }
}


### initial conditions 
###
###
df <- merge(ecodistrictF, initDf)
colnames(df)[2:3] <- c("longitude", "latitude")

for (i in seq_along(variables)) {
    ### defining class / colors
    # breaks <- c(0.25, 0.75, 1.25, 2, Inf) * (100 - as.numeric(gsub("[^0-9]", "", variables[i]))) / 100
    # breaks <- c(0, breaks[breaks<1], 1)
    breaks <-  c(0, 0.05, 0.25, 0.50, 0.75, 1)
    df[,"value"] <- cut(df[,variables[i]],
                        breaks= breaks,
                 labels = c("0-5%", "5-25%", "25-50%", "50-75%", "more than 75%"),
                 include.lowest = T)
    # colScale <- scale_fill_gradientn(name = paste0("Proportion of HQH (", names(variables)[i], ")"),
    #                                  colours = c("white", "darkolivegreen") ,
    #                                  values = c(0, 1))
    colFunc <- colorRampPalette(c("white", "darkolivegreen"))
    
    colScale <- scale_fill_manual(name = paste0("Proportion of HQH (", names(variables)[i], ")"),
                                  breaks = levels(df$value),
                                  values = colFunc(length(breaks)-1))


    pWidth  <- 1000
    pHeight <- 1200
    
    g <- ggplot(data = df, aes(x = longitude, y = latitude, group = group, fill = value)) +
        theme_bw() +
        geom_polygon(colour = 'grey25', alpha = 1, size = .1) +
        colScale +
        theme_bw() +
        theme(legend.position="top", legend.direction="horizontal") +
        #facet_grid(fire ~ harvest) +
        coord_fixed(ratio = figRatio)
    
    png(filename = paste0("caribouRS_HQHinitial_", variables[i] , ".png"),
        width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
        bg = "white")
    
    print(g + labs(title = paste0("Initial spatial distribution of Caribou high quality habitats (HQH)")) +
              #ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate\n", names(variables[i]))) +
              
              theme(plot.title = element_text(size = rel(0.6)),
                    axis.title.x = element_text(size = rel(0.7)),
                    axis.title.y = element_text(size = rel(0.7)),
                    #axis.ticks = element_blank(),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5)),
                    #strip.background = element_blank(),
                    strip.text.y = element_text(size = rel(0.7)),
                    strip.text.x = element_text(size = rel(0.7)),
                    legend.title = element_text(size = rel(0.5)),
                    legend.text = element_text(size = rel(0.4))))
    #strip.text.x = element_text(size=6)))
    dev.off()
}





