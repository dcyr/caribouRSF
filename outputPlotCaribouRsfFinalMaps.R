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


pWidth  <- 1000
pHeight <- 1200
title <- "Caribou habitat - Initial condtions"

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
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
    bg = "white")

    print(p + labs(title = title) +
              theme(plot.title = element_text(size = rel(0.6)),
                    axis.title.x = element_text(size = rel(0.7)),
                    axis.title.y = element_text(size = rel(0.7)),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5)),
                    legend.title = element_text(size = rel(0.5)),
                    legend.text = element_text(size = rel(0.4))))
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

##############
#### plotting each climate change scenario
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

##############
#### plotting worst-case, average, and best-case scenarios

df <- caribouRsMeanFinalDF %>%
    filter(scenario != "baseline")#,
           #harvest != "Harvesting level: 50%")

ensembleSummary <- df %>%
    group_by(variable) %>%
    summarise(meanRScall = mean(value))


meanDf <- df %>%
    group_by(longitude, latitude) %>%
    summarise(value = mean(value)) %>%
    mutate(var = "Average scenario")

df <- rbind(data.frame(df[which(df$variable == as.character(ensembleSummary[which.max(ensembleSummary$meanRScall), "variable"])), colnames(meanDf)[1:3]],
                       var = "Best-case scenario"),
            data.frame(df[which(df$variable == as.character(ensembleSummary[which.min(ensembleSummary$meanRScall), "variable"])), colnames(meanDf)[1:3]],
                       var = "Worst-case scenario"),
            meanDf)

df$var <- factor(df$var, levels = c("Worst-case scenario", "Average scenario", "Best-case scenario"))




pWidth  <- 2000
pHeight <- 1200
title <- "Projected habitat quality of Caribou in 2100 - Summary of simulation ensemble"
#### plotting ensemble min, mean, max, absolute values
g <- ggplot(data = df, aes(longitude, latitude, fill = value)) +
    theme_bw() +
    theme(legend.position="top", legend.direction="horizontal") +
    geom_raster() +
    facet_grid( ~ var) +
    scale_fill_gradientn(name = "RS call", limits = c(0,1),
                         colours = c("white", "lightblue", "seagreen4", "gold2", "darkred"),
                         values = c(0,  0.25, 0.5, 0.75, 1)) +
    coord_fixed(ratio = figRatio) +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'grey25', fill = NA, alpha = 1, size = .1)


png(filename = paste0("caribouRS_FinalAbsEnsemble", ".png"),
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
    bg = "white")

    print(g + labs(title = title) +
              #ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate\n", names(variables[i]))) +
              theme(legend.position="top", legend.direction="horizontal",
                    plot.title = element_text(size = rel(0.6)),
                    axis.title.x = element_text(size = rel(0.7)),
                    axis.title.y = element_text(size = rel(0.7)),
                    #axis.ticks = element_blank(),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5)),
                    #strip.background = element_blank(),
                    strip.text.y = element_text(size = rel(0.7)),
                    strip.text.x = element_text(size = rel(0.7)),
                    legend.title = element_text(size = rel(0.5)),
                    legend.text = element_text(size = rel(0.4)))

             )

dev.off()




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




##############
#### plotting worst-case, average, and best-case scenarios

df <- caribouRsFinalDiffDF %>%
    filter(scenario != "baseline")#,
           #harvest != "Harvesting level: 50%")


ensembleSummary <- df %>%
    group_by(variable) %>%
    summarise(meanRScall = mean(value))

meanDf <- df %>%
    group_by(longitude, latitude) %>%
    summarise(value = mean(value)) %>%
    mutate(var = "Average scenario")

df <- rbind(data.frame(df[which(df$variable == as.character(ensembleSummary[which.max(ensembleSummary$meanRScall), "variable"])), colnames(meanDf)[1:3]],
                       var = "Best-case scenario"),
            data.frame(df[which(df$variable == as.character(ensembleSummary[which.min(ensembleSummary$meanRScall), "variable"])), colnames(meanDf)[1:3]],
                       var = "Worst-case scenario"),
            meanDf)

df$var <- factor(df$var, levels = c("Worst-case scenario", "Average scenario", "Best-case scenario"))


pWidth  <- 2000
pHeight <- 1200
title <- "Projected variations in habitat quality of Caribou\n From 2000 to 2100 - Summary of simulation ensemble"
#### plotting ensemble min, mean, max, absolute values
g <- ggplot(data = df, aes(longitude, latitude, fill = value)) +
    theme_bw() +
    theme(legend.position="top", legend.direction="horizontal") +
    geom_raster() +
    facet_grid( ~ var) +
    scale_fill_gradientn(name = "RS call\n(variation between inital\nconditions and 2100)", limits = c(-.7,.7),
                         
                         colours = c("darkblue", "darkblue", "lightblue","white", "gold2", "darkred", "darkred")) +
    coord_fixed(ratio = figRatio) +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'grey25', fill = NA, alpha = 1, size = .1)


png(filename = paste0("caribouRS_FinalDiffEnsemble", ".png"),
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
    bg = "white")

print(g + labs(title = title) +
          #ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate\n", names(variables[i]))) +
          theme(legend.position="top", legend.direction="horizontal",
                plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.7)),
                axis.title.y = element_text(size = rel(0.7)),
                #axis.ticks = element_blank(),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5)),
                #strip.background = element_blank(),
                strip.text.y = element_text(size = rel(0.7)),
                strip.text.x = element_text(size = rel(0.7)),
                legend.title = element_text(size = rel(0.5)),
                legend.text = element_text(size = rel(0.4)))
      
)

dev.off()


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


### summarizing results by ecodistrict
require(dplyr)
df <- caribouRS_ecodist_mean %>%
    group_by(scenario, ecodistrict, fire, harvest, time) %>%
    summarise(caribouRS_mean = mean(caribouRS_mean),
              h50 = mean(h50),
              h75 = mean(h75),
              h90 = mean(h90),
              h95 = mean(h95)) %>%
    ungroup()

### extracting initial conditions, creating separate df
initDf <- df %>%
    filter(time == 0, scenario == "baseline", fire == "Fire regime: baseline", harvest == "Harvesting level: 100%") %>%
    select(ecodistrict, caribouRS_mean, h50, h75, h90, h95)

initDf <- as.data.frame(initDf)

### selecting final time step
ts <- 100

### keeping only final condition
df <- df %>%
    filter(time == ts)
df <- as.data.frame(df)

### extracting final ensemble description (min, max, mean, consensus)
ensembleDf  <- df %>%
    filter(time == ts,
           scenario != "baseline")#,
           #harvest != "Harvesting level: 50%") %>%
    group_by(ecodistrict) %>%
    summarize(caribouRS_mean_finalMax = caribouRS_mean[which.max(caribouRS_mean)],
              caribouRS_mean_finalMin = caribouRS_mean[which.min(caribouRS_mean)],
              caribouRS_mean_finalMean = mean(caribouRS_mean),
              h90finalMax = h90[which.max(h90)],
              h90finalMin = h90[which.min(h90)],
              h90finalMean = mean(h90),
              h75finalMax = h75[which.max(h75)],
              h75finalMin = h75[which.min(h75)],
              h75finalMean = mean(h75))


ensembleDf <- merge(ensembleDf, initDf[,c("ecodistrict", "caribouRS_mean", "h75", "h90")], by =  "ecodistrict")

ensembleDf <- ensembleDf %>%
    mutate(caribouRS_mean_diffMax = caribouRS_mean_finalMax - caribouRS_mean,
           caribouRS_mean_diffMin = caribouRS_mean_finalMin - caribouRS_mean,
           caribouRS_mean_diffMean = caribouRS_mean_finalMean - caribouRS_mean,
           h90diffMax = h90finalMax - h90,
           h90diffMin = h90finalMin - h90,
           h90diffMean = h90finalMean - h90,
           h75diffMax = h75finalMax - h75,
           h75diffMin = h75finalMin - h75,
           h75diffMean = h75finalMean - h75)

ensembleDf <- melt(ensembleDf, id  = "ecodistrict")

###

## selecting variables to plot
variables <- colnames(df)[c(8:9)]
names(variables) <- c("best 25%", "best 10%")

df <- merge(df, initDf, by =  "ecodistrict", suffixes = c("",".init"))

for (i in seq_along(variables)) {



    value <- 100*(df[,variables[i]] - df[,paste0(variables[i], ".init")])



    dfTmp <- data.frame(df[,c("scenario", "ecodistrict", "fire", "harvest")], value = value)
    dfTmp <- merge(ecodistrictF, dfTmp)
    colnames(dfTmp)[2:3] <- c("longitude", "latitude")

    ##################
    ##################
    ####  All treatments for each cc scenario
    colScale <- scale_fill_gradient2(name = paste0("Percent change in abundance of HQH\n(comparable to initial ",
                                                   names(variables)[i], ")"),
                                     low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0,
                                     limits = c(floor(min(dfTmp$value)/10)*10, ceiling(max(dfTmp$value)/10)*10))
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

    ##################
    ##################
    ### ensemble description
    for (j in c("diff", "final")) {

        dfTmp <- ensembleDf[grepl(variables[i], ensembleDf$variable),]
        dfTmp <- merge(ecodistrictF, dfTmp)
        colnames(dfTmp)[2:3] <- c("longitude", "latitude")

        x <- dfTmp[grepl(j, dfTmp$variable), ]
        x <-droplevels(x)
        vLevels <- c("Worst-case scenario", "Average scenario", "Best-case scenario")
        names(vLevels) <- paste0(variables[i], paste0(j, c("Min", "Mean", "Max")))
        x$variable <- factor(vLevels[as.character(x$variable)], levels = vLevels)


        #### plot preparation

        if(j == "diff") {
            title <- "Projected variations in abundance of Caribou high quality habitats (HQH)\nFrom 2010 to 2100 - Summary of simulation ensemble"
            x$value <- x$value*100
            colScale <- scale_fill_gradient2(name = paste0("Percent change in proportion of HQH\n(comparable to initial ",
                                                           names(variables)[i], ")"),
                                             low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0,
                                             limits = c(floor(min(x$value)/10)*10, ceiling(max(x$value)/10)*10))

        }
        if (j == "final") {
            title <- "Projected abundance of Caribou high quality habitats (HQH)\nFrom 2010 to 2100 - Summary of simulation ensemble"
            breaks <-  c(0, 0.10, 0.25, 0.50, 1)
            labels = c("0-10%", "10-25%", "25-50%", "more than 50%")
            x[,"value"] <- cut(x[,"value"],
                               breaks= breaks,
                               labels = labels,
                               include.lowest = T)
            colFunc <- colorRampPalette(c("white", "darkolivegreen"))
            colScale <- scale_fill_manual(name = paste0("Proportion of HQH\n(Comparable to initial ",
                                                        names(variables)[i], ")"),
                                          breaks = levels(x$value),
                                          values = colFunc(length(breaks)-1), drop = F)


        }



        pWidth  <- 2000
        pHeight <- 1200


        #### plotting ensemble min, mean, max
        g <- ggplot(data = x, aes(x = longitude, y = latitude, group = group, fill = value)) +
            theme_bw() +
            geom_polygon(colour = 'grey25', alpha = 1, size = .1) +
            coord_fixed(ratio = figRatio) +
            facet_grid( ~ variable) +
            # colScale +
            theme_bw()

            #facet_grid(fire ~ harvest) +


        png(filename = paste0("caribouRS_HQHensemble_",j, "_", variables[i], ".png"),
            width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
            bg = "white")

            print(g + labs(title = title) +
                      #ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate\n", names(variables[i]))) +
                      colScale +
                      theme(legend.position="top", legend.direction="horizontal",
                            plot.title = element_text(size = rel(0.6)),
                            axis.title.x = element_text(size = rel(0.7)),
                            axis.title.y = element_text(size = rel(0.7)),
                            #axis.ticks = element_blank(),
                            axis.text.x = element_text(size = rel(0.5)),
                            axis.text.y =  element_text(size = rel(0.5)),
                            #strip.background = element_blank(),
                            strip.text.y = element_text(size = rel(0.7)),
                            strip.text.x = element_text(size = rel(0.7)),
                            legend.title = element_text(size = rel(0.5)),
                            legend.text = element_text(size = rel(0.4)))

                     )

        #strip.text.x = element_text(size=6)))
        dev.off()

    }

    ##################
    ##################
    ### initial conditions

    x <- merge(ecodistrictF, initDf[,c("ecodistrict", variables[i])])
    colnames(x)[2:3] <- c("longitude", "latitude")
    title <- paste0("Initial spatial distribution of Caribou high quality habitats (HQH)")
    breaks <-  c(0, 0.10, 0.25, 0.50, 1)
    labels = c("0-10%", "10-25%", "25-50%", "more than 50%")
    x[,"value"] <- cut(x[,variables[i]],
                       breaks= breaks,
                       labels = labels,
                       include.lowest = T)
    colFunc <- colorRampPalette(c("white", "darkolivegreen"))
    colScale <- scale_fill_manual(name = paste0("Proportion of HQH\n(", names(variables)[i], ")"),
                                  breaks = levels(x$value),
                                  values = colFunc(length(breaks)-1), drop = F)


    pWidth  <- 1000
    pHeight <- 1200

    g <- ggplot(data = x, aes(x = longitude, y = latitude, group = group, fill = value)) +
        theme_bw() +
        geom_polygon(colour = 'grey25', alpha = 1, size = .1) +
        coord_fixed(ratio = figRatio) +
        theme_bw()

    #facet_grid(fire ~ harvest) +


    png(filename = paste0("caribouRS_HQHinitial_", variables[i], ".png"),
        width = pWidth, height = pHeight, units = "px", res = 300, pointsize = 8,
        bg = "white")

    print(g + labs(title = title) +
              #ggtitle(paste0("Caribou habitat in 2100 - ", s, " climate\n", names(variables[i]))) +
              colScale +
              theme(legend.position="top", legend.direction="horizontal",
                    plot.title = element_text(size = rel(0.6)),
                    axis.title.x = element_text(size = rel(0.7)),
                    axis.title.y = element_text(size = rel(0.7)),
                    #axis.ticks = element_blank(),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5)),
                    #strip.background = element_blank(),
                    strip.text.y = element_text(size = rel(0.7)),
                    strip.text.x = element_text(size = rel(0.7)),
                    legend.title = element_text(size = rel(0.5)),
                    legend.text = element_text(size = rel(0.4)))

    )

    #strip.text.x = element_text(size=6)))
    dev.off()
}


