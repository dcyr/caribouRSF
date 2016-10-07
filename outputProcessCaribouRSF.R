##################################################################
##################################################################
#### Dominic Cyr
rm(list =ls())
setwd("/home/dcyr/Travail/SCF/CBFA/caribou")
##################################################################
wwd <- (paste(getwd(), Sys.Date(), sep ="/"))
dir.create(wwd)
setwd(wwd)
##################################################################



########################################################################################
########################################################################################
### computing ecodistrict averages, storing into tidy data frame
require(doSNOW)
require(raster)
require(reshape2)
#outputDir <- "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/"
outputDir <- "/media/dcyr/Data/caribouRsOutputs/"
outputFiles <- list.files(outputDir)
simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")
ecodistricts <- raster("../gis/ecodistricts.tif")
########################################################################################
nClusters <- 2 ## about 4.5 Gb of RAM by core for LSJ
cl <- makeCluster(nClusters)
registerDoSNOW(cl)
caribouRS_ecodist_mean <- foreach(i = seq_along(outputFiles), .combine = "rbind")  %dopar% {#
    require(raster)
    require(reshape2)
    a <- simInfo[[i]][2]
    s <- simInfo[[i]][3]
    t <- simInfo[[i]][4]
    r <- as.numeric(simInfo[[i]][5])

    ### loading raster stack
    caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))

    ### computing zonal averages
    x <- as.data.frame(zonal(caribouRS, ecodistricts, fun = 'mean'))
    names(x)[1] <- "ecodistrict"
    x <- melt(x, id = "ecodistrict", variable.name = "time", value.name = "caribouRS_mean")
    x$time <- as.numeric(gsub("[^0-9]", "", x$time))
    x <- data.frame(area = a,
                    scenario = s,
                    treatment = t,
                    replicate = r,
                    x)
    return(x)
}
stopCluster(cl)
### a little tyding up before saving to file
caribouRS_ecodist_mean$ecodistrict <- as.factor(caribouRS_ecodist_mean$ecodistrict)
caribouRS_ecodist_mean$replicate <- as.factor(caribouRS_ecodist_mean$replicate)
df <- caribouRS_ecodist_mean
fire <- factor(rep("Fire regime: baseline", nrow(df)), levels = c("Fire regime: baseline", "Fire regime: projected"))
fire[grep("Projected", df$treatment)] <- "Fire regime: projected"
harvest <- factor(rep("Harvesting level: 100%", nrow(df)), levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("0.5", df$treatment)] <- "Harvesting level: 50%"

caribouRS_ecodist_mean <- data.frame(caribouRS_ecodist_mean, fire, harvest)
########################################################################################
save(caribouRS_ecodist_mean, file = "caribouRS_ecodist_mean_DF.RData")
########################################################################################
########################################################################################


########################################################################################
########################################################################################
### computing ecodistrict averages, storing into tidy data frame
require(doSNOW)
require(raster)
require(reshape2)
#outputDir <- "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/"
outputDir <- "/media/dcyr/Data/caribouRsOutputs/"
outputFiles <- list.files(outputDir)
simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")
ecodistricts <- raster("../gis/ecodistricts.tif")
########################################################################################
nClusters <- 10 ## takes less memory than for ecodistricts (~2.5 Gb by thread)
cl <- makeCluster(nClusters)
registerDoSNOW(cl)

caribouRS_mean <- foreach(i = seq_along(outputFiles), .combine = "rbind")  %dopar% {#
    require(raster)
    require(reshape2)
    a <- simInfo[[i]][2]
    s <- simInfo[[i]][3]
    t <- simInfo[[i]][4]
    r <- as.numeric(simInfo[[i]][5])

    ### loading raster stack
    caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))
    time <- as.numeric(gsub("[^0-9]", "", names(caribouRS)))
    x <- list()
    for (l in 1:nlayers(caribouRS)) {
        v <- values(caribouRS[[l]])
        v <- v[!is.na(v)]
        if(l == 1) {
            threshold <- quantile(v, c(0.5, 0.75, 0.9, 0.95))
            npixel <- length(v)
        }

        x[[l]] <- data.frame(area = a,
                        scenario = s,
                        treatment = t,
                        time = time[l],
                        replicate = r,
                        caribouRS_mean = mean(v),
                        h50 = sum(v>threshold[1])/npixel,
                        h75= sum(v>threshold[2])/npixel,
                        h90 = sum(v>threshold[3])/npixel,
                        h95 = sum(v>threshold[4])/npixel)

    }
    x <- do.call("rbind", x)

    return(x)
}
stopCluster(cl)
### a little tyding up before saving to file

caribouRS_mean$replicate <- as.factor(caribouRS_mean$replicate)
df <- caribouRS_mean

fire <- factor(rep("Fire regime: baseline", nrow(df)), levels = c("Fire regime: baseline", "Fire regime: projected"))
fire[grep("Projected", df$treatment)] <- "Fire regime: projected"
harvest <- factor(rep("Harvesting level: 100%", nrow(df)), levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
harvest[grep("0.5", df$treatment)] <- "Harvesting level: 50%"
caribouRS_mean <- data.frame(caribouRS_mean, fire, harvest)

########################################################################################
save(caribouRS_mean, file = "caribouRS_mean_DF.RData")
########################################################################################
########################################################################################



#####################################################################################
#####################################################################################
#########  
#########  
require(ggplot2)   
require(dplyr)
caribouRS_ecodist_mean <- get(load("../processedOutputs/caribouRS_ecodist_mean_DF.RData"))
caribouRS_mean <- get(load("../processedOutputs/caribouRS_mean_DF.RData"))
#####################################################################################
#####################################################################################

for (i in c("ecodistrict", "total")) {
    if(i == "ecodistrict") {
        df <- caribouRS_ecodist_mean
        
        df <- mutate(df, id = as.numeric(as.factor(paste(scenario, treatment, ecodistrict, replicate))))
        plotDim <- c(2400,2400)
        pRatio <- 1.2
    }
    if (i == "total") {
        df <- caribouRS_mean
        df <- mutate(df, id = as.numeric(as.factor(paste(scenario, treatment, replicate))))
        plotDim <- c(1200,1000)
        pRatio <- 0.75
        df[,c("h50", "h75", "h90", "h95")] <- df[,c("h50", "h75", "h90", "h95")] * 100
        fileNameHQH <- character()
    }
    fileNameMean <- character()
    ### shuffling ids
    idShuffle <- sample(unique(df$id))
    df$id <- idShuffle[df$id]
    
    
    df$time <- df$time + 2000    

    for (t in c("scenario", "fire", "harvest")) {
        
        if (t == "scenario") {
            cols <- c("darkolivegreen3", "dodgerblue2", "goldenrod1", "red3")
            lTitle = "Climate change scenario"
        }
        if (t == "fire") {
            cols <-  c("darkolivegreen","indianred")
            lTitle = ""
        }
        if (t == "harvest") {
            cols <- c("cadetblue4","goldenrod3")
            lTitle = ""
        }
            
        g <- ggplot(df, aes_string(x = "time", y = "caribouRS_mean", group = "id", color = t)) +
            geom_line(stat="smooth",method = "loess", span = 0.4,
                      size = 0.5,
                      alpha = 0.5) +
            scale_colour_manual(values = cols,
                                guide = guide_legend(title = lTitle,
                                                     title.theme = element_text(size = 8 * pRatio, angle = 0),
                                                     label.theme = element_text(size = 8 * pRatio, angle = 0),
                                                     #label.position = "bottom", label.hjust = 0.5,
                                                     override.aes = list(size = 2,
                                                                         alpha = 1))) +
            theme_linedraw() +
            theme(legend.position="top", legend.direction="horizontal",
                  plot.title = element_text(size = rel(pRatio)),
                  axis.title = element_text(size = rel(pRatio)),
                  axis.title.x=element_blank()) +
            labs(title = "Evolution of Caribou habitat quality",
                 y = "RS call\n")
        
        if(i == "ecodistrict") {
            g <- g + facet_wrap(~ ecodistrict)
        }
        
        fName <- paste0("caribouRS_mean_", i, "_", t ,".png")
        fileNameMean <- append(fileNameMean, fName)
        png(filename = fName,
            width = plotDim[1], height = plotDim[2], res = 300)
        
            print(g + theme(axis.text.x = element_text(size = 8 * pRatio, angle = 45, hjust = 1),
                            axis.text.y = element_text(size = 8 * pRatio),
                            strip.text = element_text(size = 8 * pRatio)))
        
        
        dev.off()
        
        #####################################################################################
        ### habitat
        
        
        #segDF <- distinct(df[, c("scenario", "fire", "harvest")])
       if (i == "total") {
            g <- ggplot(df, aes_string(group = "id", color = t)) +
                geom_line(aes_string(x = "df$time", y = "df$h75"),
                          stat="smooth",method = "loess", span = 0.4,
                          size = 0.5,
                          alpha = 0.5) +
                geom_line(aes_string(x = "df$time", y = "df$h90"),
                          stat="smooth",method = "loess", span = 0.4,
                          size = 0.5,
                          alpha = 0.5) +
                scale_colour_manual(values = cols,
                                    guide = guide_legend(title = lTitle,
                                                         title.theme = element_text(size = 8 * pRatio, angle = 0),
                                                         label.theme = element_text(size = 8 * pRatio, angle = 0),
                                                         override.aes = list(size = 2,
                                                                             alpha = 1))) +
                theme_linedraw() +
                theme(legend.position="top", legend.direction="horizontal",
                      plot.title = element_text(size = rel(pRatio)),
                      axis.title = element_text(size = rel(pRatio)),
                      axis.title.x=element_blank()) +
                labs(title = "Relative abundance of high quality habitats",
                     y = "Relative abundance (% of landscape)\n") +
                annotate("text", x = min(df$time) + 1, y = c(9, 24), label = c("Initial best 10%", "Initial best 25%"),
                         color = "grey25", size  = 2, hjust = 0, vjust = 1)
            
    
            fName <- paste0("caribouRS_HQH_", i, "_", t ,".png")
            fileNameHQH <- append(fileNameHQH, fName)
            png(filename= fName,
                width = plotDim[1], height = plotDim[2], res = 300)
            
                print(g + theme(axis.text.x = element_text(size = 8 * pRatio, angle = 45, hjust = 1),
                                axis.text.y = element_text(size = 8 * pRatio),
                                strip.text = element_text(size = 8 * pRatio)))
                
            
            dev.off()
        }
    }
    fileNameMean <- paste(getwd(), fileNameMean, sep = "/")
    
    require(animation)
    oopt = ani.options(ani.dev="png", ani.type="png", interval = 1.5, autobrowse = FALSE)
    ### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
    im.convert(fileNameMean, output = paste0("caribouRS_mean_", i, "_anim.gif"),
               extra.opts = "", clean = F)
    ### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
    
    if(i == "total") {
        fileNameHQH <- paste(getwd(), fileNameHQH, sep = "/")
        im.convert(fileNameHQH, output = paste0("caribouRS_HQH_", i, "_anim.gif"),
               extra.opts = "", clean = F)
    }
}


    
 
    
##################

r <- ecodistricts
rDF <- rasterToPoints(r)
df <- data.frame(rDF)
df$ecodistricts <- as.factor(df$ecodistricts)

##################
##################
#######  Plotting

require(RColorBrewer)

meanX <- as.matrix(by(df$x, df$ecodistricts, mean, na.rm = TRUE))
meanY <- as.matrix(by(df$y, df$ecodistricts, mean, na.rm = TRUE))

coordLab <- data.frame(X = meanX, Y = meanY, label = rownames(meanY))

colEcodistrict <- brewer.pal(8, "Dark2")
colEcodistrict <- rep(colEcodistrict, 2)

g <- ggplot(data = df, aes(x, y, fill = ecodistricts)) +
    theme_bw() +
    geom_raster() +
    annotate("text", x = coordLab$X, y = coordLab$Y, label = coordLab$label,
             color = "grey25", size  = 3) +
    #geom_text(data = NULL, aes(label = coordLab$label, x = coordLab$X, y = coordLab$Y)) +
    coord_equal() +
    scale_fill_manual(values = colEcodistrict) 
                      #guide = guide_legend(title = "Ecodistrict",
                      #                     reverse = TRUE))

png(filename = "ecodistricts.png",
    width = 1200, height = 1200,
    res = 300)

    print(g + theme(plot.title = element_text(size = rel(0.8)),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y =  element_blank(),
                    strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    legend.text = element_text(size = rel(0.5)),
                    legend.title = element_text(size = rel(0.7))) +
              labs(title = "Ecodistricts") +
              guides(fill=FALSE))
        
              
dev.off()




