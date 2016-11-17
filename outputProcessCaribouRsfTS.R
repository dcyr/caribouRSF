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



# ########################################################################################
# ########################################################################################
# ### computing ecodistrict averages, storing into tidy data frame
# require(doSNOW)
# require(raster)
# require(reshape2)
# outputDir <- ifelse(Sys.info()[["nodename"]] == "dcyr-desktop",
#                      "/media/dcyr/Data/caribouRsOutputs/",
#                      "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/")
# 
# outputFiles <- list.files(outputDir)
# simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")
# ecodistricts <- raster("../gis/ecodistricts.tif")
# r <- !is.na(ecodistricts)
# ecodistPixelsN <- zonal(r, ecodistricts, "sum")
# ########################################################################################
# nClusters <- 6 ## about 4.5 Gb of RAM by core for LSJ
# cl <- makeCluster(nClusters)
# registerDoSNOW(cl)
# caribouRS_ecodist_mean <- foreach(i = seq_along(outputFiles), .combine = "rbind")  %dopar% {#
#     require(raster)
#     require(reshape2)
#     a <- simInfo[[i]][2]
#     s <- simInfo[[i]][3]
#     t <- simInfo[[i]][4]
#     r <- as.numeric(simInfo[[i]][5])
# 
#     ### loading raster stack
#     caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))
# 
#     ### computing zonal averages
#     x <- as.data.frame(zonal(caribouRS, ecodistricts, fun = 'mean'))
#     names(x)[1] <- "ecodistrict"
#     x <- melt(x, id = "ecodistrict", variable.name = "time", value.name = "caribouRS_mean")
#     
#     ### computing HQH thresholds
#     v <- values(caribouRS[[1]])
#     v <- v[!is.na(v)]
#     quantiles <- c(0.5, 0.75, 0.9, 0.95)
#     threshold <- quantile(v, quantiles)
#     names(threshold) <- paste0("h", quantiles*100)
#     
#     for (q in seq_along(threshold)) {
#         y <- caribouRS>threshold[q]
#         y <- as.data.frame(zonal(y, ecodistricts, fun = 'sum'))
#         y[,-1] <- y[,-1] / ecodistPixelsN[,"sum"]
#         colnames(y)[1] <- "ecodistrict"
#         y <- melt(y, id = "ecodistrict", variable.name = "time", value.name = names(threshold)[q])
#         x <- merge(x, y)
#     }
#     
#     x$time <- as.numeric(gsub("[^0-9]", "", x$time))
#     x <- data.frame(area = a,
#                     scenario = s,
#                     treatment = t,
#                     replicate = r,
#                     x)
#     return(x)
# }
# stopCluster(cl)
# summary(caribouRS_ecodist_mean)
# 
# ### a little tyding up before saving to file
# caribouRS_ecodist_mean$ecodistrict <- as.factor(caribouRS_ecodist_mean$ecodistrict)
# caribouRS_ecodist_mean$replicate <- as.factor(caribouRS_ecodist_mean$replicate)
# df <- caribouRS_ecodist_mean
# fire <- factor(rep("Fire regime: baseline", nrow(df)), levels = c("Fire regime: baseline", "Fire regime: projected"))
# fire[grep("Projected", df$treatment)] <- "Fire regime: projected"
# harvest <- factor(rep("Harvesting level: 100%", nrow(df)), levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
# harvest[grep("0.5", df$treatment)] <- "Harvesting level: 50%"
# 
# caribouRS_ecodist_mean <- data.frame(caribouRS_ecodist_mean, fire, harvest)
# ########################################################################################
# save(caribouRS_ecodist_mean, file = "caribouRS_ecodist_mean_DF.RData")
# ########################################################################################
# ########################################################################################


# ########################################################################################
# ########################################################################################
# ### computing ecodistrict averages, storing into tidy data frame
# require(doSNOW)
# require(raster)
# require(reshape2)
# #outputDir <- "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/"
# outputDir <- "/media/dcyr/Data/caribouRsOutputs/"
# outputFiles <- list.files(outputDir)
# simInfo <- strsplit(gsub(".RData", "", outputFiles), "_")
# ecodistricts <- raster("../gis/ecodistricts.tif")
# ########################################################################################
# nClusters <- 10 ## takes less memory than for ecodistricts (~2.5 Gb by thread)
# cl <- makeCluster(nClusters)
# registerDoSNOW(cl)
# 
# caribouRS_mean <- foreach(i = seq_along(outputFiles), .combine = "rbind")  %dopar% {#
#     require(raster)
#     require(reshape2)
#     a <- simInfo[[i]][2]
#     s <- simInfo[[i]][3]
#     t <- simInfo[[i]][4]
#     r <- as.numeric(simInfo[[i]][5])
# 
#     ### loading raster stack
#     caribouRS <- get(load(paste(outputDir, outputFiles[i], sep = "/")))
#     time <- as.numeric(gsub("[^0-9]", "", names(caribouRS)))
#     x <- list()
#     for (l in 1:nlayers(caribouRS)) {
#         v <- values(caribouRS[[l]])
#         v <- v[!is.na(v)]
#         if(l == 1) {
#             threshold <- quantile(v, c(0.5, 0.75, 0.9, 0.95))
#             npixel <- length(v)
#         }
# 
#         x[[l]] <- data.frame(area = a,
#                         scenario = s,
#                         treatment = t,
#                         time = time[l],
#                         replicate = r,
#                         caribouRS_mean = mean(v),
#                         h50 = sum(v>threshold[1])/npixel,
#                         h75= sum(v>threshold[2])/npixel,
#                         h90 = sum(v>threshold[3])/npixel,
#                         h95 = sum(v>threshold[4])/npixel)
# 
#     }
#     x <- do.call("rbind", x)
# 
#     return(x)
# }
# stopCluster(cl)
# ### a little tyding up before saving to file
# 
# caribouRS_mean$replicate <- as.factor(caribouRS_mean$replicate)
# df <- caribouRS_mean
# 
# fire <- factor(rep("Fire regime: baseline", nrow(df)), levels = c("Fire regime: baseline", "Fire regime: projected"))
# fire[grep("Projected", df$treatment)] <- "Fire regime: projected"
# harvest <- factor(rep("Harvesting level: 100%", nrow(df)), levels = c("Harvesting level: 50%", "Harvesting level: 100%"))
# harvest[grep("0.5", df$treatment)] <- "Harvesting level: 50%"
# caribouRS_mean <- data.frame(caribouRS_mean, fire, harvest)
# 
# ########################################################################################
# save(caribouRS_mean, file = "caribouRS_mean_DF.RData")
# ########################################################################################
# ########################################################################################



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

for (i in c("total")) {#c("ecodistrict", "total")) {
    if(i == "ecodistrict") {
        df <- caribouRS_ecodist_mean
        
        df <- mutate(df, id = as.numeric(as.factor(paste(scenario, treatment, ecodistrict, replicate))))
        plotDim <- c(2400,2400)
        pRatio <- 1.2
    }
    if (i == "total") {
        df <- caribouRS_mean
        df <- df %>%
            mutate(id = as.numeric(as.factor(paste(scenario, treatment, replicate)))) %>%
            select(id, scenario, fire, harvest, time, caribouRS_mean, h75)
        ## removing incomplete simulations
        idSubsample <- df %>%
            filter(time == 100)
        idSubsample <- unique(idSubsample$id)
        df <- filter(df, id %in% idSubsample)
        
        ## for nicer plotting
        plotDim <- c(1400, 900)
        pRatio <- 0.75
        df$h75 <- df$h75 * 100
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
                      size = 0.3,
                      alpha = 0.75) +
            scale_colour_manual(values = cols,
                                guide = guide_legend(#label.position = "bottom", label.hjust = 0.5,
                                                     override.aes = list(size = 2,
                                                                         alpha = 1))) +
            theme_linedraw() +
            theme(legend.position="top", legend.direction="horizontal",
                  plot.title = element_text(size = rel(1)),
                  axis.title.x=element_blank()) +
            labs(#title = "Evolution of Caribou habitat quality",
                 y = "Average RS call\n")
        
        if(i == "ecodistrict") {
            g <- g + facet_wrap(~ ecodistrict)
        }
        
        fName <- paste0("caribouRS_mean_", i, "_", t ,".png")
        fileNameMean <- append(fileNameMean, fName)
        png(filename = fName,
            width = plotDim[1], height = plotDim[2], res = 300)
        
            print(g + theme(axis.text.x = element_text(size = rel(1), angle = 45, hjust = 1),
                            axis.text.y = element_text(size = rel(1)),
                            axis.title = element_text(size = rel(0.75)),
                            legend.title = element_blank(),
                            legend.text = element_text(size = rel(0.75))))
            
        
        dev.off()
        
        #####################################################################################
        ### habitat
        
        
        #segDF <- distinct(df[, c("scenario", "fire", "harvest")])
       if (i == "total") {
            g <- ggplot(df, aes_string(group = "id", color = t)) +
                geom_line(aes_string(x = "df$time", y = "df$h75"),
                          stat="smooth",method = "loess", span = 0.4,
                          size = 0.3,
                          alpha = 0.75) +
                scale_colour_manual(values = cols,
                                    guide = guide_legend(override.aes = list(size = 2,alpha = 1))) +
                theme_linedraw() +
                theme(legend.position="top", legend.direction="horizontal",
                      axis.title.x=element_blank()) +
                labs(#title = "Relative abundance of high quality habitats (HQH*)",
                     y = "Relative abundance\n(% of landscape)\n") #+
            
            ### coordinate of the description
            yLabel <- ggplot_build(g)$panel$ranges[[1]]$y.range[2]
            
            fName <- paste0("caribouRS_HQH_", i, "_", t ,".png")
            fileNameHQH <- append(fileNameHQH, fName)
            png(filename= fName,
                width = plotDim[1], height = plotDim[2], res = 300)
            
                print(g + theme(axis.text.x = element_text(size = rel(1), angle = 45, hjust = 1),
                                axis.text.y = element_text(size = rel(1)),
                                axis.title = element_text(size = rel(0.75)),
                                legend.title = element_blank(),
                                legend.text = element_text(size = rel(0.75)))) #+
                          # annotate("text", x = min(df$time) + 1, y = yLabel, label = c("*HQH: As defined by the best quartile in initial landscape."),
                          #          color = "grey25", size  = 2, hjust = 0, vjust = 1))
                
            
            dev.off()
        }
    }
    # fileNameMean <- paste(getwd(), fileNameMean, sep = "/")
    # 
    # require(animation)
    # oopt = ani.options(ani.dev="png", ani.type="png", interval = 1.5, autobrowse = FALSE)
    # ### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
    # im.convert(fileNameMean, output = paste0("caribouRS_mean_", i, "_anim.gif"),
    #            extra.opts = "", clean = F)
    # ### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
    # 
    # if(i == "total") {
    #     fileNameHQH <- paste(getwd(), fileNameHQH, sep = "/")
    #     im.convert(fileNameHQH, output = paste0("caribouRS_HQH_", i, "_anim.gif"),
    #            extra.opts = "", clean = F)
    # }
}


    
 
    
# ##################
# 
# r <- ecodistricts
# rDF <- rasterToPoints(r)
# df <- data.frame(rDF)
# df$ecodistricts <- as.factor(df$ecodistricts)
# 
# ##################
# ##################
# #######  Plotting
# 
# require(RColorBrewer)
# 
# meanX <- as.matrix(by(df$x, df$ecodistricts, mean, na.rm = TRUE))
# meanY <- as.matrix(by(df$y, df$ecodistricts, mean, na.rm = TRUE))
# 
# coordLab <- data.frame(X = meanX, Y = meanY, label = rownames(meanY))
# 
# colEcodistrict <- brewer.pal(8, "Dark2")
# colEcodistrict <- rep(colEcodistrict, 2)
# 
# g <- ggplot(data = df, aes(x, y, fill = ecodistricts)) +
#     theme_bw() +
#     geom_raster() +
#     annotate("text", x = coordLab$X, y = coordLab$Y, label = coordLab$label,
#              color = "grey25", size  = 3) +
#     #geom_text(data = NULL, aes(label = coordLab$label, x = coordLab$X, y = coordLab$Y)) +
#     coord_equal() +
#     scale_fill_manual(values = colEcodistrict) 
#                       #guide = guide_legend(title = "Ecodistrict",
#                       #                     reverse = TRUE))
# 
# png(filename = "ecodistricts.png",
#     width = 1200, height = 1200,
#     res = 300)
# 
#     print(g + theme(plot.title = element_text(size = rel(0.8)),
#                     axis.title.x = element_blank(),
#                     axis.title.y = element_blank(),
#                     axis.ticks = element_blank(),
#                     axis.text.x = element_blank(),
#                     axis.text.y =  element_blank(),
#                     strip.background = element_blank(),
#                     strip.text.x = element_blank(),
#                     legend.text = element_text(size = rel(0.5)),
#                     legend.title = element_text(size = rel(0.7))) +
#               labs(title = "Ecodistricts") +
#               guides(fill=FALSE))
#         
#               
# dev.off()




