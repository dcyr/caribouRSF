##################################################################
##################################################################
#### Dominic Cyr
rm(list =ls())
setwd("~/Travail/SCF/CBFA/caribou")
##################################################################
wwd <- (paste(getwd(), Sys.Date(), sep ="/"))
dir.create(wwd)
setwd(wwd)
##################################################################



########################################################################################
require(raster)
require(reshape2)

ecodistricts <- raster("../gis/ecodistricts.tif")
r <- !is.na(ecodistricts)
ecodistPixelsN <- zonal(r, ecodistricts, "sum")


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
        
        df <- df %>%
            filter(budworm == "Budworm",
                   fire != "No fires") %>%
            mutate(id = as.numeric(as.factor(paste(scenario, treatment, ecodistrict, replicate))))
        
        plotDim <- c(2400,2400)
        pRatio <- 1.2
    }
    if (i == "total") {
        df <- caribouRS_mean
        df <- df %>%
            filter(budworm == "Budworm",
                   fire != "No fires") %>%
            mutate(id = as.numeric(as.factor(paste(scenario, treatment, replicate))))
        plotDim <- c(1400,1000)
        pRatio <- 0.75
        df[,c("h75", "h90")] <- df[,c("h75", "h90")] * 100
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
            cols <-  c("darkolivegreen","brown4")
            lTitle = ""
        }
        if (t == "harvest") {
            cols <- c("cadetblue4","tan3", "sienna4")
            lTitle = ""
        }
        
        g <- ggplot(df, aes_string(x = "time", y = "caribouRS_mean", group = "id", color = t)) +
            geom_line(stat="smooth",method = "loess", span = 0.4,
                      size = 0.5,
                      alpha = 0.75) +
            scale_colour_manual(values = cols,
                                guide = guide_legend(title = lTitle,
                                                     title.theme = element_text(size = 10 * pRatio, angle = 0),
                                                     label.theme = element_text(size = 10 * pRatio, angle = 0),
                                                     #label.position = "bottom", label.hjust = 0.5,
                                                     override.aes = list(size = 2,
                                                                         alpha = 1))) +
            theme_linedraw() +
            theme(legend.position="top", legend.direction="horizontal",
                  plot.title = element_text(size = 12 * pRatio),
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
        
        print(g + theme(axis.text.x = element_text(size = 12 * pRatio, angle = 45, hjust = 1),
                        axis.text.y = element_text(size = 12 * pRatio),
                        strip.text = element_text(size = 12 * pRatio)))
        
        
        dev.off()
        
        #####################################################################################
        ### habitat
        
        
        #segDF <- distinct(df[, c("scenario", "fire", "harvest")])
        if (i == "total") {
            g <- ggplot(df, aes_string(group = "id", color = t)) +
                geom_line(aes_string(x = "df$time", y = "df$h75"),
                          stat="smooth",method = "loess", span = 0.4,
                          size = 0.5,
                          alpha = 0.75) +
                # geom_line(aes_string(x = "df$time", y = "df$h90"),
                #           stat="smooth",method = "loess", span = 0.4,
                #           size = 0.5,
                #           alpha = 0.75) +
                scale_colour_manual(values = cols,
                                    guide = guide_legend(title = lTitle,
                                                         title.theme = element_text(size = 10 * pRatio, angle = 0),
                                                         label.theme = element_text(size = 10 * pRatio, angle = 0),
                                                         override.aes = list(size = 2,
                                                                             alpha = 1))) +
                theme_linedraw() +
                theme(legend.position="top", legend.direction="horizontal",
                      plot.title = element_text(size = rel(pRatio)),
                      axis.title = element_text(size = rel(pRatio)),
                      axis.title.x=element_blank()) +
                labs(title = "Relative abundance of high quality habitats",
                     y = "Relative abundance (% of landscape)\n") #+
                # annotate("text", x = min(df$time) + 1, y = c(9, 24), label = c("Initial best 10%", "Initial best 25%"),
                #          color = "grey25", size  = 2, hjust = 0, vjust = 1)
            
            
            fName <- paste0("caribouRS_HQH_", i, "_", t ,".png")
            fileNameHQH <- append(fileNameHQH, fName)
            png(filename= fName,
                width = plotDim[1], height = plotDim[2], res = 300)
            
            print(g + theme(axis.text.x = element_text(size = 12 * pRatio, angle = 45, hjust = 1),
                            axis.text.y = element_text(size = 12 * pRatio),
                            strip.text = element_text(size = 12 * pRatio)))
            
            
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



