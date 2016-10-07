rm(list = ls())
setwd("~/Travail/SCF/CBFA/caribou")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(ggmap)
require(rgdal)

studyArea <- readOGR(dsn = "../data", layer = "LSJ_Caribou_Extent")
studyArea <- spTransform(studyArea, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyArea)
studyAreaR <- raster("../initRasters/areaExtentCaribou.tif")
totalArea <- sum(values(studyAreaR), na.rm = T) * 6.25



############################################################
############################################################
#########
#########   Study area map
#########
############################################################
############################################################

fond <- get_map(location = "Lac St-Jean", zoom = 6)

map <- ggmap(fond) +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'black', fill = 'black', alpha = .3, size = .3)

xBreaks <- seq(from = floor(min(studyAreaF$long)), to = ceiling(max(studyAreaF$long)), by = 1)
yBreaks <- seq(from = floor(min(studyAreaF$lat)), to = ceiling(max(studyAreaF$lat)), by = 1)

xRange <- as.numeric(attr(fond, "bb")[c(2,4)])
yRange <- as.numeric(attr(fond, "bb")[c(1,3)])

png(filename = "studyArea.png",
    width = 2000, height = 2000, units = "px", res = 300, pointsize = 12,
    bg = "white")

    print(map + ggtitle("CBFA Caribou - Study area") +
              annotate("text", label = paste("Total area:", signif(totalArea, 3)/1000000, "Mha"),
                       x = xRange[2] - 0.2, y = yRange[2] + 0.1, hjust = 1, vjust = 0, size = 4, colour = "black") +
              geom_vline(xintercept = xBreaks, color = "white", alpha = 0.25, size = 0.25) +
              geom_hline(yintercept = yBreaks, color = "white", alpha = 0.25, size = 0.25) +

              scale_x_continuous(breaks = xBreaks,
                                 minor_breaks = waiver()) +
              scale_y_continuous(breaks = yBreaks,
                                 minor_breaks = waiver()) +
              theme(plot.title = element_text(size = rel(1.2)),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_line(size = 0.25))
          )

dev.off()


############################################################
############################################################
#########   
#########   Results (test)
#########   
############################################################
############################################################




simDir <- "/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs"
simOutputs <- list.files(simDir, full.names = T)
caribouRsTreatments <- unique(substr(simOutputs, 1, nchar(simOutputs)-8 ))
ts <- 100

for (i in seq_along(caribouRsTreatments)) {
    treat <- caribouRsTreatments[i]
    tName <- basename(treat)
    fName <- simOutputs[grep(paste(paste(treat, "_", c(1:5), sep = ""), collapse = "|"), simOutputs)]
    simInfo <- unlist(strsplit(tName, "_"))

    scenName  <- simInfo[3]
    harvName <- ifelse(grepl("0.5", simInfo[4]), "harvest50", "harvest100")
    fireName <- ifelse(grepl("Baseline", simInfo[4]), "fireBaseline", "fireProjected")
    
    print(paste("processing", tName))
    for (j in seq_along(fName)) {
        caribouRS <- get(load(fName[j]))
        
        x <- which(names(caribouRS) == paste0("caribouRS_", ts))
        ## check if layer exists
        if(!length(x) == 1) next
            x <- caribouRS[[x]]

        if(j == 1) {
            tmp <- x
            layerName <- names(x)
            
        }
        else {
            tmp <- stack(tmp, x)
        }
        print(paste("replicate", j, "of", length(fName)))
    }
    
    layerName <- gsub("caribouRS", "caribouRsMean", layerName)
    layerName <- paste(layerName, scenName, fireName, harvName, sep = "_")
    tmp <- mean(tmp)
    names(tmp) <- layerName
    if(i == 1) {
        caribouRsMeanFinal <-  tmp 
    } else {
        caribouRsMeanFinal <- stack(caribouRsMeanFinal, tmp)
    }
}

save(caribouRsMeanFinal, file = "caribouRsMeanFinal.RData")




# ########################################################################
# ########################################################################
# ## plotting results with google map base layer
# ## 
# ########################################################################
# ########################################################################
# caribouRS <- get(load(simOutputs))
# caribouRS <- projectRaster(caribouRS, crs = CRS("+init=epsg:4326"))
# 
# box <- bbox(caribouRS)
# box[[1]] <- box[[1]]-1
# 
# caribouRS <- rasterToPoints(caribouRS)
# caribouRS <- data.frame(caribouRS)
# 
# fond <- get_map(location = box)
# class(fond)
# class(caribouRS)
# 
# rangeX <- range(caribouRS$x)
# rangeY <- range(caribouRS$y)
# 
# layers <- colnames(df)[3:ncol(caribouRS)]
# timesteps <- as.numeric(gsub("[^0-9]", "", layers))
# filenames <- character()
# for (i in seq_along(layers)) {
#     v <- layers[i]
#     ts <- timesteps[i]
# 
#     map <- ggmap(fond) +
#         geom_point(data=caribouRS, aes_string(x="x", y="y", color = v), shape=15, size=0.01, alpha = 1) +
#         scale_color_gradientn(name = "RSF call",  limits =c(0,0.85),
#                               colours = c("white", "lightblue", "seagreen4", "gold2", "darkred"),
#                               values = c(0,  0.25, 0.5, 0.75, 1)) +
#         annotate("text", label = paste0("Time = ", ts),
#                  x = rangeX[1], y = rangeY[2], vjust = 1, hjust = 0, size = 2, colour = "darkred") +
#         annotate("text", label = paste0("Climate: ", tolower(s)),
#                  x = rangeX[1], y = rangeY[1]+0.96*(rangeY[2]-rangeY[1]), vjust = 1, hjust = 0, size = 1.5, colour = "black") +
#         annotate("text", label = paste0("Fire regime: ", tolower(f)),
#                  x = rangeX[1], y = rangeY[1]+0.93*(rangeY[2]-rangeY[1]), vjust = 1, hjust = 0, size = 1.5, colour = "black") +
#         annotate("text", label = paste0("Harvest level: ", ifelse(h == "0.5", "50%", "100%")),
#                  x = rangeX[1], y = rangeY[1]+0.9*(rangeY[2]-rangeY[1]), vjust = 1, hjust = 0, size = 1.5, colour = "black")
#     
#     fName <- paste0("rsfCall_", i, ".png")
#     filenames <- append(filenames, fName)
#     
#     png(filename = fName,
#         width = 1024, height = 800, units = "px", res = 300, pointsize = 6,
#         bg = "white")
#     
#         print(map + # ggtitle("Study area") +
#                   
#                   theme(plot.title = element_blank(), #element_text(size = rel(1.2)),
#                         axis.title.x = element_blank(),
#                         axis.title.y = element_blank(),
#                         axis.ticks = element_blank(),
#                         axis.text.x = element_blank(),
#                         axis.text.y =  element_blank(),
#                         strip.background = element_blank(),
#                         strip.text.x = element_blank(),
#                         legend.text = element_text(size = rel(0.5)),
#                         legend.title = element_text(size = rel(0.7))))
#     dev.off()
#     
# }
# 
# require(animation)
# oopt = ani.options(ani.dev="png", ani.type="png", interval = 0.5, autobrowse = FALSE)
# ### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
# im.convert(filenames, output = "example.gif",
#            extra.opts = "", clean = F)


