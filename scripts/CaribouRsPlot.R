### initial biomass plot

caribouRS <- get(load("/media/dcyr/Seagate Backup Plus Drive/caribouRsOutputs/caribouRS_LSJ_baseline_BudwormBaselineFireHarvest_1.RData"))
setwd("/home/dcyr/Travail/SCF/CBFA/caribou")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(ggplot2)
require(colorRamps)
require(raster)
filenames <- character()
for (i in 1:nlayers(caribouRS)) {
    r <- caribouRS[[i]]
    ts <- as.numeric(gsub("[^0-9]", "", names(r)))
    caribouRS_P <- rasterToPoints(r)
    df <- data.frame(caribouRS_P)
    rangeX <- range(df$x)
    rangeY <- range(df$y)
    
    
    p <- ggplot(data = df, aes_string("x", "y", fill = colnames(df)[3])) +
        theme_bw() +
        theme(legend.position="top", legend.direction="horizontal") +
        geom_raster() +
        # ?geom_text(data = NULL, x = maxX, y = maxY,
        #           label = paste0("Time =", ts)) +
        annotate("text", label = paste0("Time = ", ts), x = rangeX[2], y = rangeY[2], hjust = 1, size = 6, colour = "darkred") +
        annotate("text", label = paste0("Climate: baseline"), x = rangeX[2], y = rangeY[1]+0.96*(rangeY[2]-rangeY[1]), hjust = 1,  colour = "black") +
        annotate("text", label = paste0("Fire regime: baseline"), x = rangeX[2], y = rangeY[1]+0.93*(rangeY[2]-rangeY[1]), hjust = 1,  colour = "black") +
        annotate("text", label = paste0("Harvest level: 100%"), x = rangeX[2], y = rangeY[1]+0.9*(rangeY[2]-rangeY[1]), hjust = 1,  colour = "black") +
        scale_fill_gradientn(name = "Prob. occurence",  limits =c(0,0.85),
                             #colours = c("white", "palegreen3", "gold2", "darkred"),
                             colours = c("white", "lightblue", "seagreen4", "gold2", "darkred"),
                             #colours = c(matlab.like2(5)),
                             values = c(0,  0.25, 0.5, 0.75, 1)) +
        coord_equal()
    
    filenames <- append(filenames, paste0("caribouRS_LSJ_", i, ".png"))
    png(filename = filenames[i],
        width = 6, height = 8, units = "in", res = 150, pointsize = 10,
        bg = "white")
    
        print(p + ggtitle("Relative probability of occurence of Caribou") +
    
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y =  element_blank(),
                        strip.background = element_blank(),
                        strip.text.x = element_blank()),
              plot.margin = unit( c(0,0,2,0) , units = "lines" ))
    #strip.text.x = element_text(size=6)))
    dev.off()
}

require(animation)
oopt = ani.options(ani.dev="png", ani.type="png", interval = 0.5, autobrowse = FALSE)
### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
im.convert(filenames, output = "test.gif",
           extra.opts = "", clean = F)



# 
# # histogram
# foo <- biomass[[1]]
# foo[foo == 0] <- NA
# 
# png(filename = paste0("initialBiomassDistrib_LSJ.png"),
#     width = 800, height = 600,
#     bg = "white")
# 
#     hist(values(foo), freq=F,
#          main = "Distribution of initial biomass\n(active pixels, after LANDIS Spinup phase)",
#          xlab = "tons/ha")
#     #abline(v = mean(values(foo), na.rm = T), col = "blue")
# 
# dev.off()

############
############
############        # require(RColorBrewer)
# # foo[foo<0] <- 0
# # foo[foo>1] <- 1
# # 
# png(filename = "caribouResults.png", width = 1024, height = 1024,
#     units = "px", pointsize = 12)
#     
#     plot(foo, breaks = seq(0, 1, 0.1), col = brewer.pal(10, "RdYlGn"),
#          main = "Caribou resource selection probability\n(initial conditions)")
# 
# dev.off()
# 
# 
# 
# png(filename = "roadsDecayDist.png", width = 1024, height = 1024,
#     units = "px", pointsize = 12)
#     
#     plot(roadsDecayDist,
#          breaks = seq(0, 1, 0.1),
#          col = brewer.pal(11, "RdYlBu"),
#          main = "Decaying distance to road\n")   
# 
# dev.off()