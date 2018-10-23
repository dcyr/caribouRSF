###### Dominic Cyr, in collaboration with Yan Boulanger and Martin-Hugues St-Laurent
rm(list = ls())
setwd("E:/SCF/CBFA/caribou")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(dplyr)

omega_sq <- function(aov_in, neg2zero=T){
    aovtab <- summary(aov_in)[[1]]
    n_terms <- length(aovtab[["Sum Sq"]]) - 1
    output <- rep(-1, n_terms)
    SSr <- aovtab[["Sum Sq"]][n_terms + 1]
    MSr <- aovtab[["Mean Sq"]][n_terms + 1]
    SSt <- sum(aovtab[["Sum Sq"]])
    for(i in 1:n_terms){
        SSm <- aovtab[["Sum Sq"]][i]
        DFm <- aovtab[["Df"]][i]
        output[i] <- (SSm-DFm*MSr)/(SSt+MSr)
        if(neg2zero & output[i] < 0){output[i] <- 0}
    }
    names(output) <- rownames(aovtab)[1:n_terms]
    
    return(output)
}


####################################################################################
####################################################################################
#########
#########   Fetching results
#########
############################################################
############################################################
## caribouRS_ecodist_mean <- get(load("../processedOutputs/caribouRS_ecodist_mean_DF.RData"))
caribouRS_mean <- get(load("../processedOutputs/caribouRS_mean_DF.RData"))


### filtering out some treatments
caribouRS_mean <- caribouRS_mean %>%
    filter(budworm == "Budworm",
           fire != "No fires")

summary(caribouRS_mean)
distinct(caribouRS_mean[, c("scenario", "fire", "harvest", "growth")])


#################################################################################
## Performing variance partitionning for all years and species
ts <- 30 ## final timesteps, simulation is omitted if timestep unavailable



#################################################################################
### average RSF call
omega_sq_results <- omega_sqP_results <- list()
for (v in c("caribouRS_mean", "h75")) {
    omega_sq_results[[v]] <- omega_sqP_results[[v]] <- list()
    for (s in c("RCP26", "RCP45", "RCP85")) {
        
        df <- caribouRS_mean %>%
            filter(time == ts,
                   scenario %in% c("baseline", s))
        
        ### ANOVA
        model <- aov(df[,v] ~ fire + growth + harvest , data = df)
        ### calculation of omega-squared
        omega_sq_results[[v]][[s]] <- omega_sq(model, neg2zero=T)
    }  
}




require(reshape2)
results <- as.data.frame(omega_sq_results)
results <- rbind(results, apply(results, 2, function(x) 1-sum(x)))
rownames(results)[nrow(results)] <- "residuals"
results <- data.frame(results, variable = gsub(" ", "", rownames(results)))
results <- melt(results, variable.name = "response")
results[,"scenario"] <- gsub("caribouRS_mean.|h75.", "", results$response)
results$variable <- factor(results$variable, levels = c("fire", "harvest", "growth", "residuals"))
results$response <- gsub(".RCP26|.RCP45|.RCP85", "", results$response)
responseName <- c(caribouRS_mean = "Average conditions",
                  h75 = "Abundance of HQH (top 25%)")
results$response <- factor(responseName[results$response], levels = as.character(responseName))



require(ggplot2)
cols <- c("darkred", "darkgoldenrod3", "palegreen3", "grey25")#"aquamarine3")


p <- ggplot(aes(y=value, x=scenario, fill = variable), data = results) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(0,1), linetype = 1, colour = "grey25") +
    geom_col(position = position_stack(reverse = T)) +
    facet_wrap( ~ response) +
    # coord_flip() +
    geom_text(aes(label=paste0(round(100*value,1), "%")),
              position = position_stack(reverse = T), angle = 0, vjust = 1.25, hjust = 0.5,
              check_overlap = T, size = rel(2.5), colour = "white") +
    # geom_text(aes(x = c(1,2,1,2), y = 1, label=paste0("residual\n12%")),
    #           angle = 0, vjust = 1.25, hjust = 0.5,
    #           check_overlap = T, size = rel(2.5), colour = "white") +
    scale_fill_manual('', values = cols,
                      guide = guide_legend(reverse=TRUE))



png(filename = paste0("variationDecomp_", ts, ".png"),
    width = 8, height = 5, units = "in", res = 300, pointsize=8)

print(p +
          theme_dark()+
          labs(title ="Variation partitioning of caribou habitat quality",
               subtitle = paste0("After ", ts, " years of simulation (horizon ", 2000+ts, ")"),
               caption = "*Values < 0.001 not shown",
               x = "",
               y = expression(omega^2 ~ "*")) +#"Omega squared*\n") +
          theme(#legend.position="top", legend.direction="horizontal",
              # axis.text.x = element_text(angle = 45, hjust = 1),
              plot.caption=element_text(size=rel(0.7)))
)


dev.off()





## performing variance partitionning Borcard-Style
## (based on multiple linear regression (univariate response), or RDA (multivariate response))

for (s in c("RCP26", "RCP45", "RCP85")) {
    df <- caribouRS_mean %>%
        filter(time == ts)#,
               #scenario %in% c("baseline", s))
    
    ## structuring data for varpart
    
    # response variable (can be a matrix, here it's only a vector)
    Y <- data.frame(caribouRS_mean = df$caribouRS_mean)
    X <- df[,c("scenario", "fire", "growth", "harvest")]
    
    
    
    require(vegan)
    part <- varpart(Y, ~scenario, ~growth, ~fire, ~harvest, data = X)
    plot(part)
    #showvarparts(part)
                
    # classic Venn
    cutoff <- 0.001
    png(filename = paste0("vennDiagClassic_", s, ".png"),
        width = 5, height = 4, units = "in", res = 600, pointsize=8)
    
        plot(part, main = "foo",
             Xnames = c("scenario",
                        "Growth",
                        "Fire",
                        "Harvest"),
             cutoff = cutoff, digits = 2)
    
    dev.off()
}

