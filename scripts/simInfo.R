
simInfoFnc <- function(x){
    #### listing sim folders, sim infos
    simDir <- scenario <- treatment <- replicate <- simArea <- character()
    
    dirTmp1 <- list.dirs(x, recursive=FALSE)
    
    for (i in seq_along(dirTmp1)){
        dirTmp2 <- list.dirs(dirTmp1[i], recursive=FALSE)
        for (j in seq_along(dirTmp2)){ # j <- 1
            dirTmp3 <- list.dirs(dirTmp2[j], recursive=FALSE)
            for (r in seq_along(dirTmp3)) {
                dirTmp3 <- gsub(x, ".", dirTmp3[r])
                scenario <- append(scenario, strsplit(dirTmp3[i], "/")[[1]][2])
                treatment <- append(treatment, strsplit(dirTmp3[j], "/")[[1]][3])
                replicate <- append(replicate, strsplit(dirTmp3[r], "/")[[1]][4])
                #simArea <- append(simArea, studyArea)
                simDir <- dirTmp3
            }
        }
    }
    scenario <- as.factor(scenario)
    treatment <- as.factor(treatment)
    replicate <- as.factor(replicate)
    
    simInfo <- list(simDir = simDir,
                    scenario = scenario,
                    treatment = treatment,
                    replicate = replicate)
    return(simInfo)
}