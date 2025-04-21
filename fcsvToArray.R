fcsvToArray <- function(file, dir, lms = NULL, nams = NULL){
    fs = paste(dir, file, sep = "/")

    if(is.null(lms)){
        temp1 = read.csv(file = fs[1], skip = 3, header = FALSE, row.names = 12)
        lms = rownames(temp1)
    }
    
    LM = array(data = NA, 
               dim = c(length(lms), 3, length(fs)),
               dimnames = list(lms, c("X", "Y", "Z"), fs))
    
    for(i in 1:length(fs)){
        temp2 = Morpho::read.fcsv(fs[[i]])
        row.names(temp2) = lms
        LM[,,i] = as.matrix(temp2[lms,])
    }
    
    if(is.null(nams)){dimnames(LM)[[3]] = fs} else{dimnames(LM)[[3]] = nams}
    
    return(LM)
}
