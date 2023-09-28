# midlineLM()
# Calculate midline landmark between two 3D landmarks across an array

# Given a matrix of landmark pairs and an array organized p x k x n (landmarks, 
# dimensions, specimens), calculates the midline ladnmark between those landmarks 
# across all specimens.

# 09/28/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.3.1, RStudio 2023.6.0.421
# Windows 10 x64

# SOURCES: Conversion into matrix code from 
#          https://github.com/cran/geomorph/blob/master/R/two.d.array.r

# @param data           Array of landmark data
# @param pairs          Matrix of landmark pairs, such that each row represents
#                       two landmarks you wish to calculate midline landmark
#                       between.
# @param two.d          Logical, whether to return a 3D or a 2D array.
#                       Default: FALSE

# @return               A named array or matrix of midline landmark positions.

# @examples
# library(geomorph)
# data(scallops)
# scalLMs <- scallops$coorddata
# scalPairs <- scallops$land.pairs

# midlineLM(scalLMs, pairs = scalPairs)

midlineLM <- function(data = NA, pairs, two.d = FALSE){
  if(is.null(row.names(pairs))){row.names(pairs) <- 1:nrow(pairs)}
  
  results <- array(data = NA, dim = c(nrow(pairs), dim(data)[2:3]),
                   dimnames = list(row.names(pairs), dimnames(data)[[2]],
                                   dimnames(data)[[3]])
                   )
  
  
  for(i in dimnames(data)[[3]]){
    for(j in dimnames(data)[[2]]){
      for(k in row.names(pairs)){
        results[k,j,i] <- mean(c(data[pairs[k,1],j,i], data[pairs[k,2],j,i]))
      }
    }
  }
  
  if(two.d == FALSE){return(results)}
  else{
    pxk <- dim(results)[1]*dim(results)[2]
    n <- dim(results)[3]
    tmp <- aperm(results, c(3,2,1))
    dim(tmp) <- c(n,pxk)
    rownames(tmp)<-dimnames(results)[[3]] 
    colnames(tmp)<-as.vector(t(outer(dimnames(results)[[1]], 
                                     dimnames(results)[[2]], 
                                     FUN = paste, sep=sep)))
    return(tmp)
  }
}
