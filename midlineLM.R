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
    # SOURCE: https://github.com/cran/geomorph/blob/master/R/two.d.array.r
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
