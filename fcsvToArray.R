fcsvToArray <- function(file = NULL, dir = NULL, lms = NULL, nams = NULL){
  if(is.null(file) && !is.null(dir)){
    file = list.files(path = dir, pattern = "fcsv", full.names = TRUE)
  }
  
  if(is.null(lms)){
    temp = read.csv(file = file[1], skip = 3, header = FALSE, row.names = 12)
    lms = rownames(temp)
  }
  
  if(is.null(nams)){nams = list.files(path = dir, pattern = "fcsv")}
  
  LM = array(data = NA, 
             dim = c(length(lms), 3, length(file)),
             dimnames = list(lms, c("X", "Y", "Z"), file))
  
  for(i in 1:length(file)){
      temp = read.csv(file = file[[i]], skip = 3, header = FALSE, row.names = 12)
      LM[,,i] = as.matrix(temp[lms,2:4])
  }
  
  dimnames(LM)[[3]] = nams
  return(LM)
}
