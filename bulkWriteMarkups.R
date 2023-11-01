# bulkWriteMarkups()
# Write .fcsv files for each specimen in a sample

# Given an array of coordinate data or a .csv file, write corresponding 
# .fcsv files for each specimen.

# 11/01/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.3.2, RStudio 2023.9.1.494
# Windows 10 x64

# SOURCE:         An expansion on Murat Maga's write.markups.fcsv
#                 https://github.com/muratmaga/SlicerMorph_Rexamples/blob/main/write.markups.fcsv.R

# @param pts      A 3D array of coordinate data in the local environment.
# @param file     A .csv file of coordinate data to be processed by 
#                 geomorph's arrayspecs function.
# @param names    Names for each of the specimens.
#                 Default: dimnames(pts)[[3]]
# @param prefix   Optional prefixes to be added to file names.
# @param suffix   Optional suffixes to be added to the file names.
# @param sep      Argument to be passed to geomorph's arrayspecs function
#                 if necessary. 
#                 Default: "."

# @return         One .fcsv file for each specimen.

bulkWriteMarkups <- function(pts = NULL, file = NULL, names = dimnames(pts)[[3]], prefix = NULL, suffix = NULL, sep = "."){
  
  if(is.null(pts) && !is.null(file)){
    message(paste0("Reading ", file))
    pts <- read.csv(file = file, header = TRUE, row.names = 1, check.names = FALSE)
    
    if(length(grep(paste0(".","Z"), colnames(temp)[3])) == 1){k = 3}
    if(length(grep(paste0(".","X"), colnames(temp)[3])) == 1){k = 2}
    
    require(geomorph)
    pts <- geomorph::arrayspecs(A = pts, p = ncol(pts)/k, k = k, sep = sep)
  }
  
  labels = dimnames(pts)[[1]]
  
  for(j in 1:dim(pts)[3]){
    temp = "# Markups fiducial file version = 4.13\n# CoordinateSystem = LPS\n# columns = id,x,y,z,ow,ox,oy,oz,vis,sel,lock,label,desc,associatedNodeID\n"
    
    for(i in 1:dim(pts)[1]){
      temp = paste0(temp, '\n',
                    paste(paste0("vtkMRMLMarkupsFiducialNode_", i-1), 
                          pts[i,1,j],
                          pts[i,2,j], 
                          pts[i,3,j],
                          paste(rep(0,3), collapse=','), 
                          paste(rep(1,4), collapse=','), 
                          paste(labels[i]),
                          paste(rep("",2), collapse=','), 
                          sep=','))
      
      cat (temp, file = paste0(prefix, names[j], suffix, ".fcsv"))
    }
  }
}

