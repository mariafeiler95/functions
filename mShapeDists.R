# mShapeDists()
# Calculate the distance of specimens from the mean shape of the array

# Given an array of coordinate data and a number of desired specimens, find the 
# desired number of specimens that are closest to the mean shape. 

# 03/03/3023
# M.E.F.
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# SOURCE:   Based on geomorph's findMeanSpec()

# @param A  Array of coordinate data, either 2D or 3D
# @param n  Number of desired specimens

# @return   A vector of Procrustes distances from the calculated mean shape,
#           where names are the names of the specimens.

# @examples 
# library(geomorph)
# data(scallops)
# scalgpa <- gpagen(scallops$coorddata)

# Find the two specimens closest to the mean shape.
# mShapeDists(A = scalgpa$coords, n = 2)

mShapeDists <- function(A, n){
  require(geomorph)
  if(!is.array(A)) {
    stop("Data matrix not a 3D array (see 'arrayspecs').") }
  ref <- mshape(A)
  x <- two.d.array(A)
  x <- rbind(x, as.vector(t(ref)))
  dists <- as.matrix(dist(x))[,(nrow(x))]
  dists <- sort(dists[-which(dists == 0)]) #Remove 0 and sort
  return(dists[1:n])
}
