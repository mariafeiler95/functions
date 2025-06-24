# mShapeDists()
# Calculate the distance of specimens from the mean shape of the array

# Given an array of coordinate data and a number of desired specimens, find the 
# desired number of specimens that are closest to the mean shape. 

# 06/24/2025
# M.E.F.
# mariaefeiler@gmail.com

# R4.4.0, RStudio 2024.04.1+748
# Windows 10 x64

# SOURCE: Based on geomorph's findMeanSpec()

# @param A             Array of coordinate data, either 2D or 3D
# @param n             Number of desired specimens
#                      If "all", returns all specimens in order.
#                      If a negative integer, gives the specimens furthest from 
#                      the mean shape.
#                      Default value = 3
# @param nams.only     If TRUE, only returns specimen names. If FALSE, then 
#                      distances are also reported.

# @return   A vector of Procrustes distances from the calculated mean shape,
#           where names are the names of the specimens.

# @examples 
# library(geomorph)
# data(scallops)
# scalgpa <- gpagen(scallops$coorddata)

# Find the two specimens closest to the mean shape.
# mShapeDists(A = scalgpa$coords, n = 2)

# Find the two specimens farthest from the mean shape.
# mShapeDists(A = scalgpa$coords, n = -2)

# Sort the specimens in order from closest to farthest from mean shape, retaining only names.
# mShapeDists(A = scalgpa$coords, n = "all", nams.only = TRUE)

mShapeDists <- function(A, n = 3, nams.only = FALSE){
  require(geomorph)
  if(!is.array(A)) {
    stop("Data matrix not a 3D array (see 'arrayspecs').") }
  ref <- mshape(A)
  x <- two.d.array(A)
  x <- rbind(x, as.vector(t(ref)))
  dists <- as.matrix(dist(x))[,(nrow(x))]
  dists <- sort(dists[-which(dists == 0)]) #Remove 0 and sort

  if(nams.only == TRUE){
        if(is.character(n)){return(names(dists))}
        if(n < 0){return(tail(names(dists), abs(n)))}
        if(n > 0){return(names(dists)[1:n])}
    }
    
    if(nams.only == FALSE){
        if(is.character(n)){return(dists)}
        if(n < 0){return(tail(dists, abs(n)))}
        if(n > 0){return(dists[1:n])}
    }
}
