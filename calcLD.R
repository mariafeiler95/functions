# calcLD()
# Calculate linear distance between two 3D landmarks across an array

# Given a matrix of landmark pairs and an array organized p x k x n (landmarks, 
# dimensions, specimens), calculates the linear distance between those landmarks 
# across all specimens.

# 08/25/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.3.1, RStudio 2023.6.0.421
# Windows 10 x64

# SOURCES: Original code written by Christopher Percival.

# @param data           Array of landmark data
# @param pairs          Matrix of landmark pairs, such that each row represents
#                       two landmarks you wish to calculate linear distances
#                       between.
#                       Default: first and second landmarks
# @param sum            Logical, whether to take a sum of all distances 
#                       calculated.
#                       Default: FALSE
# @param rel            Logical, whether to calculate the relative contributions
#                       of each distance to the total length.
#                       Default: FALSE
# @param nam            A character vector of names to be used for the linear 
#                       distances calculated. Needs to be the same length as the 
#                       number of rows of the pairs matrix.
#                       Default: numbered 

# @return               A named vector of distances or a matrix containing 
#                       linear distances between the landmarks as columns and 
#                       specimens as rows.

# @examples
# library(geomorph)
# data(scallops)
# scalLMs <- scallops$coorddata
# scalPairs <- scallops$land.pairs

# calcLD(scalLMs, pairs = scalPairs, sum = TRUE, rel = TRUE)

calcLD <- function(data, pairs = c(1,2), sum = FALSE, rel = FALSE, nam = 1:nrow(pairs)){
    if(is.null(nrow(pairs))){
        # Pull two landmarks
        LMs1 = data[pairs[1],,]
        LMs2 = data[pairs[2],,]
        
        # Calculate distances
        dist = sqrt(colSums((LMs1-LMs2)^2))
        
        # Add specimen names and return.
        names(dist) = dimnames(data)[[3]]
        return(dist)
    }
    
    else{
        dist = matrix(data = NA, nrow = dim(data)[3], ncol = nrow(pairs),
                      dimnames = list(dimnames(data)[[3]], nam)
                      )
        
        for(i in 1:nrow(pairs)){
            LMs1 = data[pairs[i,1],,]
            LMs2 = data[pairs[i,2],,]
            
            dist[,i] = sqrt(colSums((LMs1-LMs2)^2))
        }
        
        if(sum == TRUE){dist = cbind(dist, "Total" = rowSums(dist))}
        
        if(rel == TRUE){
            rels = matrix(data = NA, nrow = dim(data)[3], ncol = nrow(pairs),
                          dimnames = list(dimnames(data)[[3]], paste(nam, "Rel", sep = ".")))
            
            for(i in 1:nrow(pairs)){
                rels[,i] = dist[,i]/dist[,"Total"]
            }
            
            dist = cbind(dist, rels)
        }
        
        return(dist) 
    }
}
