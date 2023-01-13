# procDistCIs()
# Calculate Procrustes distance between two arrays of landmark data

# Given two arrays of landmark data where the dimensions are individual 
# landmarks, X,Y,Z coordinates, and specimens for p, k, and n, respectively,
# runs a bootstrap to get the Procrustes distance between the landmark sets and
# a confidence interval.

# 01/12/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# SOURCES: Original code written by Christopher Percival.

# @param LMs1           Matrix or array of control specimen(s)
# @param LMs2           Matrix or array of test specimen(s)
# @param nit            Numeric denoting number of iterations for the bootstrap
#                       Default: 1000
# @param alpha          Numeric denoting desired confidence 
#                       (for 95%, alpha = 0.05)
#                       Default: 0.05
# @param verbose        Logical, whether to return the distances of individual 
#                       landmarks as a proxy for landmark effect
#                       Default: TRUE

# @return               A list containing:
#                       $ProcDist               Procrustes distance
#                       $CILower                CI lower bound
#                       $CIUpper                CI upper bound
#                       $IndEucDists            Euclidean distances between
#                                               individuals
#                       $BootstrapProcDists     Bootstrapped Procrustes 
#                                               distances

# @examples
# library(geomorph)
# data(scallops)
# scalgpa <- gpagen(scallops$coorddata)

# Compare specimens 1 and 2 to 3 through 5, non-verbose option, 100 iterations, 
# 80% confidence
# procDistCIs(LMs1 = scalgpa$coords[,,1:2],
#             LMs2 = scalgpa$coords[,,3:5],
#             nit = 100,
#             alpha = 0.2,
#             verbose = FALSE)
# )

procDistCIs <-  function(LMs1, LMs2, nit = 1000, alpha = 0.05, verbose = TRUE){
        # SETUP
        # Ensure geomorph is loaded
        require(geomorph)
        
        # PROCRUSTES DISTANCES
        # Landmark distances between samples
        dists <- sqrt(rowSums((mshape(LMs1) - mshape(LMs2))^2))
        
        # Calculate Procrustes Distance between specimens
        PD <- sum(dists)
        
        # BOOTSTRAP
        # Create object to catch bootstrapped Procrustes Distance values
        boot <- c()
        
        for (i in 1:1000) {
                # Collect samples of specimens with replacement from LMs1 matrix
                samp1 <- sample(1:dim(LMs1)[[3]], 
                                dim(LMs1)[[3]], 
                                replace = TRUE
                                )
                samp2 <- sample(1:dim(LMs2)[[3]], 
                                dim(LMs2)[[3]], 
                                replace = TRUE
                                )
                
                # Collect landmark data of said samples
                samp1 <- LMs1[,,samp1]
                samp2 <- LMs2[,,samp2]
                
                # Calculate Procrustes distance between bootstrapped LM samples and save  
                # to vector
                boot <- c(boot, sum(sqrt(rowSums((mshape(samp1)-mshape(samp2))^2))))
        }
        
        # Calculate confidence interval
        lower <- unname(quantile(boot, probs = alpha/2))
        upper <- unname(quantile(boot, probs = 1 - alpha/2))
        
        # OUTPUT 
        if(verbose == TRUE){
                result <- list("ProcDist" = PD,
                               "CILower" = lower,
                               "CIUpper" = upper,
                               "alpha" = alpha,
                               "IndEucDists" = dists,
                               "BootstrapProcDists" = boot
                               )
        }
        else{
                result <- list("ProcDist" = PD, 
                                "CILower" = lower,
                                "CIUpper" = upper,
                                "alpha" = alpha
                               )
        }
        
        return(result)
}
