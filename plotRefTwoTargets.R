# plotRefTwoTargets()
# Plot three dimensional shape differences between a reference and target 
# specimen(s).

# 04/27/2023
# M.E.F. 
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# Given one reference and one to ten target landmark sets, generate a 3D plot of 
# shape differences of the target specimen(s) relative to the reference 
# specimen.

# SOURCE:               geomorph's plotRefToTarget()
#                       https://rdrr.io/cran/geomorph/man/plotRefToTarget.html

# @param ref            Matrix of landmark coordinates for the reference 
#                       landmarks.
# @param tar            Matrix/array of landmark coordinates for the target
#                       landmarks.
# @param mag            Numeric, desired magnification to be used when 
#                       visualizing shape differences. 
#                       Default: 1        
# @param gridPars       An optional object made by geomorph's gridPar()
# @param prog           Logical, whether or not to plot the landmark targets as
#                       a progression.
# @param vectcols       Character vector of colors for target landmark vectors. 
# @param label          Logical, whether or not to plot landmark labels. If TRUE
#                       and the reference matrix have defined landmark names in 
#                       dimnames(ref)[[1]], then those will be used. If not, 
#                       landmarks will be numbered in order of appearance in 
#                       the reference matrix.
#                       If a vector of the same length as the first dimension
#                       of the array is provided, then those labels will be used.
# @param links          An optional matrix defining links between landmarks. 

# @return               A 3D, interactive scatter plot.

# @examples
# library(geomorph)
# data(scallops)
# scalgpa <- gpagen(scallops$coorddata)

# Plot only reference matrix.
# plotRefTwoTargets(scalgpa$consensus)

# Plot reference matrix with labels and links. 
# scallinks <- matrix(c(1,rep(2:16, each = 2), 1), nrow = 16, byrow = TRUE)
# plotRefTwoTargets(scalgpa$consensus, label = TRUE, links = scallinks)

# Plot reference and target vectors with 2x mag.
# plotRefTwoTargets(scalgpa$consensus, scalgpa$coords[,,1], mag = 2)

# Plot reference and target vectors with defined colors and smaller points.
# plotRefTwoTargets(scalgpa$consensus,
#                   scalgpa$coords[,,1:3],
#                   vectcols = c("black", "red", "blue"),
#                   radius = 0.0025
#                   )

plotRefTwoTargets<-function(ref, tar = NULL, mag = 1, gridPars = NULL, prog = FALSE,
                            vectcols = NULL, label = FALSE, links = NULL, ...){
    # Require libraries    
        require(geomorph)
    
    # DEFINE PLOT PARAMETERS (param: gridPars)
        # When null, defined by the default created by geomorph's gridPar()
        # https://rdrr.io/cran/geomorph/man/gridPar.html
        if(is.null(gridPars)){gP <- gridPar()} else{gP <- gridPars}
        
    # PLOT REFERENCE LANDMARKS (params: ref, label, links)  
        plot3d(ref,
               type = "s",
               col = gP$pt.bg,
               size = gP$pt.size,
               aspect = FALSE,
               xlab = "",
               ylab = "",
               zlab = "",
               axes = F,
               ...)
        
        # Add labels if provided in deference specimen.
        if(label == TRUE){
                # If the dimnames of the matrix are empty...
                if(is.null(dimnames(ref)[[1]])){
                        text3d(ref, 
                               texts = paste(1:dim(ref)[1]), 
                               adj = (gP$txt.adj+gP$pt.size),
                               pos = gP$txt.pos, 
                               cex = gP$txt.cex,
                               col = gP$txt.col)
                }
                
                # ... or if they define the landmark names. 
                else{
                text3d(ref, 
                       texts = dimnames(ref)[[1]], 
                       adj = (gP$txt.adj+gP$pt.size),
                       pos = gP$txt.pos, 
                       cex = gP$txt.cex,
                       col = gP$txt.col)
                }
        }
        # Add labels if provided as a vector (only works if same length, otherwise defaults)
        if(length(label) == dim(ref)[1]{
                text3d(ref,
                       texts = label,
                       adj = (gP$txt.adj+gP$pt.size),
                       pos = gP$txt.pos, 
                       cex = gP$txt.cex,
                       col = gP$txt.col)
        }
        
        # Add landmark links if defined.
        if(!is.null(links)){
                # Define parameters
                linkcol <- rep(gP$tar.link.col, nrow(links))
                linklwd <- rep(gP$tar.link.lwd, nrow(links))
                linklty <- rep(gP$tar.link.lty, nrow(links))
                
                # Plot links
                for (i in 1:nrow(links)){
                        segments3d(rbind(ref[links[i,1],],
                                         ref[links[i,2],]),
                                   col = linkcol[i],
                                   lty = linklty[i],
                                   lwd = linklwd[i]
                                   )
                }
        }
        
    # PLOT TARGET LANDMARKS (T1-10, vectcols, mag, gridPars)  
        dims <- dim(tar)
        

        # If only reference landmarks provided...
        if(is.null(dims)){
                message("No target landmarks provided, plotted reference landmarks only.")
                on.exit(options(options(show.error.messages = FALSE)))
                stop()
        }
        
        # Define vector colors
        # If vectcols == NULL, "Paired" from RColorBrewer is used.
        if(is.null(vectcols)){
                require(RColorBrewer)
                vectcols <- brewer.pal(10, "Paired")
        }
        
        # If one target is provided...
        if(length(dims) == 2){
                for(j in 1:dims[1]){
                        temp <- tar + (tar - ref)*(mag - 1)
                        segments3d(rbind(ref[j,], temp[j,]),
                                   lwd = 2,
                                   col = vectcols[1])
                }
        }
                
        # If more than one target is provided.        
        if(length(dims) > 2){
                # Confirm there are enough colors.
                if(length(vectcols) < dims[3]){
                        stop(paste0("Not enough colors provided in vectcols. There are ",
                                    dims[3], " target landmark set(s), but only ",
                                    length(vectcols), " color(s) were provided."))
                }
                
                # Calculate and plot differences between target and reference
                # landmarks.
                # If not sequential...
                if(prog == FALSE){
                  # Calculate and plot differences between target and reference
                  # landmarks.
                  for(i in 1:dims[3]){
                    for(j in 1:dims[1]){
                      temp <- tar[,,i] + (tar[,,i] - ref)*(mag - 1)
                      segments3d(rbind(ref[j,], temp[j,]),
                                 lwd = 2,
                                 col = vectcols[i])
                    }
                  }
                }
               # If sequential...
                  if(prog == TRUE){
                    for(j in 1:dims[1]){
                      temp <- tar[,,1] + (tar[,,1] - ref)*(mag - 1)
                      segments3d(rbind(ref[j,], temp[j,]),
                                 lwd = 2,
                                 col = vectcols[1])
                    } 

                    for(i in 2:dims[3]){
                      for(j in 1:dims[1]){
                        temp <- tar[,,i] + (tar[,,i] - tar[,,i-1])*(mag - 1)

                        segments3d(rbind(tar[j,,i-1], temp[j,]),
                                   lwd = 2,
                                   col = vectcols[i])


                      }
                    }
                  }
  }
}
