# shapeHullsPCA()
# Creates convex hulls for ordination plots.

# Given PC coordinates, optional groups, and graphing parameters, creates a plot
# with PC coordinates and convex hulls.

# 06/24/2025
# M.E.F.
# mariaefeiler@gmail.com

# R4.4.0, RStudio 2024.04.1+748
# Windows 10 x64

# SOURCE: Based on geomorph's shapeHulls()

# @param x              Data to produce hulls. Can be a matrix of PC scores, an 
#                       array of raw data (will be prompted asked if Procrustes
#                       alignment is desired), or a plot.gm.prcomp/gm.prcomp object.
# @param axis1          PC to be plotted on the x-axis.
# @param axis2          PC to be plotted on the y-axis.
# @param groups         Optional vector of groups for the data. Can be a character 
#                       vector or a factor.
# @param group.cols     Colors for hulls/points
# @param group.lwd      Line weight for hulls. Default is no line (see group.lty)
# @param group.lty      Line type for hulls. Default is no line (0).
# @param group.fill     Alpha value of your hulls. Default is 0/no fill.
# @param hulls.only     Logical, whether to include points on the plot.
# @param legend.pos     If legend is desired, indicate the position to be fed to
#                       the legend() function.
# @param print.pc.var   If a gm.prcomp/ordinate object is provided or produced,
#                       prints the summary table of eigenvalues/PC variances.
# @param ...            ... and any parameters to be passed to the plot function.

# @return               A plot of PC scores.

shapeHullsPCA <- function(x, axis1 = 1, axis2 = 2, groups = NULL, 
                          group.cols = NULL, group.lwd = NULL, group.lty = 0,
                          group.fill = 0, hulls.only = FALSE, legend.pos = NULL,
                          print.pc.var = FALSE, ...){
    require(geomorph)

    # Get data into workable condition
    if(sum(match(class(x), "plot.gm.prcomp"), na.rm = TRUE) > 0){
        # If plot.gm.prcomp provided, pull PC scores from object...
        y <- as.matrix(x$PC.points)
        if(ncol(y) < 2) {
            stop("Cannot create hulls in fewer than 2 dimensions.")
        }
    } else if(is.matrix(x)){
        # If PC scores are provided in a matrix...
        y <- x
    } else if(sum(match(class(x), "ordinate"), na.rm = TRUE) > 0){
        # If gm.prcomp or ordinate object is provided...
        y <- x$x
        # Prints variance of PCs if desired...
        if(print.pc.var == TRUE){summary(x)}
    } else if(is.array(x)){
        # If raw shape data is provided, will ask if they should be Procrustes aligned
        # then will run ordination using geomorph::gpagen() and geomorph::gm.prcomp()...
        answer1 <- readline(paste0("Data presented as array of dimensions ", 
                                   dim(x)[1], "x", dim(x)[2], "x", dim(x)[3], ". ", 
                                   "Would you like to run a Procrustes alignment? (y=yes | enter=no)"))
        if(answer1 == "y") {
            cat("...Running Procrustes alignment...\n")
            gpa <- gpagen(A = x, print.progress = FALSE)
            
            cat("...Running ordination...\n")
            y <- gm.prcomp(gpa$coords)
            # Prints variance of PCs if desired...
            if(print.pc.var == TRUE){summary(y)}
            y <- y$x
        } else {
            cat("...Running ordination...\n")
            y <- gm.prcomp(x)
            
            # Prints variance of PCs if desired...
            if(print.pc.var == TRUE){summary(y)}
            y <- y$x
        }
    } 
    
    # Get plot information
    n <- nrow(y)
    y <- y[,c(axis1, axis2)]
    
    # Stop if groups vector/factor is not correct length...
    if(!is.null(groups) && length(groups) != n) {
        stop("Different number of observations between groups factor and PC plot.\n",
             call. = FALSE)
    }
    
    # If no groups are provided, make dummy groups variable.
    if(is.null(groups)) {groups <- rep(1, n)}
    
    # Parses groups parameter, can receive factor or plain vector.
    if(is.factor(groups)) {ug <- levels(groups)}
    
    if(!is.factor(groups)) {
        groups <- factor(groups, levels = unique(groups))
        ug <- levels(groups)
        warning("Groups not presented as factor, will factorize with the following levels:",
                call. = FALSE, immediate. = TRUE)
        cat(ug)
        
        answer2 <- readline("Save factor for future use? (y=yes | enter=no)")
        
        if(answer2 == "y") {
            .GlobalEnv$PCAfactor <- groups
            cat("Factor saved as 'PCAfactor' to global environment.")
        } else (cat("Factor not saved to global environment."))
        
    }
    
    # Get number of levels in groups factor.
    g <- nlevels(groups)
    
    # Parses graphing parameters for hulls...
    if(length(group.cols) == 0) {
        group.cols <- 1:g
    } else if(length(group.cols) == 1) {
        group.cols <- rep(group.cols, g)
    } else if(length(group.cols) != g) {
        stop(paste0("Number of requested group colors does not match the number of groups.\n
                    Please provide either a single value or a vector of length ", g, "."))
    }

    if(length(group.lwd) == 0) {
        group.lwd <- rep(1,g)
    } else if(!is.null(group.lwd) && length(group.lwd) == 1) {
        group.lwd <- rep(group.lwd, g)
    } else if(length(group.lwd) != g) {
        stop(paste0("Number of requested group widths does not match the number of groups.\n
                    Please provide either a single value or a vector of length ", g, "."))
    }
    
    if(length(group.lty) == 0) {
        group.lty <- rep(1,g)
    } else if(!is.null(group.lty) && length(group.lty) == 1) {
        group.lty <- rep(group.lty, g)
    } else if(length(group.lty) != g) {
        stop(paste0("Number of requested group line types does not match the number of groups.\n
                    Please provide either a single value or a vector of length ", g, "."))
    }
    
    # Gets hull dimensions
    chp <- list()
    for(i in 1:g){
        yy <- y[groups == ug[i],]
        chp[[i]] <- chull(yy)
        chp[[i]] <- c(chp[[i]], chp[[i]][1])
    }
    
    plot(y, pch = 19, col = par()$bg, ...)
    
    # Plots hulls
    for(i in 1:g){
        yy <- y[groups == ug[i],]
        polygon(yy[chp[[i]],1], yy[chp[[i]],2], border = group.cols[i], 
                lwd = group.lwd[i], lty = group.lty[i], 
                col = adjustcolor(group.cols[i], alpha.f = group.fill),
                ...)
    }
    
    # Plots points
    if(hulls.only == FALSE){
        points(x = y[,1], y = y[,2], pch = 21, col = "black", 
               bg = group.cols[groups], ...)
    }
    
    # Plots legend
    if(!is.null(legend.pos)){
        legend(legend.pos, legend = ug, cex = .75, pt.cex = 1.5, pch = 21, 
               col = "black", pt.bg = group.cols)
    }

}
