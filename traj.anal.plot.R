traj.anal.plot <- function(x, pc.axes = c(1,2), ...) {
  require(RRPP)

  if(!is.null(x$pca)) {
    pca <- x$pca
    rot <- pca$rotation
    Y <- x$fit$LM$Y
    props <- pca$sdev^2 / sum(pca$sdev^2)
    pc.points <- center(Y) %*% rot
    trajectories <- x$trajectories[[1]]
  }
  
  if(is.null(x$pca) && x$type == "factorial") {
    f <- if(x$fit$LM$gls) x$fit$LM$gls.fitted else x$fit$LM$fitted
    pca <- prcomp(f)
    rot <- pca$rotation
    Y <- x$fit$LM$Y
    Y.cent <- colMeans(Y)
    props <- pca$sdev^2 / sum(pca$sdev^2)
    pc.points <- center(Y) %*% rot
    trajectories <- x$trajectories[[1]]
    if(is.matrix(trajectories)) trajectories <- list(trajectories)
    traj.c <- matrix(Y.cent, NROW(trajectories[[1]]), 
                     NCOL(trajectories[[1]]), byrow = TRUE)
    trajectories <- lapply(trajectories, function(x) (x - traj.c) %*% rot)
  }
  
  if(x$type == "single.factor") {
    f <- if(x$fit$LM$gls) x$fit$LM$gls.fitted else x$fit$LM$fitted
    tp <- x$n.points
    p <- NCOL(f)/tp
    n <- NROW(f)
    ft <- array(f, c(n, p, tp))
    ft2 <- ft[,,1]
    for(i in 2:tp) ft2 <- rbind(ft2, ft[,,i])
    pca <- prcomp(ft2)
    rot <- pca$rotation
    Y <- x$fit$LM$Y
    Y2 <- array(Y, c(n, p, tp))
    Y <- Y2[,,1]
    for(i in 2:tp) Y <- rbind(Y, Y2[,,i])
    Y.cent <- colMeans(Y)
    props <- pca$sdev^2 / sum(pca$sdev^2)
    pc.points <- center(Y) %*% rot
    trajectories <- x$trajectories[[1]]
    if(is.matrix(trajectories)) trajectories <- list(trajectories)
    traj.c <- matrix(Y.cent, NROW(trajectories[[1]]), 
                     NCOL(trajectories[[1]]), byrow = TRUE)
    trajectories <- lapply(trajectories, function(x) (x - traj.c) %*% rot)
  }
  
  
  dots <- list(...)
  if(is.null(dots$xlab))
    xlabel <- paste0("PC ", pc.axes[1], " for fitted values: ", 
                    round(props[pc.axes[1]] *100, 2), "%")
  if(is.null(dots$ylab))
    ylabel <- paste0("PC ", pc.axes[2], " for fitted values: ", 
                    round(props[pc.axes[2]] *100, 2), "%")
  
  if(!is.null(dots$xlab) && !is.null(dots$ylab)) 
    plot(pc.points[,pc.axes[1]], pc.points[,pc.axes[2]], asp = 1, ...)
  if(!is.null(dots$xlab) && is.null(dots$ylab)) 
    plot(pc.points[,pc.axes[1]], pc.points[,pc.axes[2]], asp = 1, ylab = ylabel, ...)
  if(is.null(dots$xlab) && !is.null(dots$ylab)) 
    plot(pc.points[,pc.axes[1]], pc.points[,pc.axes[2]], asp = 1, xlab = xlabel, ...)
  if(is.null(dots$xlab) && is.null(dots$ylab))
    plot(pc.points[,pc.axes[1]], pc.points[,pc.axes[2]], asp = 1, xlab = xlabel, ylab = ylabel, ...)
  out <- list(pca = pca, pc.points = pc.points, 
              trajectory.analysis = x, trajectories = trajectories)
  invisible(out)
}
