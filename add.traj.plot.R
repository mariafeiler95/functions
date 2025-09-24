add.traj.plot <- function(TP, 
                             traj.pch = 21,
                             traj.col = 1,
                             traj.lty = 1,
                             traj.lwd = 1,
                             traj.cex = 1.5,
                             traj.bg = 1,
                             start.bg = 3,
                             end.bg = 2) {
  
  traj <- TP$trajectories
  nt <- length(traj)
  np <- NROW(traj[[1]])
  
  if(length(traj.pch) != 1 && length(traj.pch) != nt)
    stop("For add.trajectories, traj.pch must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(traj.pch) == 1) 
           traj.pch <- rep(traj.pch, nt)
  
  if(length(traj.col) != 1 && length(traj.col) != nt)
    stop("For add.trajectories, traj.col must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(traj.col) == 1) 
           traj.col <- rep(traj.col, nt)
  
  if(length(traj.lty) != 1 && length(traj.lty) != nt)
    stop("For add.trajectories, traj.lty must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(traj.lty) == 1) 
           traj.lty <- rep(traj.lty, nt)
  
  if(length(traj.lwd) != 1 && length(traj.lwd) != nt)
    stop("For add.trajectories, traj.lwd must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(traj.lwd) == 1) 
           traj.lwd <- rep(traj.lwd, nt)
  
  if(length(traj.cex) != 1 && length(traj.cex) != nt)
    stop("For add.trajectories, traj.cex must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(traj.cex) == 1) 
           traj.cex <- rep(traj.cex, nt)
  
  if(length(traj.bg) != 1 && length(traj.bg) != nt)
    stop("For add.trajectories, traj.bg must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(traj.bg) == 1) 
           traj.bg <- rep(traj.bg, nt)
  
  if(length(start.bg) != 1 && length(start.bg) != nt)
    stop("For add.trajectories, start.bg must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(start.bg) == 1) 
           start.bg<- rep(start.bg, nt)
  
  if(length(end.bg) != 1 && length(end.bg) != nt)
    stop("For add.trajectories, end.bg must be equal in length 
         to the number of trajectories or just one value\n",
         call. = FALSE) else if(length(end.bg) == 1) 
           end.bg<- rep(end.bg, nt)     
  
  for(i in 1:nt){
    x <- traj[[i]][,TP$pc.axes[1]]
    y <- traj[[i]][,TP$pc.axes[2]]
    lines(x, y, col = traj.col[i], lwd = traj.lwd[i], lty = traj.lty[i])
    points(x, y, col = 1, pch = traj.pch[i], 
           lwd = traj.lwd[i], cex = traj.cex[i], bg = traj.bg[i])
    points(x[1], y[1], col = 1, pch = traj.pch[i], 
           lwd = traj.lwd[i], cex = traj.cex[i], bg = start.bg[i])
    points(x[np], y[np], col = 1, pch = traj.pch[i], 
           lwd = traj.lwd[i], cex = traj.cex[i], bg = end.bg[i])
  }
  
}
