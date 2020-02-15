
plotCumCases <- function(aggr, plot.conf=TRUE, plot.recov=TRUE, plot.dead=TRUE, thk=1) {
  curves <- c(plot.conf, plot.recov, plot.dead)
  names(curves) <- c("conf", "recov", "dead")
  curves <- which(curves)
  colpal <- c("red3", "skyblue3", "purple3")
  legLab <- c("Confirmed", "Recovered", "Deaths")
  if (length(curves)==0 || nrow(aggr)==0) return()

  plot(aggr$date, aggr[[names(curves)[1]]], 
    type="l", bty="l", lwd=2*thk, col=colpal[curves[1]],
    xlab="Date", ylab="Cumulative number of cases")
  if (length(curves)>1) {
    for (i in 2:length(curves)) {
      points(aggr$date, aggr[[names(curves)[i]]],
        type="l", lwd=2*thk, col=colpal[curves[i]])
    }
  }
  legend("topleft", legLab[curves], lwd=2*thk, col=colpal[curves], 
    bty="n", seg.len=5, inset=c(0.05,0.05))
}



plotIncCases <- function(aggr, plot.conf=TRUE, plot.recov=TRUE, plot.dead=TRUE, thk=1, line=FALSE) {
  curves <- c(plot.conf, plot.recov, plot.dead)
  names(curves) <- c("conf", "recov", "dead")
  curves <- which(curves)
  colpal <- c("red3", "skyblue3", "purple3")
  legLab <- c("Confirmed", "Recovered", "Deaths")
  if (length(curves)==0 || nrow(aggr)==0) return()
  
  aggr <- aggr[c(1,1:nrow(aggr)),]
  aggr[1,c("conf","recov","dead")] <- 0
  aggr[1,"date"] <- aggr[1,"date"]-1

  lwdVal <- as.integer(diff(range(aggr$date[-1]))) * 1.04 / length(curves) * 1.1
  plot(aggr$date[-1], diff(aggr[[names(curves)[1]]]), 
    type=ifelse(line,"l","h"), bty="l", lwd=ifelse(line, 2, lwdVal)*thk, 
    lend=1, col=colpal[curves[1]],
    xlab="Date", ylab="Number of new cases reported")
  if (length(curves)>1) {
    for (i in 2:length(curves)) {
      points(aggr$date[-1]+0.2*(i-1)*as.integer(!line), diff(aggr[[names(curves)[i]]]),
        type=ifelse(line,"l","h"), lwd=ifelse(line, 2, lwdVal)*thk, 
        lend=1, col=colpal[curves[i]])
    }
  }
  if (line) {
    legend("topleft", legLab[curves], lwd=2*thk, col=colpal[curves], bty="n", seg.len=5, inset=c(0.05,0.05))
  } else {
    legend("topleft", legLab[curves], pch=15, col=colpal[curves], bty="n", pt.cex=1.8, inset=c(0.05,0.05))
  }
}



plotSecDer <- function(aggr, plot.conf=TRUE, plot.recov=TRUE, plot.dead=TRUE, thk=1) {
  curves <- c(plot.conf, plot.recov, plot.dead)
  names(curves) <- c("conf", "recov", "dead")
  curves <- which(curves)
  colpal <- c(conf="red3", recov="skyblue3", dead="purple3")
  legLab <- c("Confirmed", "Recovered", "Deaths")
  if (length(curves)==0 || nrow(aggr)==0) return()
  
  aggr <- aggr[c(1,1,1:nrow(aggr)),]
  aggr[1:2,c("conf","recov","dead")] <- 0
  aggr[1:2,"date"] <- aggr[1:2,"date"]-1
  
  L <- list(
    conf = try(loess.smooth(as.integer(aggr$date[-(1:2)]), diff(diff(aggr$conf))), silent=TRUE),
    recov = try(loess.smooth(as.integer(aggr$date[-(1:2)]), diff(diff(aggr$recov))), silent=TRUE),
    dead = try(loess.smooth(as.integer(aggr$date[-(1:2)]), diff(diff(aggr$dead))), silent=TRUE)
  )
  plot(aggr$date[-(1:2)], diff(diff(aggr[[names(curves)[1]]])), 
    type="n", bty="l", xlab="Date", 
    ylab="Change in number of new cases reported since previous report")
  abline(h=0, col="lightgrey", lwd=2)
  for (i in names(curves)) {
    points(aggr$date[-(1:2)], diff(diff(aggr[[i]])), type="l", lwd=2*thk, col=colpal[i])
  }
  for (i in names(curves)) {
    if (!inherits(L[[i]], "try-error"))
      points(L[[i]]$x, L[[i]]$y, col=colpal[i], lwd=2*thk, lty="dashed", type="l")
  }
  legend("topleft", c(legLab[curves], "LOESS fit"), lwd=2*thk, bty="n", seg.len=5, 
    col=c(colpal[curves], "darkgrey"), lty=c(rep("solid",length(curves)), "dashed"), inset=c(0.05,0.05))
}

