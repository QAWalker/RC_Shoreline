findTides <- function(WLdf, w = 11, boundary = "discard"){
  
  timecol <- which(sapply(WLdf, is.POSIXt))[1]
  lcol <- which(!sapply(WLdf, is.POSIXt))[1]
  WLdf$tide <- NULL
  
  WLdf <- WLdf[!is.na(WLdf[,lcol]),]
  
  require(scorepeak)
  
  WLdf[,"isHighTide"] <- isHighTide <- detect_localmaxima(data = WLdf[,lcol], w = w, boundary = boundary)
  WLdf[,"isLowTide"] <- isHighTide <- detect_localmaxima(data = WLdf[,lcol]*-1, w = w, boundary = boundary)
  
  for (i in which(WLdf$isHighTide)) {
    d <- date(WLdf[i, timecol])
    WLdf[i, "tide"] <- ifelse(WLdf[i,lcol]>=max(WLdf[WLdf[,timecol] >= d & WLdf[,timecol] <= d + days(1),]$WL), "HH", "H")
  }
  
  for (i in which(WLdf$isLowTide)) {
    d <- date(WLdf[i, timecol])
    WLdf[i, "tide"] <- ifelse(WLdf[i,lcol]<=min(WLdf[WLdf[,timecol] >= d & WLdf[,timecol] <= d + days(1),]$WL), "LL", "L")
  }
  
  return(select(WLdf, -c("isHighTide", "isLowTide")))
}