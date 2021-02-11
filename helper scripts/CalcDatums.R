calcDatums <- function(WLdf){
  #WLdf <- findTides(WLdf, w = 11, boundary = "discard")
  require(reshape2)
  d <- data.frame(datum = c("MHHW", "MHW", "MSL", "MLW", "MLLW"), "WL" = NA)
  d <- dcast(d, . ~ datum, value.var = "WL", )[,-1]
  d <- select(d, c("MHHW", "MHW", "MSL","MLW", "MLLW"))
  
  d$MLW <- mean(WLdf$WL[grep(x = WLdf$tide, pattern = "L")], na.rm = T)
  d$MLLW <- mean(WLdf$WL[grep(x = WLdf$tide, pattern = "LL")], na.rm = T)
  d$MSL <- mean(WLdf$WL, na.rm = T)
  d$MHW <- mean(WLdf$WL[grep(x = WLdf$tide, pattern = "H")], na.rm = T)
  d$MHHW <- mean(WLdf$WL[grep(x = WLdf$tide, pattern = "HH")], na.rm = T)
  
  return(d)
}