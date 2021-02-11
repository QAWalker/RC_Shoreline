inundationTime <- function (WLdf, in.elev, unit = "mins", approx.method = "linear", maxGap = minutes(6)){
  require(lubridate)
  require(rootSolve)
  
  timecol <- which(sapply(WLdf, is.POSIXt))[1]
  lcol <- which(!sapply(WLdf, is.POSIXt))[1]
  
  df4nacheck <- data.frame("t"= WLdf[,timecol], "wl" = WLdf[,lcol])
  
  gaps <- which(difftime(WLdf[,timecol], lag(WLdf[,timecol]))>maxGap)
  #WLdf[(gaps[1]-2):(gaps[1]+2),]
  if(length(gaps)>0){
    warning(
      c(
        paste(
          "there are",
          length(gaps),
          "gaps in time greater than",
          maxGap,
          "with a total missing time of",
          sum(difftime(WLdf[, timecol], lag(WLdf[, timecol]), units = "mins")[gaps], na.rm = T),
          "minutes   \n"), 
        paste(sapply(gaps, 
                     function(g){
                       paste(
                         as.numeric(diff(WLdf$DateTime)[g-1]),
                         units.difftime(diff(WLdf$DateTime)[g]),
                         "@",
                         WLdf[g-1, timecol], 
                         "through", 
                         ifelse(as.Date(WLdf[g-1, timecol]) == as.Date(WLdf[g, timecol]), 
                                substr(WLdf[g, timecol], 12, 19), 
                                as.character(WLdf[g, timecol])))
                     } 
        ), "", sep = "\n")
      )
    )
  }
  g = gaps[1]
  sapply(gaps, 
         function(g){
           paste(WLdf$DateTime[g-1], 
                 "through", 
                 ifelse(as.Date(WLdf$DateTime[g-1]) == as.Date(WLdf$DateTime[g]), 
                        substr(WLdf$DateTime[g], 12, 19), 
                        WLdf[g, timecol]))
         } 
         
  )
  
  #df4nacheck <- filter(df4nacheck, !is.na(wl))
  if(sum(difftime(df4nacheck[,"t"], lag(df4nacheck[,"t"]))>maxGap, na.rm = T)>=1){
    return(NA)
  }
  if(max(WLdf[,lcol], na.rm = T) < in.elev){
    IT <- 0
    return(IT)
  }
  if(min(WLdf[,lcol], na.rm = T) > in.elev){
    IT <- as.numeric(difftime(range(WLdf[,timecol])[2], range(WLdf[,timecol])[1]), units = unit)
    return(IT)
  }
  
  if(approx.method != "linear"&& approx.method !="spline"){
    stop("choose Spline or linear interpolation")
  }
  if(approx.method == "linear"){
    af.datetime <- approxfun(x = WLdf[,1], y = WLdf[,2])
  }
  if(approx.method == "spline"){
    af.datetime <- splinefun(x = WLdf[,1], y = WLdf[,2])
  }
  
  #n.switches <- sum((WLdf[, lcol] >= in.elev) != lag(WLdf[, lcol] >= in.elev), na.rm = T)
  d.range = range(WLdf[,timecol])
  per <- data.frame(t = seq.POSIXt(d.range[1], d.range[2], by = "min"))
  per$af <- af.datetime(per$t)
  
  per$wet <- per$af>=in.elev
  per$switch <- lag(per$wet)!=per$wet
  
  df <- data.frame(t1 = c(per[1,"t"], per[per$switch!=F & !is.na(per$switch), "t"]), 
                   t2 = c(per[per$switch!=F & !is.na(per$switch), "t"], per[length(per$t) ,"t"])) %>% 
    mutate(difftime = difftime(t2, t1), 
           wet = af.datetime(t1+(difftime/2)) >= in.elev)
  
  IT <- as.numeric(sum(df$difftime[df$wet == T]), units = unit)
  
  return(IT)
}