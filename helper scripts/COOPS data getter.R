get_noaa.WL <-  function(product, begindate, enddate, datum, stationid, timezone, units, format){
  require(httr)
  
  GET(
    url = paste0(
      "https://tidesandcurrents.noaa.gov/api/datagetter?product=",
      product, #water_level
      "&application=NOAA.NOS.NCCOS.BFT",
      "&begin_date=",
      begindate, #20180801
      "&end_date=",
      enddate, #20190110
      "&datum=",
      datum, #NAVD
      "&station=",
      stationid, #8658120
      "&time_zone=",
      timezone, #lst
      "&units=", 
      units, #metric
      "&format=",
      format #json
    )
  )
}

NOAA.WL <-
  function(start, end, timezone = "lst",
           product = "water_level",
           datum = "NAVD",
           stationid,
           units = "metric") {
    
    require(lubridate)
    require(httr)
    require(jsonlite)
    
    
    format = "json"
    
    if(!is.Date(start)){stop("Start date must be in Date format")}
    if(!is.Date(end)){stop("End date must be in Date format")}  
    
    if (product %in% c("water_level", "water_temperature")) {
      dates <- seq.Date(from = start, to = end, by = "30 days")
      if (max(dates)!=end) {dates <- as.Date(c(dates, end + days(1))) }
    } else {dates <- c(start, end)}
    if(product=="datums"){
      dates <- c(start, end)
    }
    if(product == "predictions"){
      dates <- c(start, end)
    }
    
    for (d in 1:(length(dates)-1)) {#}
      i = dates[[d]]
      begindate = paste0(year(i), 
                         ifelse(month(i)<10, 
                                paste0("0", month(i)),
                                month(i)),
                         ifelse(day(i)<10, 
                                paste0("0", day(i)),
                                day(i))
      )
      
      j = dates[[d+1]]
      enddate = paste0(year(j), 
                       ifelse(month(j)<10, 
                              paste0("0", month(j)),
                              month(j)),
                       ifelse(day(j)<10, 
                              paste0("0", day(j)),
                              day(j))
      )
      
      get_WL <-
        get_noaa.WL(
          product = product,
          begindate = begindate,
          enddate = enddate,
          timezone = timezone,
          units = units,
          datum = datum,
          stationid = stationid,
          format = format
        )
      
      get_WL_txt <- content(get_WL, "text") 
      
      get_WL_json <- fromJSON(get_WL_txt)
      
      if(names(get_WL_json)[1] == "error"){
        return(paste("Error message:", get_WL_json$error$message))
      }
      
      if(product == "water_level"){
        get_WL_df <- as.data.frame(get_WL_json$data[ , c("t", "v")])
        names(get_WL_df) <- c("DateTime", "WL")
        get_WL_df$DateTime <- ymd_hm(get_WL_df$DateTime)
        get_WL_df$WL <- as.numeric(get_WL_df$WL)
      }else{ 
        if(product == "datums"){
        get_WL_df <- as.data.frame(get_WL_json$datums)
        #names(get_WL_df) <- c("Datum", paste0("WL.", datum))
        }else{
          get_WL_df <- as.data.frame(get_WL_json$predictions)
        }
      }
      
      if(!exists("WLfromweb")) {
        WLfromweb <- get_WL_df
      } else{
        WLfromweb <- rbind(WLfromweb, get_WL_df)
      }
      rm(get_WL)
      rm(get_WL_txt)
      rm(get_WL_json)
      rm(get_WL_df)
      require(dplyr)
      WLfromweb <- distinct(WLfromweb)
    }
    
    if(product == "water_level"){
      WLfromweb <- WLfromweb[order(WLfromweb$DateTime), ]
      WLfromweb <- filter(WLfromweb, DateTime >= start & DateTime <= end)
    }
    
    Sys.sleep(time = 0.5)
    return(WLfromweb)
  }

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

findTides <- function(WLdf, w = 11, boundary = "discard"){
  
  timecol <- which(sapply(WLdf, is.POSIXt))[[1]]
  lcol <- which(!sapply(WLdf, is.POSIXt))[[1]]
  WLdf$tide <- NA
  
  WLdf <- WLdf[!is.na(WLdf[,lcol]),]
  
  library(scorepeak)
  
  WLdf[,"isHighTide"] <- isHighTide <- detect_localmaxima(data = WLdf[,lcol], w = w, boundary = boundary)
  WLdf[,"isLowTide"] <- isHighTide <- detect_localmaxima(data = WLdf[,lcol]*-1, w = w, boundary = boundary)
  
  for (i in which(WLdf$isHighTide)) {
    d <- date(WLdf[i, timecol])
    WLdf[i, "tide"] <- ifelse(WLdf[i,lcol]>=max(WLdf[date(WLdf[,timecol]) == d, lcol]), "HH", "H")
  }
  
  for (i in which(WLdf$isLowTide)) {
    d <- date(WLdf[i, timecol])
    WLdf[i, "tide"] <- ifelse(WLdf[i,lcol]<=min(WLdf[date(WLdf[,timecol]) == d, lcol]), "LL", "L")
  }
  
  return(dplyr::select(WLdf, -c("isHighTide", "isLowTide")))
}

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
