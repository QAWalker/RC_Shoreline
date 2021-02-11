predictions20 <- NOAA.WL(start = ymd("20200103"), end = ymd("20210101"), product = "predictions", stationid = 8656483)

tides20 <- predictions20 %>% 
  mutate(t = ymd_hm(t),
         v = as.numeric(v),
         datetime = as.character(t)) %>%
  as.data.frame() %>% 
  findTides() 

tides <- tides20 %>% 
  # findTides() %>% 
  filter(!is.na(tide)) %>% 
  mutate(daytimetide = ifelse(hour(datetime) >= 7 & hour(datetime) <= 19, T, F),
         date = date(datetime),
         time = paste0(hour(t), ":", minute(t)),
         day = weekdays(ymd_hms(datetime))) %>% 
  filter(daytimetide) %>% 
  dplyr::select("datetime", "date", "day", "time", "pred.wl" = "v", "tide", "daytimetide", -"t")

write_csv(
  x = tides20,
  path = "R:/CEE/QuinW/BFT 2020 Tides.csv",
  na = ""
)

weekdays(ymd_hms(datetime))
