rm(list = ls())

library(tidyverse)
library(reshape2)
library(lubridate)
library(readr)

#create color-blind pallette for plotting
cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))
#run the data wrangling script to create our data frames for plotting
source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/combined wrangling.R")
# source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/Master Veg Data Wrangling.R")

source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/ncvs2value.R")
source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/ncvs2bb.R")
source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/bb2value.R")

surveyDates <- VegData %>% 
  group_by(Year, Site, SampTime) %>% 
  summarize(firstsurvey = range(ymd(paste(Year, Month, Day, sep = "-")))[1],
            lastsurvey = range(ymd(paste(Year, Month, Day, sep = "-")))[2]) %>% 
  ungroup()

names <- names(read.csv("R:/CEE/RC shoreline/Data/Meterological Data/Beaufort Michael J Smith field.csv"))
MetData.bft <-
  read_csv(
    "R:/CEE/RC shoreline/Data/Meterological Data/Beaufort Michael J Smith field.csv",
    #col_types = rep("c", length(names)),
    col_names = names,
    na = c("", "-9999")
  )[-1,]

rainfall <- MetData.bft %>% 
  filter(ELEMENT == "PRCP") %>% 
  melt(id.vars = c("YEAR", "MONTH", "ELEMENT")) %>% 
  mutate(variable = as.character(variable)) %>% 
  filter(variable != "STATION.ID") %>% 
  mutate(DAY = as.integer(substr(variable, 6, nchar(variable))), 
         MONTH = as.integer(MONTH),
         YEAR = as.integer(YEAR),
         variable = substr(variable, 1, 5),
         DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>% #dates that do not exist return a warning
  dplyr::select("DATE", "YEAR", "MONTH", "DAY", everything()) %>% 
  filter(!is.na(DATE))

rainfall.wide <- rainfall %>% 
  dcast(DATE + YEAR + MONTH + DAY + ELEMENT ~ variable) %>% 
  mutate(VALUE = as.numeric(VALUE) / 10) %>% 
  dplyr::select("DATE", "YEAR", "MONTH", "DAY","RAIN.mm" = "VALUE", everything(), -"ELEMENT")

rainfall.monthly <- rainfall.wide %>% 
  group_by(YEAR, MONTH) %>% 
  summarize(RAIN.mm = sum(RAIN.mm, na.rm = T)) %>% 
  ungroup()
# write.csv(x = rainfall.monthly, "R:/CEE/RC shoreline/Data/Meterological Data/Beaufort Michael J Smith field MONTHLY.csv", row.names = F)

drought <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/Environmental Data.csv", stringsAsFactors = F) %>%
  mutate(Date= myd(paste0(Date, "01")))

names(drought) <- c("Date", "Month", "Year", "Precip.cm", "Temp.C", "HDDays", "CDDays", "PDSI", "PHDI", "PZI", "MPDSI", "SPI.1", "SPI.2", "SPI.3", "SPI.6")

drought <- full_join(drought, data.frame(Date = mdy(paste(rep(paste(1:12, 01, sep = "-"), 14), rep(2006:2019, each = 12), sep = "-")))) %>% 
  mutate(Month = month(Date), 
         Year = year(Date))

localPDSI <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/PDSI from UNL Drought Atlas at MHC.csv", stringsAsFactors = F) %>% 
  mutate(Date = as.Date(mdy_hms(Date)),
         PDSI.MHC = PDSI)

drought <- left_join(drought, select(localPDSI, "Date", "PDSI.MHC"), by = "Date")

KBDI <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/KBDI time series.csv", stringsAsFactors = F) %>% 
  mutate(Date = ymd(Date))

drought <- left_join(drought, KBDI, by = "Date")

localSPI <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/SPI local from Drought Atlas UNL.csv", stringsAsFactors = F) %>% 
  mutate(Date = mdy(Date))

drought <- left_join(drought, localSPI, by = "Date")

localSPI.NCCD <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/local SPI from nc climate office.csv", stringsAsFactors = F) %>%
  mutate(Date = mdy(Date)) %>% 
  rename_if(startsWith(names(.), "SPI"), ~paste0(., ".NCCD"))


drought <- left_join(drought, localSPI.NCCD, by = "Date")

envidata <- list("GS" = data.frame(surveyDates), "YTD" = data.frame(surveyDates), "insta" = data.frame(surveyDates))

envidata$GS$Precip.GS.mm <- sapply(X = surveyDates$firstsurvey, FUN = function(date){
  sum(rainfall.wide$RAIN.mm[rainfall.wide$DATE >= ymd(paste(year(date), 04, 01, sep = "-")) &
                              rainfall.wide$DATE <= date], na.rm = T)
})

envidata$YTD$Precip.YTD.mm <- sapply(X = surveyDates$firstsurvey, FUN = function(date){
  sum(rainfall.wide$RAIN.mm[rainfall.wide$DATE >= ymd(paste(year(date), 01, 01, sep = "-")) &
                              rainfall.wide$DATE <= date], na.rm = T)
})

for (var in c("Temp.C", "PDSI", "PHDI", "PZI", "MPDSI", "KBDI", "PDSI.MHC")) {
  envidata$GS[,paste0(var, ".GS.mean")] <- 
    sapply(
      X = surveyDates$firstsurvey, 
      FUN = function(date){
        mean(drought[drought$Date >= ymd(paste(year(date), 04, 01, sep = "-")) &
                       drought$Date <= date, var], na.rm = T)
      }
    )
  envidata$GS[,paste0(var, ".GS.sum")] <- 
    sapply(
      X = surveyDates$firstsurvey, 
      FUN = function(date){
        sum(drought[drought$Date >= ymd(paste(year(date), 04, 01, sep = "-")) &
                      drought$Date <= date, var], na.rm = T)
      }
    )
  
  envidata$YTD[,paste0(var, ".YTD.mean")] <- 
    sapply(
      X = surveyDates$firstsurvey, 
      FUN = function(date){
        mean(drought[drought$Date >= ymd(paste(year(date), 01, 01, sep = "-")) &
                       drought$Date <= date, var], na.rm = T)
      }
    )
  
  envidata$YTD[,paste0(var, ".YTD.sum")] <- 
    sapply(
      X = surveyDates$firstsurvey, 
      FUN = function(date){
        sum(drought[drought$Date >= ymd(paste(year(date), 01, 01, sep = "-")) &
                      drought$Date <= date, var], na.rm = T)
      }
    )
}

for(var in c(names(drought)[which(grepl(pattern = "SPI", names(drought)))], "PDSI", "PHDI", "PZI", "MPDSI", "KBDI", "PDSI.MHC")){
  envidata$insta[, paste0(var, ".I")]  <-
    unlist(
      sapply(X = surveyDates$firstsurvey,
             FUN = function(date){
               as.numeric(drought[drought$Month == month(date) & drought$Year == year(date), var])
             })
    )
}

#### inundation ####
source("R:/CEE/QuinW/R Funs/COOPS data getter.R")

bftWL <- list()
for (yr in 2006:2019) {
  bftWL[[paste(yr)]] <- read.csv(paste0("R:/CEE/QuinW/BFT DUML Water levels/BFTWL ", yr,".csv"), stringsAsFactors = F) %>% 
    mutate(DateTime = ymd_hms(DateTime))
}

envidata$datums <- data.frame(surveyDates)
startMSL = -0.1375083 
for(i in 1:length(envidata$datums$firstsurvey)){#}
  date <- as.character(envidata$datums$firstsurvey[i])
  
  d <- calcDatums(filter(bftWL[[as.character(unique(year(date)))]]))
  envidata$datums[i, names(d)] <- d
  
  d <- calcDatums(filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date))
  envidata$datums[i, paste0(names(d), ".YTD")] <- d
  
  d <- calcDatums(filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date, DateTime >= mdy(paste0("04-01-", as.character(unique(year(date)))))))
  envidata$datums[i, paste0(names(d), ".GS")] <- d
  
  envidata$datums[i, "t"] <- as.numeric(inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date),
                                                       in.elev = min(filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date)$WL)-1), units = "days")
  
  envidata$datums[i, "t.YTD"] <- as.numeric(ymd(date) - ymd(paste0(year(date), "0101")), units = "days")
  
  envidata$datums[i, "t.GS"] <- as.numeric(ymd(date) - ymd(paste0(year(date), "0401")), units = "days")
  
  for (dat in names(d)) {
    #envidata$datums[i, paste0(dat, "_IT.mins")] <- inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date), 
    #                                                                   in.elev = as.numeric(d[dat]))
    #envidata$datums[i, paste0(dat, "_IT.pct")] <- envidata$datums[i, paste0(dat, "_IT.mins")] / envidata$datums[i, "t"]
    
    #IT <- inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date), 
    #                                                              in.elev = as.numeric(d[dat]))
    #envidata$datums[i, paste0(dat, "_IT.pct")] <- IT / envidata$datums[i, "t"]
  }
  envidata$datums[i, paste0("IT.pct", seq(-.5, .5, by = 0.25))] <- sapply(seq(-.5, .5, by = 0.25),
                                                                          FUN = function(L){
                                                                            inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date), in.elev = L) / envidata$datums[i, "t"]
                                                                          }
  )
  
  envidata$datums[i, paste0("GS.IT.pct", seq(-.5, .5, by = 0.25))] <- sapply(seq(-.5, .5, by = 0.25),
                                                                             FUN = function(L){
                                                                               inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date, DateTime>= mdy(paste0("0401", as.character(unique(year(date)))))), in.elev = L) / (envidata$datums[i, "t.GS"]*24*60)
                                                                             }
  )
  envidata$datums[i, "startMSL_IT.pct"] <- inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date), 
                                                          in.elev = startMSL) / envidata$datums[i, "t"]
} 

#x <- lapply(names(envidata), function(n){write_csv(envidata[[n]], paste0("R:/CEE/RC shoreline/Data/Meterological Data/survey dates and environmental variables ",n,".csv"))})
#rm(x)
drought.melt <- melt(drought, id.vars = c("Date", "Month", "Year"))

envidata$all <- full_join(envidata$GS, full_join(envidata$YTD, full_join(envidata$insta, envidata$datums)))

write_csv(envidata[["all"]], paste0("R:/CEE/RC shoreline/Data/Meterological Data/survey dates and environmental variables all.csv"))

envidata.melt <- envidata$all %>% 
  melt(., id.vars = names(surveyDates)) 

#unique(envidata.melt.p$variable)

envidata.melt.p <- filter(envidata.melt, 
                          Site == "PI",
                          grepl(x = SampTime, pattern = "SU"), 
                          variable %in% c("Precip.GS.mm", "Temp.C.GS.mean", 
                                          "KBDI.I",
                                          "MSL.GS",
                                          "SPI.1.NCCD.I",
                                          "SPI.4.NCCD.I", 
                                          "t.GS",
                                          "GS.IT.pct0")) %>% 
  droplevels.data.frame() %>% 
  mutate(variable = as.character(variable),
         variable = factor(variable),
         variable = fct_recode(variable,
                               "Total Precipitation (mm)"= "Precip.GS.mm",
                               "Mean Temperature (C)" = "Temp.C.GS.mean",
                               "Keetch-Byram Drought Index" = "KBDI.I",
                               "1 Month Stnd. Precip. Index" = "SPI.1.NCCD.I",
                               "4 Month Stnd. Precip. Index" = "SPI.4.NCCD.I",
                               "GS Mean Sea Level (m NAVD)" = "MSL.GS",
                               "GS Inundation at 0m NAVD (% time)" = "GS.IT.pct0",
                               "Growing season Length (days)" = "t.GS"),
         variable = fct_relevel(variable,
                                "Total Precipitation (mm)",
                                "Mean Temperature (C)",
                                "GS Mean Sea Level (m NAVD)",
                                "GS Inundation at 0m NAVD (% time)",
                                "Keetch-Byram Drought Index",
                                "1 Month Stnd. Precip. Index",
                                "4 Month Stnd. Precip. Index",
                                "Growing season Length (days)"))
# variable = fct_recode(variable,
#                       "Annual" = "Annual Mean Sea Level (m NAVD)" ,
#                       "Growing Season" = "GS Mean Sea Level (m NAVD)"))

#x <- with(envidata.melt.p, summary(lm(value ~ firstsurvey)))

rsq <- filter(envidata.melt.p, grepl(x = SampTime, pattern = "SU")) %>% 
  group_by(variable) %>% 
  summarize(m = round(summary(lm(value ~ Year))$coefficients[2], 3),
            b = round(summary(lm(value ~ Year))$coefficients[1], 2),
            r2 = round(summary(lm(value ~ Year))$r.squared, 3),
            x = min(firstsurvey, na.rm = T),
            y = max(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2)

#levels(envidata.melt.p$variable)

ggplot(filter(envidata.melt.p, grepl(x = SampTime, pattern = "SU")), aes(firstsurvey, value)) +
  geom_text(data = rsq,
            aes(x, y, label = paste0("~r^2 ==", r2), color = sig),
            size = 4.5, parse = T, hjust = 0, vjust = 1, show.legend = F)+
  geom_smooth(method = "lm", se = F, color = "darkgray")+
  geom_point() +
  facet_wrap( ~ variable, scales = "free", nrow = 2)+
  scale_color_manual(values = c("gray", "black"))+
  scale_x_date(date_labels = "'%y")+
  labs(x = NULL, 
       y = NULL,
       title = "Growing Season Environmental Variables",
       subtitle = "Measured April 1 - Survey Date at Pivers Island")

ggsave(
  filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/PI Environmental data.png"),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

rsq <- filter(envidata$all, grepl(x = SampTime, pattern = "SU")) %>% 
  summarize(m = round(summary(lm(Precip.GS.mm ~ t.GS))$coefficients[2], 3),
            b = round(summary(lm(Precip.GS.mm ~ t.GS))$coefficients[1], 2),
            r2 = round(summary(lm(Precip.GS.mm ~ t.GS))$r.squared, 3)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2)

ggplot(filter(envidata$all, Site %in% c('PI', 'PKS', 'NCMM'), grepl(x = SampTime, pattern = "SU")), aes(t.GS, Precip.GS.mm))+
  geom_smooth(method = "lm", se = F, color = "black")+
  geom_point(aes(color = as.factor(Year), shape = Site), size = 3)+
  geom_text(data = rsq, aes(x = 90, y = 550, label = paste0("~r^2 ==", r2)), 
            color = "black", size = 4.5, parse = T, hjust = 0, vjust = 1, show.legend = F)+
  labs(x = "Growing Season Duration (days)", 
       y = "Growing Season Total Precipitation (mm)", 
       color = "Year", 
       title = "Growing Season Length and Total Precipitation")
ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/Growing Season Length and Precip.png"),
       width = 9, height = 6, units = "in", dpi = 300)

#### clean up the global environment ####
rm(d, KBDI, localPDSI, localSPI, localSPI.NCCD, masterVegData.summary.trt, masterVegData.summary.trt.ftr, masterVegData.summary.trt.ftr.envi)
rm(surveyDates, VegData, VegData.melt, VegData.summary, VegData.summary.trt, VegData.summary.trt.ftr, VegData0607, VegData0607.melt, VegData0607.summary, VegData0607.summary.trt, VegData0607.summary.trt.ftr)
rm(comCols, ct, dat, date, i, names, nms, PctCoverColNums, QuantColNums, run, SppColNums, startMSL, var, yr)

####Veg Data comparison with Environmental data####
envidata.summary <- envidata.melt.p %>%  
  filter(Site %in% c("PI", "PKS", "NCMM")) %>% 
  group_by(Year, SampTime, variable) %>% 
  summarise(value = mean(value, na.rm = F)) %>% 
  ungroup() 

masterVegData.summary.site <- VegData.melt %>% 
  # select(-'variable.sppname', -'variable.commonname', -'variable.axislabel', -"Transect_f") %>% 
  filter(!(Feature %in% c("Adjacent", "Eroded")), 
         Site %in% c("NCMM", 'PKS', 'PI')) %>% 
  group_by(Treatment, Season, variable, Year, SampTime, Plot) %>% 
  summarize(Date = mean(Date, na.rm = T),
            mean = mean(value, na.rm = T), 
            sd = sd(value, na.rm = T), 
            n = n_distinct(value, na.rm = T), 
            se = NA) %>% 
  ungroup() %>% 
  rename(variable.veg = variable) %>% 
  mutate(se = ifelse(n == 1, NA, sd / sqrt(n))) %>% 
  left_join(envidata.summary) %>% 
  filter(grepl(x = SampTime, pattern = "SU"))
#variable %in% c("Precip.GS.mm",
#"Temp.C.GS.mean",
#"PDSI.GS.mean", "PDSI.I", "PDSI.MHC.GS.mean", "PDSI.MHC.I",
#"KBDI.GS.mean", "KBDI.I",
#"MSL", "MSL.YTD", "MSL.GS",
#"SPI.1.I", "SPI.6.I",
#"SPI.1.NCCD.I", "SPI.6.NCCD.I",
#"SPI.1.MHC.I", "SPI.6.MHC.I") %>%
# mutate(variable = as.character(variable),
#        variable = factor(variable),
#        # variable = fct_recode(variable,
#        #                       "Total Precipitation (mm)"= "Precip.GS.mm",
#        #                       "Mean Temperature (C)" = "Temp.C.GS.mean",
#        #                       "Mean Palmer Drought Severity Index" = "PDSI.GS.mean",
#        #                       "Instant Palmer Drought Severity Index" = "PDSI.I",
#        #                       "Mean Local Palmer Drought Severity Index" = "PDSI.MHC.GS.mean",
#        #                       "Instant Local Palmer Drought Severity Index" = "PDSI.MHC.I",
#        #                       "Mean Keetch-Byram Drought Index" = "KBDI.GS.mean",
#        #                       "Instant Keetch-Byram Drought Index" = "KBDI.I",
#        #                       "1 Month Precipitation Index" = "SPI.1.I",
#        #                       "6 Month Precipitation Index" = "SPI.6.I",
#        #                       "1 Month Local Precipitation Index" = "SPI.1.MHC.I",
#        #                       "6 Month Local Precipitation Index" = "SPI.6.MHC.I",
#        #                       "1 Month Grid Precipitation Index" = "SPI.1.NCCD.I",
#        #                       "6 Month Grid Precipitation Index" = "SPI.6.NCCD.I",
#        #                       "GS Mean Sea Level (m NAVD)" = "MSL.GS",
#        #                       "Annual Mean Sea Level (m NAVD)" = "MSL",
#        #                       "YTD Mean Sea Level (m NAVD)" = "MSL.YTD"),
#        variable = fct_relevel(variable,
#                               "Total Precipitation (mm)",
#                               "Mean Temperature (C)",
#                               "Mean Palmer Drought Severity Index",
#                               "Instant Palmer Drought Severity Index",
#                               "Mean Local Palmer Drought Severity Index",
#                               "Instant Local Palmer Drought Severity Index",
#                               "Mean Keetch-Byram Drought Index",
#                               "Instant Keetch-Byram Drought Index",
#                               "1 Month Precipitation Index",
#                               "6 Month Precipitation Index",
#                               "1 Month Local Precipitation Index",
#                               "6 Month Local Precipitation Index",
#                               "1 Month Grid Precipitation Index",
#                               "6 Month Grid Precipitation Index",
#                               "Annual Mean Sea Level (m NAVD)",
#                               "GS Mean Sea Level (m NAVD)",
#                               "YTD Mean Sea Level (m NAVD)"))

levels(masterVegData.summary.site$variable)

if(!exists("spplist")) {
  spplist <- list()
} else{
  if (!is_list(spplist)) {
    spplist <- list()
  }
}
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$pctcover.plotting <- c("Salt", "Spat", "Sspp", "oyster_live", "oyster_culch", "wrack", "mussel")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$quant.plotting <- c("liveStem_m2", "HMean", "snails_m2")

tempdf <- filter(masterVegData.summary.site,
                 grepl(x = SampTime, pattern = "SU"),
                 Plot <= 20, 
                 #Site == "PI",
                 variable.veg == "liveStem_m2",
                 variable == "Growing season Length (days)") %>% 
  droplevels.data.frame()

rsq <- tempdf %>% 
  group_by(Plot) %>% 
  summarize(m = round(summary(lm(mean ~ value))$coefficients[2], 3),
            b = round(summary(lm(mean ~ value))$coefficients[1], 2),
            r2 = round(summary(lm(mean ~ value))$r.squared, 3),
            x = min(value, na.rm = T),
            y = max(mean, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2)

ggplot(tempdf,
       aes(value, mean, color = as.factor(Year)))+
  geom_smooth(method = "lm", se = F, color = "black")+
  geom_point(aes(shape = Treatment), size = 3)+
  facet_wrap(~Plot, scales = "free")+
  geom_text(data = rsq, aes(x = x, y = y, label = paste0("~r^2 ==", r2)), 
            color = "black", size = 4.5, parse = T, hjust = 0, vjust = 1, show.legend = F)+
  # scale_color_manual(values = c(dichromat::colorschemes$Categorical.12, (gray.colors(3)[2:1])))+
  labs(title = "Growing Season length and S. alterniflora Density",
       color = "Year", 
       y = "Mean Stem Density",
       x = "Growing Season Duration (days)")
# ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/Growing Season Length and S. alt density.png"),
#        width = 9, height = 6, units = "in", dpi = 300)

tempdf <- filter(masterVegData.summary.site,
                 grepl(x = SampTime, pattern = "SU"),
                 Plot <= 20, 
                 #Site == "PI",
                 variable.veg == "HMean",
                 variable == "Growing season Length (days)") %>% 
  droplevels.data.frame()

rsq <- tempdf %>% 
  group_by(Plot) %>% 
  summarize(m = round(summary(lm(mean ~ value))$coefficients[2], 3),
            b = round(summary(lm(mean ~ value))$coefficients[1], 2),
            r2 = round(summary(lm(mean ~ value))$r.squared, 3)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2)

min(tempdf$value, na.rm = T)
max(tempdf$mean, na.rm = T)

#install.packages("dichromat")
# library(dichromat)

ggplot(tempdf, aes(value, mean, color = as.factor(Year)))+
  geom_smooth(method = "lm", se = F, color = "black")+
  geom_point(aes(shape = Treatment), size = 3)+
  facet_wrap(~Plot)+
  geom_text(data = rsq, aes(x = 105, y = 93.78929, label = paste0("~r^2 ==", r2)), 
            color = "black", size = 4.5, parse = T, hjust = 0, vjust = 1, show.legend = F)+
  # scale_color_manual(values = c(dichromat::colorschemes$Categorical.12, (gray.colors(3)[2:1])))+
  labs(y = "Mean Height (cm)",
       x = "Growing Season Duration (days)", 
       title = "Growing Season Length and S. alterniflora Height",
       color = "Year")
# ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/Growing Season Length and S. alt height.png"),
#        width = 9, height = 6, units = "in", dpi = 300)

levels(masterVegData.summary.trt.ftr.envi$variable)[grep(x = levels(masterVegData.summary.trt.ftr.envi$variable), pattern = "SPI")]
spp = "Salt"
for (spp in spplist$pctcover.plotting) {#}
  envidata.melt.p <- filter(envidata.melt, 
                            # Site == "PI",
                            grepl(x = SampTime, pattern = "SU"), 
                            variable %in% c("Precip.GS.mm", "Temp.C.GS.mean", 
                                            "KBDI.I",
                                            "MSL.GS",
                                            "SPI.1.NCCD.I",
                                            "SPI.4.NCCD.I", 
                                            "t.GS",
                                            "GS.IT.pct0")) %>% 
    droplevels.data.frame() %>% 
    mutate(variable = as.character(variable),
           variable = factor(variable),
           variable = fct_recode(variable,
                                 "Total Precipitation (mm)"= "Precip.GS.mm",
                                 "Mean Temperature (C)" = "Temp.C.GS.mean",
                                 "Keetch-Byram Drought Index" = "KBDI.I",
                                 "1 Month Stnd. Precip. Index" = "SPI.1.NCCD.I",
                                 "4 Month Stnd. Precip. Index" = "SPI.4.NCCD.I",
                                 "GS Mean Sea Level (m NAVD)" = "MSL.GS",
                                 "GS Inundation at 0m NAVD (% time)" = "GS.IT.pct0",
                                 "Growing season Length (days)" = "t.GS"),
           variable = fct_relevel(variable,
                                  "Total Precipitation (mm)",
                                  "Mean Temperature (C)",
                                  "GS Mean Sea Level (m NAVD)",
                                  "GS Inundation at 0m NAVD (% time)",
                                  "Keetch-Byram Drought Index",
                                  "1 Month Stnd. Precip. Index",
                                  "4 Month Stnd. Precip. Index",
                                  "Growing season Length (days)"))
  
  unique(envidata.melt.p$variable)
  
  envidata.summary <- envidata.melt.p %>%  
    filter(Site %in% c("PI", "PKS", "NCMM")) %>% 
    group_by(Site, Year, SampTime, variable) %>% 
    summarise(value = mean(value, na.rm = F)) %>% 
    ungroup() 
  
  unique(envidata.summary$variable)
  
  masterVegData.summary.trt.ftr.envi <- masterVegData.melt %>% 
    select(-'variable.sppname', -'variable.commonname', -'variable.axislabel', -"Transect_f") %>% 
    filter(!(Feature %in% c("Adjacent", "Eroded"))) %>% 
    group_by(Site, Treatment, Season, variable, Year, SampTime, Plot) %>% 
    summarize(Date = mean(Date, na.rm = T),
              mean = mean(value, na.rm = T), 
              sd = sd(value, na.rm = T), 
              n = n_distinct(value, na.rm = T), 
              se = NA) %>% 
    ungroup() %>% 
    rename(variable.veg = variable) %>% 
    mutate(se = ifelse(n == 1, NA, sd / sqrt(n))) %>% 
    left_join(envidata.summary) %>% 
    filter(grepl(x = SampTime, pattern = "SU"))
  
  unique(masterVegData.summary.trt.ftr.envi$variable)
  
  # table(envidata$all$SPI.1.NCCD.I)
  vegdata.summary.p <- masterVegData.summary.trt.ftr.envi %>% 
    filter(grepl(x = SampTime, pattern = "SU"),
           Plot <=20,
           variable.veg == spp) %>% 
    filter(!is.na(variable)) %>% 
    droplevels.data.frame()
  
  vegdata.summary.p <- rbind(vegdata.summary.p, 
                             mutate(
                               distinct(
                                 select(vegdata.summary.p, -c("variable", "value"))
                               ),
                               variable = "Year",
                               value = Year
                             )
  )
  
  sppname = sppnames$sppname[sppnames$abb==spp] 
  commonname = sppnames$commonname[sppnames$abb==spp] 
  axisname = sppnames$axis.label[sppnames$abb==spp] 
  unit = sppnames$unit[sppnames$abb==spp][[1]]
  unitname = sppnames$unitname[sppnames$abb==spp]
  suffix = "? SE"
  
  require(dplyr)
  rsq <- vegdata.summary.p %>% 
    filter(!is.na(variable)) %>% 
    droplevels.data.frame() %>% 
    group_by(Treatment, Plot, variable) %>% 
    summarize(r2 = round(summary(lm(mean ~ value))$r.squared, 2),
              x = min(value, na.rm = T),
              y = max(mean, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(sig = (r2)>0.2)
  
  unique(vegdata.summary.p$variable)
  
  ggplot(vegdata.summary.p, aes(value, mean, color = Treatment))+
    geom_smooth(method = "lm", se = F, color = "black")+
    geom_point(size = 2, show.legend = F)+
    geom_text(data = rsq, aes(x, y, label = paste("r^2 ==", r2), color = sig),  size = 5, parse=T, hjust = 0, vjust = 1, show.legend = F)+
    #geom_text(data = rsq, aes(x, 100, label = paste("r^2 ==", r2), color = sig),  size = 5, parse=T, hjust = 0, vjust = 1, show.legend = F)+
    facet_grid(Treatment+Plot~variable, scales = "free")+
    scale_color_manual(values = c("gray20", cbPalette[2:3], "red"))+
    #coord_cartesian(ylim = c(0, 100))+
    labs(x = "environmental variable value", 
         y = paste("mean", axisname, "percent cover", suffix),
         title = paste0(sppname, " Percent Cover and Environmental Data")) + 
    theme(strip.text.x = element_text(size = 7))
  
  ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/",
                           axisname, " percent cover and Environmental data.png"),
         width = 16, height = 9, units = "in", dpi = 300)
}

for (spp in spplist$quant.plotting) {
  envidata.melt.p <- filter(envidata.melt, 
                            # Site == "PI",
                            grepl(x = SampTime, pattern = "SU"), 
                            variable %in% c("Precip.GS.mm", "Temp.C.GS.mean", 
                                            "KBDI.I",
                                            "MSL.GS",
                                            "SPI.1.NCCD.I",
                                            "SPI.4.NCCD.I", 
                                            "t.GS",
                                            "GS.IT.pct0")) %>% 
    droplevels.data.frame() %>% 
    mutate(variable = as.character(variable),
           variable = factor(variable),
           variable = fct_recode(variable,
                                 "Total Precipitation (mm)"= "Precip.GS.mm",
                                 "Mean Temperature (C)" = "Temp.C.GS.mean",
                                 "Keetch-Byram Drought Index" = "KBDI.I",
                                 "1 Month Stnd. Precip. Index" = "SPI.1.NCCD.I",
                                 "4 Month Stnd. Precip. Index" = "SPI.4.NCCD.I",
                                 "GS Mean Sea Level (m NAVD)" = "MSL.GS",
                                 "GS Inundation at 0m NAVD (% time)" = "GS.IT.pct0",
                                 "Growing season Length (days)" = "t.GS"),
           variable = fct_relevel(variable,
                                  "Total Precipitation (mm)",
                                  "Mean Temperature (C)",
                                  "GS Mean Sea Level (m NAVD)",
                                  "GS Inundation at 0m NAVD (% time)",
                                  "Keetch-Byram Drought Index",
                                  "1 Month Stnd. Precip. Index",
                                  "4 Month Stnd. Precip. Index",
                                  "Growing season Length (days)"))
  
  unique(envidata.melt.p$variable)
  
  envidata.summary <- envidata.melt.p %>%  
    filter(Site %in% c("PI", "PKS", "NCMM")) %>% 
    group_by(Site, Year, SampTime, variable) %>% 
    summarise(value = mean(value, na.rm = F)) %>% 
    ungroup() 
  
  unique(envidata.summary$variable)
  
  masterVegData.summary.trt.ftr.envi <- masterVegData.melt %>% 
    select(-'variable.sppname', -'variable.commonname', -'variable.axislabel', -"Transect_f") %>% 
    filter(!(Feature %in% c("Adjacent", "Eroded"))) %>% 
    group_by(Site, Treatment, Season, variable, Year, SampTime, Plot) %>% 
    summarize(Date = mean(Date, na.rm = T),
              mean = mean(value, na.rm = T), 
              sd = sd(value, na.rm = T), 
              n = n_distinct(value, na.rm = T), 
              se = NA) %>% 
    ungroup() %>% 
    rename(variable.veg = variable) %>% 
    mutate(se = ifelse(n == 1, NA, sd / sqrt(n))) %>% 
    left_join(envidata.summary) %>% 
    filter(grepl(x = SampTime, pattern = "SU"))
  
  unique(masterVegData.summary.trt.ftr.envi$variable)
  
  # table(envidata$all$SPI.1.NCCD.I)
  vegdata.summary.p <- masterVegData.summary.trt.ftr.envi %>% 
    filter(grepl(x = SampTime, pattern = "SU"),
           Plot <=20,
           variable.veg == spp) %>% 
    filter(!is.na(variable)) %>% 
    droplevels.data.frame()
  
  vegdata.summary.p <- rbind(vegdata.summary.p, 
                             mutate(
                               distinct(
                                 select(vegdata.summary.p, -c("variable", "value"))
                               ),
                               variable = "Year",
                               value = Year
                             )
  )
  
  sppname = sppnames$sppname[sppnames$abb==spp] 
  commonname = sppnames$commonname[sppnames$abb==spp] 
  axisname = sppnames$axis.label[sppnames$abb==spp] 
  unit = sppnames$unit[sppnames$abb==spp][[1]]
  unitname = sppnames$unitname[sppnames$abb==spp]
  suffix = "? SE"
  
  require(dplyr)
  rsq <- vegdata.summary.p %>% 
    filter(!is.na(variable)) %>% 
    droplevels.data.frame() %>% 
    group_by(Treatment, Plot, variable) %>% 
    summarize(r2 = round(summary(lm(mean ~ value))$r.squared, 2),
              x = min(value, na.rm = T),
              y = max(mean, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(sig = (r2)>0.2)
  
  ggplot(vegdata.summary.p, aes(value, mean, color = Treatment))+
    geom_smooth(method = "lm", se = F, color = "black")+
    geom_point(size = 2, show.legend = F)+
    geom_text(data = rsq, aes(x, y, label = paste("r^2 ==", r2), color = sig),  size = 5, parse=T, hjust = 0, vjust = 1, show.legend = F)+
    facet_grid(Treatment+Plot~variable, scales = "free")+
    scale_color_manual(values = c("gray20", cbPalette[2:3], "red"))+
    labs(x = "environmental variable value", 
         y = vegdata.summary.p$variable.axislabel[[1]],
         title = paste0(vegdata.summary.p$variable.sppname[[1]],
                        if_else(
                          vegdata.summary.p$variable.sppname[[1]] == vegdata.summary.p$variable.commonname[[1]],
                          " ",
                          paste0(" (", vegdata.summary.p$variable.commonname[[1]], ") ", 
                                 if_else(
                                   grepl(x = spp, pattern = "_m2"), "Density", "Mean Height"
                                 ))
                        ), " and Environmental Variables")) + 
    theme(strip.text.x = element_text(size = 7))
  
  
  ggsave(
    filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/", sppname, if_else(
      grepl(x = spp, pattern = "_m2"), " Density ", " Mean Height "
    ), " and Environmental data.png"),
    width = 16,
    height = 9,
    units = "in",
    dpi = 300
  )
  
}

### create a table of significiant rsq values for all species
rm(rsq.table)

envidata.melt.rs <- filter(envidata.melt, 
                           Site  %in% c("PI", 'PKS', 'NCMM'),
                           grepl(x = SampTime, pattern = "SU"), 
                           variable %in% c("Precip.GS.mm", "Temp.C.GS.mean", 
                                           "KBDI.I",
                                           "MSL.GS",
                                           "SPI.1.NCCD.I",
                                           "SPI.4.NCCD.I", 
                                           "t.GS",
                                           "GS.IT.pct0")) %>% 
  droplevels.data.frame() %>% 
  mutate(variable = as.character(variable),
         variable = factor(variable),
         variable = fct_recode(variable,
                               "Total Precipitation (mm)"= "Precip.GS.mm",
                               "Mean Temperature (C)" = "Temp.C.GS.mean",
                               "Keetch-Byram Drought Index" = "KBDI.I",
                               "1 Month Stnd. Precip. Index" = "SPI.1.NCCD.I",
                               "4 Month Stnd. Precip. Index" = "SPI.4.NCCD.I",
                               "GS Mean Sea Level (m NAVD)" = "MSL.GS",
                               "GS Inundation at 0m NAVD (% time)" = "GS.IT.pct0",
                               "Growing season Length (days)" = "t.GS"),
         variable = fct_relevel(variable,
                                "Total Precipitation (mm)",
                                "Mean Temperature (C)",
                                "GS Mean Sea Level (m NAVD)",
                                "GS Inundation at 0m NAVD (% time)",
                                "Keetch-Byram Drought Index",
                                "1 Month Stnd. Precip. Index",
                                "4 Month Stnd. Precip. Index",
                                "Growing season Length (days)"))
envidata.melt.rs <- rbind(envidata.melt.rs, 
                          mutate(
                            distinct(
                              select(envidata.melt.rs, -c("variable", "value"))
                            ),
                            variable = "Year",
                            value = Year
                          ))

envidata.summary <- envidata.melt.rs %>%  
  group_by(Site, Year, SampTime, variable) %>% 
  summarise(value = mean(value, na.rm = F)) %>% 
  ungroup() 

VegData.melt.envi <- VegData.melt %>% 
  # select(-'variable.sppname', -'variable.commonname', -'variable.axislabel', -"Transect_f") %>% 
  filter(!(Feature %in% c("Adjacent", "Eroded")),
         Site %in% c("NCMM", 'PKS', 'PI'),
         grepl(x = SampTime, pattern = "SU"), 
         Plot <= 20) %>% 
  # group_by(Treatment, Season, variable, Year, SampTime, Plot) %>% 
  # summarize(Date = mean(Date, na.rm = T),
  #           mean = mean(value, na.rm = T), 
  #           sd = sd(value, na.rm = T), 
  #           n = n_distinct(value, na.rm = T), 
  #           se = NA) %>% 
  # ungroup() %>% 
  rename(variable.veg = variable, 
         value.veg = value) %>% 
  # mutate(se = ifelse(n == 1, NA, sd / sqrt(n))) %>% 
  left_join(envidata.summary) %>% 
  rename(variable.envi = variable, 
         value.envi = value)

spplist$pctcover <- names(VegData)[PctCoverColNums]
spplist$quant <- names(VegData)[QuantColNums]

# ggplot(VegData.melt.envi, aes(value.envi, value.veg))+
#   geom_point()+
#   facet_wrap(~variable.envi, scales = "free")

VegData.melt.bb <- VegData %>% 
  filter(Transect != "4a", 
         Plot<= 20, 
         !(Feature %in% c("Adjacent", "Eroded")),
         Site %in% c("NCMM", 'PKS', 'PI'),
         grepl(x = SampTime, pattern = "SU")) %>%
  melt(
    id.vars = 1:which(names(.) == "Plot"),
    measure.vars = c(paste(names(VegData)[c(PctCoverColNums)], "_category", sep = ""), names(VegData)[QuantColNums]),
    factorsAsStrings = F
  ) %>% 
  mutate(value.bb = ifelse(SurveyMethod == "CVS", ncvs2bb(value), value))

VegData.melt.bb.envi <- VegData.melt.bb %>% 
  select(-value) %>% 
  mutate(variable = as.character(variable),
         variable = ifelse(variable %in% names(VegData)[QuantColNums], 
                           variable, stringr::str_remove(variable, "_category"))) %>% 
  filter(!(Feature %in% c("Adjacent", "Eroded")),
         Site %in% c("NCMM", 'PKS', 'PI'),
         grepl(x = SampTime, pattern = "SU"), 
         Plot <= 20) %>% 
  rename(variable.veg = variable, 
         value.veg = value.bb) %>% 
  left_join(envidata.summary) %>% 
  rename(variable.envi = variable, 
         value.envi = value) %>% 
  filter(variable.veg %in% spplist$pctcover) 

rsq.all <- VegData.melt.envi %>% 
  group_by(variable.veg, Treatment, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = as.character(Plot))

rsq.trt <- VegData.melt.envi %>% 
  group_by(variable.veg, Treatment, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = "All")

rsq.plt <- VegData.melt.envi %>% 
  group_by(variable.veg, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = as.character(Plot))

rsq.plt.trt <- VegData.melt.envi %>% 
  group_by(variable.veg, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = "All")

rsq.mid <- bind_rows(rsq.all, rsq.plt, rsq.trt, rsq.plt.trt) %>% 
  mutate(veg.method  = ifelse(variable.veg %in% spplist$pctcover, "Midpoint", "count"))

rsq.all.bb <- VegData.melt.bb.envi %>% 
  group_by(variable.veg, Treatment, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = as.character(Plot))

rsq.trt.bb <- VegData.melt.bb.envi %>% 
  group_by(variable.veg, Treatment, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = "All")

rsq.plt.bb <- VegData.melt.bb.envi %>% 
  group_by(variable.veg, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = as.character(Plot))

rsq.plt.trt.bb <- VegData.melt.bb.envi %>% 
  group_by(variable.veg, variable.envi) %>% 
  summarize(slope = coef(lm(value.veg ~ value.envi))[[2]],
            r2 = summary(lm(value.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = "All")

rsq.bb <- bind_rows(rsq.all.bb, rsq.plt.bb, rsq.trt.bb, rsq.plt.trt.bb) %>% 
  mutate(veg.method  = ifelse(variable.veg %in% spplist$pctcover, "BB", "count"))

rsq.table <- bind_rows(rsq.bb, rsq.mid) %>% 
  select(variable.veg, veg.method, everything())

write.csv(
  rsq.table, 
  file = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/Veg and Environmental R Squared Table.csv"),
  row.names = F
)

tdf <- VegData.melt.envi %>% 
  filter(variable.veg == "Salt", 
         Treatment == "Sill",
         Plot == 15, 
         variable.envi == "Keetch-Byram Drought Index") %>% 
  as.tbl()

ggplot(tdf, aes(y = value.veg, x = value.envi))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

##### Rsquare table with summary data
rm(rsq.table)

envidata.melt.rs <- filter(envidata.melt, 
                           Site  %in% c("PI", 'PKS', 'NCMM'),
                           grepl(x = SampTime, pattern = "SU"), 
                           variable %in% c("Precip.GS.mm", "Temp.C.GS.mean", 
                                           "KBDI.I",
                                           "MSL.GS",
                                           "SPI.1.NCCD.I",
                                           "SPI.4.NCCD.I", 
                                           "t.GS",
                                           "GS.IT.pct0")) %>% 
  droplevels.data.frame() %>% 
  mutate(variable = as.character(variable),
         variable = factor(variable),
         variable = fct_recode(variable,
                               "Total Precipitation (mm)"= "Precip.GS.mm",
                               "Mean Temperature (C)" = "Temp.C.GS.mean",
                               "Keetch-Byram Drought Index" = "KBDI.I",
                               "1 Month Stnd. Precip. Index" = "SPI.1.NCCD.I",
                               "4 Month Stnd. Precip. Index" = "SPI.4.NCCD.I",
                               "GS Mean Sea Level (m NAVD)" = "MSL.GS",
                               "GS Inundation at 0m NAVD (% time)" = "GS.IT.pct0",
                               "Growing season Length (days)" = "t.GS"),
         variable = fct_relevel(variable,
                                "Total Precipitation (mm)",
                                "Mean Temperature (C)",
                                "GS Mean Sea Level (m NAVD)",
                                "GS Inundation at 0m NAVD (% time)",
                                "Keetch-Byram Drought Index",
                                "1 Month Stnd. Precip. Index",
                                "4 Month Stnd. Precip. Index",
                                "Growing season Length (days)"))
envidata.melt.rs <- rbind(envidata.melt.rs, 
                          mutate(
                            distinct(
                              select(envidata.melt.rs, -c("variable", "value"))
                            ),
                            variable = "Year",
                            value = Year
                          ))

envidata.summary <- envidata.melt.rs %>%  
  group_by(Site, Year, SampTime, variable) %>% 
  summarise(value = mean(value, na.rm = F)) %>% 
  ungroup() 

VegData.summary.envi.rs <- VegData.melt %>% 
  filter(!(Feature %in% c("Adjacent", "Eroded")),
         Site %in% c("NCMM", 'PKS', 'PI'),
         grepl(x = SampTime, pattern = "SU"), 
         Plot <= 20) %>% 
  rename(variable.veg = variable, 
         value.veg = value) %>% 
  group_by(Site, Treatment, Season, variable.veg, Year, SampTime, Plot) %>%
  summarize(Date = mean(Date, na.rm = T),
            mean.veg = mean(value.veg, na.rm = T),
            sd.veg = sd(value.veg, na.rm = T),
            n.veg = n_distinct(value.veg, na.rm = T)) %>%
  ungroup() %>%
  left_join(envidata.summary) %>% 
  rename(variable.envi = variable, 
         value.envi = value)

tdf <- VegData.summary.envi.rs %>% 
  filter(variable.veg == "Salt", 
         Treatment == "Sill",
         Plot == 15, 
         variable.envi == "Keetch-Byram Drought Index") %>% 
  as.tbl()

ggplot(tdf, aes(y = mean.veg, x = value.envi))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

ggplot(filter(VegData.summary.envi.rs, variable.veg == "HMean"), aes(value.envi, mean.veg))+
  geom_point(aes(color = (Site), shape = Treatment))+
  facet_grid(Plot~variable.envi, scales = "free")+
  geom_smooth(method = 'lm', se = F)

VegData.melt.bb <- VegData %>% 
  filter(Transect != "4a", 
         Plot<= 20, 
         !(Feature %in% c("Adjacent", "Eroded")),
         Site %in% c("NCMM", 'PKS', 'PI'),
         grepl(x = SampTime, pattern = "SU")) %>%
  melt(
    id.vars = 1:which(names(.) == "Plot"),
    measure.vars = c(paste(names(VegData)[c(PctCoverColNums)], "_category", sep = ""), names(VegData)[QuantColNums]),
    factorsAsStrings = F
  ) %>% 
  mutate(value.bb = ifelse(SurveyMethod == "CVS", ncvs2bb(value), value))

VegData.summary.bb.envi <- VegData.melt.bb %>% 
  select(-value) %>% 
  mutate(variable = as.character(variable),
         variable = ifelse(variable %in% names(VegData)[QuantColNums], 
                           variable, stringr::str_remove(variable, "_category"))) %>% 
  filter(!(Feature %in% c("Adjacent", "Eroded")),
         Site %in% c("NCMM", 'PKS', 'PI'),
         grepl(x = SampTime, pattern = "SU"), 
         Plot <= 20) %>% 
  rename(variable.veg = variable, 
         value.veg = value.bb) %>% 
  group_by(Site, Treatment, Season, variable.veg, Year, SampTime, Plot) %>%
  summarize(Date = mean(Date, na.rm = T),
            mean.veg = mean(value.veg, na.rm = T),
            sd.veg = sd(value.veg, na.rm = T),
            n.veg = n_distinct(value.veg, na.rm = T)) %>%
  ungroup() %>%
  left_join(envidata.summary) %>% 
  rename(variable.envi = variable, 
         value.envi = value) %>% 
  filter(variable.veg %in% spplist$pctcover) 

rsq.sum.all <- VegData.summary.envi.rs %>% 
  group_by(variable.veg, Treatment, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = as.character(Plot))

rsq.sum.trt <- VegData.summary.envi.rs %>% 
  group_by(variable.veg, Treatment, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = "All")

rsq.sum.plt <- VegData.summary.envi.rs %>% 
  group_by(variable.veg, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = as.character(Plot))

rsq.sum.plt.trt <- VegData.summary.envi.rs %>% 
  group_by(variable.veg, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = "All")

rsq.sum.mid <- bind_rows(rsq.sum.all, rsq.sum.plt, rsq.sum.trt, rsq.sum.plt.trt) %>% 
  mutate(veg.method  = ifelse(variable.veg %in% spplist$pctcover, "Midpoint", "count"))

rsq.sum.all.bb <- VegData.summary.bb.envi %>% 
  group_by(variable.veg, Treatment, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = as.character(Plot))

rsq.sum.trt.bb <- VegData.summary.bb.envi %>% 
  group_by(variable.veg, Treatment, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Plot = "All")

rsq.sum.plt.bb <- VegData.summary.bb.envi %>% 
  group_by(variable.veg, Plot, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = as.character(Plot))

rsq.sum.plt.trt.bb <- VegData.summary.bb.envi %>% 
  group_by(variable.veg, variable.envi) %>% 
  summarize(slope = coef(lm(mean.veg ~ value.envi))[[2]],
            r2 = summary(lm(mean.veg ~ value.envi))$r.squared) %>%
  ungroup() %>%
  mutate(sig = (r2) > 0.2, 
         Treatment = "All", 
         Plot = "All")

rsq.sum.bb <- bind_rows(rsq.sum.all.bb, rsq.sum.plt.bb, rsq.sum.trt.bb, rsq.sum.plt.trt.bb) %>% 
  mutate(veg.method  = ifelse(variable.veg %in% spplist$pctcover, "BB", "count"))

rsq.sum.table <- bind_rows(rsq.sum.bb, rsq.sum.mid) %>% 
  select(variable.veg, veg.method, everything())

write.csv(
  rsq.sum.table, 
  file = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/Veg and Environmental R Squared Table with site means.csv"),
  row.names = F
)
