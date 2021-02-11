# rm(list = ls())

library(tidyverse)
library(reshape2)
library(lubridate)
library(readr)

#create color-blind pallette for plotting
cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))

## create a df to store the environmental variables 
# start with the date of the first and last survey each year
envidata <- VegData %>% 
  group_by(Year, Site, SampTime) %>% 
  summarize(firstsurvey = range(ymd(paste(Year, Month, Day, sep = "-")))[1],
            lastsurvey = range(ymd(paste(Year, Month, Day, sep = "-")))[2]) %>% 
  ungroup()

# read in the meterological data from the airport in bft
MetData.bft <- read_excel(paste0(getwd(), "/Data/RC_Shoreline_Environmental_MSC.xlsx"), sheet = "BFT MJS Field") %>% 
  mutate(DATE = as.Date(DATE),
         YEAR = year(DATE), 
         MONTH = month(DATE)) %>% 
  select(-"STATION", -"NAME")

# create a df for just the precip
rainfall <- MetData.bft %>% 
  melt(id.vars = "DATE") %>% 
  filter(variable == "PRCP") %>% 
  mutate(YEAR = year(DATE), 
         MONTH = month(DATE)) %>% 
  select(DATE, YEAR, MONTH, RAIN.mm = value)

#### calculate the growing season envi variables for each site and survey date ####
## growing season (GS) = april 1st - survey date
# total GS precipitation
envidata$Precip.GS.mm <- sapply(X = envidata$firstsurvey, FUN = function(date){
  sum(rainfall$RAIN.mm[rainfall$DATE >= ymd(paste(year(date), 04, 01, sep = "-")) &
                         rainfall$DATE <= date], na.rm = T)
})

# average GS avg, min, max temperature
for (var in c("TAVG", "TMAX", "TMIN")) {
  envidata[,paste0(var, ".GS.C")] <- 
    sapply(
      X = envidata$firstsurvey, 
      FUN = function(date){
        temp <- MetData.bft %>% 
          melt(id.vars = "DATE") %>% 
          filter(variable == var,
                 DATE >= ymd(paste(year(date), 04, 01, sep = "-")),
                 DATE <= date)
        
        mean(temp$value, na.rm = T) 
      }
    )
}

### Read in the Palmer drought severity index data
PDSI <- read_excel(paste0(getwd(), "/Data/RC_Shoreline_Environmental_MSC.xlsx"), sheet = "PDSI") %>% 
  mutate(Date = as.Date(Date))

## Join PDSI data and survey dates by closest date
# generate date differences between each survey date and each psdi date
temp <- outer(envidata$firstsurvey, PDSI$Date, "-")

# remove where PDSI Date is after firstsurvey
temp[temp < 0] <- NA

# find index of minimum date difference
ind <- apply(temp, 1, function(i) which.min(i))

# match index of minimum to the Survey Dates
envidata <- cbind(envidata,  PDSI[ind,"PDSI"]) 

# clean up
rm(temp, ind)

#### inundation ####
source(paste0(getwd(), "/helper scripts/CalcDatums.R"))
source(paste0(getwd(), "/helper scripts/inundationTime.R"))

bftWL <- list()
for (yr in 2006:2020) {
  bftWL[[paste(yr)]] <- read.csv(paste0(getwd(), "/Data/BFT DUML Water levels/BFTWL ", yr,".csv"), stringsAsFactors = F) %>% 
    mutate(DateTime = ymd_hms(DateTime))
}

i = 1
startMSL = -0.1375083 
for(i in 1:length(envidata$firstsurvey)) {#}
  date <- as.character(envidata$firstsurvey[i])
  
  d <- calcDatums(filter(bftWL[[as.character(unique(year(date)))]]))
  envidata[i, names(d)] <- d
  
  d <-
    calcDatums(filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date))
  envidata[i, paste0(names(d), ".YTD")] <- d
  
  d <-
    calcDatums(filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date, DateTime >= mdy(paste0(
      "04-01-", as.character(unique(year(date)))
    ))))
  envidata[i, paste0(names(d), ".GS")] <- d
  
  envidata[i, "t"] <-
    as.numeric(inundationTime(
      WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date),
      in.elev = min(filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date)$WL) -
        1
    ), units = "days")
  
  envidata[i, "t.YTD"] <-
    as.numeric(ymd(date) - ymd(paste0(year(date), "0101")), units = "days")
  
  envidata[i, "t.GS"] <-
    as.numeric(ymd(date) - ymd(paste0(year(date), "0401")), units = "days")
  
  for (dat in names(d)) {
    #envidata[i, paste0(dat, "_IT.mins")] <- inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date),
    #                                                                   in.elev = as.numeric(d[dat]))
    #envidata[i, paste0(dat, "_IT.pct")] <- envidata[i, paste0(dat, "_IT.mins")] / envidata[i, "t"]
    
    #IT <- inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date),
    #                                                              in.elev = as.numeric(d[dat]))
    #envidata[i, paste0(dat, "_IT.pct")] <- IT / envidata[i, "t"]
  }
  envidata[i, paste0("IT.pct", seq(-.5, .5, by = 0.25))] <-
    sapply(
      seq(-.5, .5, by = 0.25),
      FUN = function(L) {
        inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date),
                       in.elev = L) / envidata[i, "t"]
      }
    )
  
  envidata[i, paste0("GS.IT.pct", seq(-.5, .5, by = 0.25))] <-
    sapply(
      seq(-.5, .5, by = 0.25),
      FUN = function(L) {
        inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date, DateTime >= mdy(paste0(
          "0401", as.character(unique(year(date)))
        ))),
        in.elev = L) / (envidata[i, "t.GS"] * 24 * 60)
      }
    )
  envidata[i, "startMSL_IT.pct"] <-
    inundationTime(WLdf = filter(bftWL[[as.character(unique(year(date)))]], DateTime <= date),
                   in.elev = startMSL) / envidata[i, "t"]
}

#x <- lapply(names(envidata), function(n){write_csv(envidata[[n]], paste0("R:/CEE/RC shoreline/Data/Meterological Data/survey dates and environmental variables ",n,".csv"))})
#rm(x)

write_csv(envidata, paste0(getwd(), "/Data/R output/survey dates and environmental variables all.csv"))

envidata.melt <- envidata %>% 
  melt(., id.vars = c("Year", "Site", "SampTime", "firstsurvey", "lastsurvey"))

# unique(envidata.melt$variable)

envidata.melt.p <- filter(envidata.melt, 
                          Site == "PI",
                          grepl(x = SampTime, pattern = "SU"), 
                          variable %in% c("Precip.GS.mm", "TMAX.GS.C",
                                          "TMIN.GS.C", "PDSI",
                                          "MSL.GS", "t.GS",
                                          "GS.IT.pct0")) %>% 
  droplevels.data.frame() %>% 
  mutate(variable = as.character(variable),
         variable = factor(variable),
         variable = fct_recode(variable,
                               "Total GS Precipitation (mm)"= "Precip.GS.mm",
                               "Mean Daily Max Temp (C)" = "TMAX.GS.C",
                               "Mean Daily Min Temp (C)" = "TMIN.GS.C",
                               "Palmer Drought Severity Index" = "PDSI",
                               "GS Mean Sea Level (m NAVD)" = "MSL.GS",
                               "GS Inundation at 0m NAVD (% time)" = "GS.IT.pct0",
                               "Growing season Length (days)" = "t.GS"),
         variable = fct_relevel(variable,
                                "Total GS Precipitation (mm)",
                                "Mean Daily Max Temp (C)",
                                "Mean Daily Min Temp (C)",
                                "Palmer Drought Severity Index",
                                "GS Mean Sea Level (m NAVD)",
                                "GS Inundation at 0m NAVD (% time)",
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
  filename = paste0(getwd(), "/Figures/Environmental/PI Environmental data.png"),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

rsq <- filter(envidata, grepl(x = SampTime, pattern = "SU")) %>% 
  summarize(m = round(summary(lm(Precip.GS.mm ~ t.GS))$coefficients[2], 3),
            b = round(summary(lm(Precip.GS.mm ~ t.GS))$coefficients[1], 2),
            r2 = round(summary(lm(Precip.GS.mm ~ t.GS))$r.squared, 3)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2)

ggplot(filter(envidata, Site %in% c('PI', 'PKS', 'NCMM'), grepl(x = SampTime, pattern = "SU")), aes(t.GS, Precip.GS.mm))+
  geom_smooth(method = "lm", se = F, color = "black")+
  geom_point(aes(color = as.factor(Year), shape = Site), size = 3)+
  geom_text(data = rsq, aes(x = 90, y = 550, label = paste0("~r^2 ==", r2)), 
            color = "black", size = 4.5, parse = T, hjust = 0, vjust = 1, show.legend = F)+
  labs(x = "Growing Season Duration (days)", 
       y = "Growing Season Total Precipitation (mm)", 
       color = "Year", 
       title = "Growing Season Length and Total Precipitation")
ggsave(filename = paste0(getwd(), "/Figures/Environmental/Growing Season Length and Precip.png"),
       width = 9, height = 6, units = "in", dpi = 300)

#### clean up the global environment ####
rm(d)
rm(surveyDates, VegData.summary.trt, VegData.summary.trt.ftr, VegData0607, VegData0607.melt, VegData0607.summary, VegData0607.summary.trt, VegData0607.summary.trt.ftr)
rm(comCols, ct, dat, date, i, names, nms, PctCoverColNums, QuantColNums, run, SppColNums, startMSL, var, yr)

####Veg Data comparison with Environmental data####
envidata.summary <- envidata.melt.p %>%  
  filter(Site %in% c("PI", "PKS", "NCMM")) %>% 
  group_by(Year, SampTime, variable) %>% 
  summarise(value = mean(value, na.rm = F)) %>% 
  ungroup() 

VegData.envidata.summary <- VegData.summary %>% 
  left_join(envidata.summary, by = c("SampTime"),  suffix = c(".veg", ".envi"))

if(!exists("spplist")) {
  spplist <- list()
} else{
  if (!is_list(spplist)) {
    spplist <- list()
  }
}
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$pctcover.plotting <- c("Salt", "Spat", "Sspp", "oyster_live", "oyster_culch", "mussel")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$quant.plotting <- c("liveStem_m2", "HMean", "snails_m2", "BMtotal")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$rare.plotting <- c("Bfru", "Dspi", "Hspp", "Ifru", "Jroe", "Lcar", "Malb", "Slin", "Srob", "Ssem", "Vlut", "macroalgae")#### Rsquared Table ####

tempdf <- filter(VegData.envidata.summary,
                 grepl(x = SampTime, pattern = "SU"),
                 Plot <= 20, 
                 #Site == "PI",
                 variable.veg == "liveStem_m2",
                 variable.envi == "Growing season Length (days)") %>% 
  droplevels.data.frame()

rsq.all <- VegData.envidata.summary %>% 
  filter(Plot<=20,
         !is.na(variable.envi),
         variable.veg %in% c(spplist$quant.plotting, "Salt")) %>% 
  group_by(Plot, Treatment, variable.veg, variable.envi) %>% 
  summarize(m = round(summary(lm(mean ~ value))$coefficients[2], 3),
            #b = round(summary(lm(mean ~ value))$coefficients[1], 2),
            r2 = round(summary(lm(mean ~ value))$r.squared, 3)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2) 


for (i in 1:length(rsq.all$variable.veg)) {
  rsq.all$variable.veg.name[i] <- paste(axis.label(as.character(rsq.all$variable.veg[i])), unitname(as.character(rsq.all$variable.veg[i])))
}

ggplot(rsq.all, aes(variable.veg.name, variable.envi))+
  geom_text(aes(label = paste0(signif(round(r2, 2), 2), " (", ifelse(sign(m)==-1, "-", "+"), ")"), color = sig), show.legend = F)+
  facet_grid(Plot~Treatment)+
  scale_color_manual(values = c("black", 'red'))+
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45/2, hjust = 1))+
  labs(x = NULL, y = NULL)

ggsave(filename = paste0(getwd(), "/Figures/Environmental/All Environmental Data Rsq Table .png"),
       width = 16, height = 9, units = "in", dpi = 300)

############

tempdf <- filter(VegData.envidata.summary,
                 grepl(x = SampTime, pattern = "SU"),
                 Plot <= 20, 
                 #Site == "PI",
                 variable.veg == "liveStem_m2",
                 variable.envi == "Growing season Length (days)") %>% 
  droplevels.data.frame()

rsq <- tempdf %>% 
  group_by(Plot, Treatment) %>% 
  summarize(m = round(summary(lm(mean ~ value))$coefficients[2], 3),
            b = round(summary(lm(mean ~ value))$coefficients[1], 2),
            r2 = round(summary(lm(mean ~ value))$r.squared, 3),
            x = min(value, na.rm = T),
            y = mean(mean, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sig = (r2)>0.2)

ggplot(tempdf,
       aes(value, mean, color = Treatment, group = Treatment))+
  geom_smooth(method = "lm", se = F)+
  geom_point(aes(fill = as.factor(Year), shape = Treatment), size = 3, color = "transparent")+
  facet_wrap(~Plot, scales = "free")+
  geom_text(data = rsq, aes(x = x, y = y, label = paste0("~r^2 ==", r2)), 
            size = 4.5, parse = T, hjust = 0, vjust = 1, show.legend = F)+
  # scale_color_manual(values = c(dichromat::colorschemes$Categorical.12, (gray.colors(3)[2:1])))+
  scale_shape_manual(values = c(21, 24))+
  labs(title = "Growing Season length and S. alterniflora Density", 
       y = "Mean Stem Density",
       x = "Growing Season Duration (days)")
# ggsave(filename = paste0(getwd(), "/Figures/Environmental/Growing Season Length and S. alt density.png"),
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
ggsave(filename = paste0(getwd(), "/Figures/Environmental/Growing Season Length and S. alt height.png"),
       width = 9, height = 6, units = "in", dpi = 300)


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
