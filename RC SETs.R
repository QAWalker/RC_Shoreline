# rm(list = ls())
library(tidyverse)
library(lubridate)
library(reshape2)
capitalize <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

theme_set(theme_bw())

SETids <- read.csv(paste0(getwd(), "/RC_SET_id.csv"), stringsAsFactors = F, strip.white = T) %>% 
  rename(SET = SET.number, Location = Relative.Location, VRP.elev = Orthometric.Elevation.NAVD88.G_12B..m.) %>% 
  select(-"DATE.of.last.OPUS.or.Laser.leveling", -"Technique", -"Reference.point")

SET <- read.csv(paste0(getwd(), "/RC_SET_Data_Master_updated.csv"), stringsAsFactors = F, strip.white = T) %>% 
  mutate(Date = mdy(Date)) %>% 
  rename(SET = SetNumber, Elev = SurfaceElevation.mNAVD88) %>% 
  left_join(SETids) %>% 
  select(names(SETids), everything(), -"SasDate") %>% 
  # filter(Site %in% c("PKS", "PI", "NCMM"), Marsh.Type == "Fringing", 
  #        !(SET==1&Date==ymd("2004-09-03")),
  #        !(SET==10&Date==ymd("2004-10-22"))) %>%
  arrange(Date) %>% 
  group_by(SET, SETArmPosition, PinNum) %>% 
  mutate(d.Elev = Elev - Elev[1], first.date = Date[1], 
         dec.date = decimal_date(Date)) %>% 
  ungroup() 

SET.date <- SET %>% 
  arrange(Date) %>% 
  group_by(SET, SETArmPosition) %>% 
  summarize(first.date = Date[1],
            second.date = unique(Date)[2],
            readings.total = length(unique(Date)),
            readings.2004 = sum(year(unique(Date))==2004),
            readings.2005 = sum(year(unique(Date))==2005),
            readings.2006 = sum(year(unique(Date))==2006),
            readings.2007 = sum(year(unique(Date))==2007),
            readings.2008 = sum(year(unique(Date))==2008),
            readings.2009 = sum(year(unique(Date))==2009),
            readings.2010 = sum(year(unique(Date))==2010),
            readings.2011 = sum(year(unique(Date))==2011),
            readings.2012 = sum(year(unique(Date))==2012),
            readings.2013 = sum(year(unique(Date))==2013),
            readings.2014 = sum(year(unique(Date))==2014),
            readings.2015 = sum(year(unique(Date))==2015),
            readings.2016 = sum(year(unique(Date))==2016),
            readings.2017 = sum(year(unique(Date))==2017),
            readings.2018 = sum(year(unique(Date))==2018),
            readings.2019 = sum(year(unique(Date))==2019)) %>% 
  ungroup() %>% 
  left_join(SETids) 

# ggplot(filter(SET, Date <= ymd("2006-01-01")), aes((Date), d.Elev*100, color = as.factor(SETArmPosition))) +
#   geom_line(aes(group = paste(SET, SETArmPosition, PinNum))) +
#   geom_point() +
#   facet_wrap(SET~., 
#              scales = "free_y", 
#              labeller = "label_both") +
#   labs(x = NULL, color = "Arm Position", y = "Change in Elevation (mm)", title = "Change Pin Elevations RC SETS in the first year")
# ggsave("R:/CEE/RC shoreline/Data/SET Data/for R/RC SETs elev first year free scales.png", width = 16, height = 6, units = "in", dpi = 300)

## From Lynch et al. (2015) ##
# Briefly, we calculated the slope of a linear regression of the change in
# measured pin lengths over time for each of the 36 pins.
#
# Slopes were then averaged, first by arm position, and then among arm positons
# to determine the overall slope of elevation change for each device. 
#
# The reported standard deviation reflects the variability of average slope
# values for each arm.
#
# This approach results in larger confidence intervals than would be obtained by
# simply taking the average of all 36 pins (although the slope would be the
# same).  Thus, the confidence intervals reported for these rates are
# conservative.

# summary(lm(Elev~decimal_date(Date), data = SET))$r.squared

slopes.trt <- SET %>% 
  filter(!is.na(SET)) %>% #, Site %in% c("PKS", "PI", "NCMM"), Marsh.Type == "Fringing") %>% 
  group_by(SET, SETArmPosition, PinNum) %>% 
  summarize(slope = summary(lm(d.Elev~decimal_date(Date)))$coefficients[2],
            n.readings = length(unique(Date)),
            first.Date = decimal_date(min(Date, na.rm = T)),
            last.Date = decimal_date(max(Date, na.rm = T))) %>% 
  ungroup() %>% 
  filter(!is.nan(slope)) %>% 
  {. ->> slopes.pins} %>% 
  group_by(SET, SETArmPosition) %>% 
  summarize(mean.slope = mean(slope, na.rm = T),
            sd.slope = sd(slope, na.rm = T),
            unique.pins = n_distinct(PinNum),
            n.readings = sum(n.readings, na.rm = T),
            first.Date = min(first.Date, na.rm = T),
            last.Date = max(last.Date, na.rm = T),
            ttest = list(t.test(slope))) %>% 
  mutate(ttest = map(ttest, broom::tidy)) %>%
  unnest(cols = c(ttest)) %>%
  ungroup() %>% 
  {. ->> slopes.arms} %>% 
  group_by(SET) %>% 
  summarize(mean.mean.slope = mean(mean.slope, na.rm = T),
            sd.mean.slope = sd(mean.slope, na.rm = T),
            unique.armpositions = n_distinct(SETArmPosition),
            unique.pins = sum(unique.pins),
            n.readings = sum(n.readings, na.rm = T),
            first.Date = min(first.Date, na.rm = T),
            last.Date = max(last.Date, na.rm = T),
            ttest = list(t.test(mean.slope))) %>% 
  mutate(ttest = map(ttest, broom::tidy)) %>%
  unnest(cols = c(ttest)) %>%
  ungroup() %>% 
  left_join(SETids) %>% 
  {. ->> slopes.SETs} %>% 
  filter(Treatment %in% c("Natural", "Sill")) %>% 
  group_by(Treatment, Location) %>% 
  summarize(mean.rate = mean(mean.mean.slope, na.rm = T),
            sd.rate = sd(mean.mean.slope, na.rm = T),
            n.SETs = length(SET),
            unique.SETs = n_distinct(SET),
            n.armpositions = sum(unique.armpositions),
            unique.pins = sum(unique.pins),
            n.readings = sum(n.readings),
            first.Date = min(first.Date, na.rm = T),
            last.Date = max(last.Date, na.rm = T),
            ttest = list(t.test(mean.mean.slope))) %>% 
  mutate(ttest = map(ttest, broom::tidy)) %>%
  unnest(cols = c(ttest)) %>% 
  ungroup() %>% 
  mutate(intercept = -1*mean.rate*first.Date,
         se.rate = sd.rate/sqrt(n.SETs),
         CI.95.t = qnorm(0.975, mean = mean.rate, sd = sd.rate),
         CI.95.m = 1.96*se.rate)

summary.SETs <- SET %>% 
  filter(Site %in% c("PKS", "PI", "NCMM"), 
         Marsh.Type == "Fringing") %>% 
  arrange(Date) %>% 
  group_by(SET, SETArmPosition, PinNum) %>% 
  mutate(d.Elev = Elev - first(Elev)) %>% 
  ungroup() %>% 
  group_by(Date, SET, SETArmPosition) %>% 
  summarize(n = length(Elev),
            m.Elev = mean(Elev, na.rm = T),
            sd.Elev = sd(Elev, na.rm = T),
            se.Elev = sd.Elev/sqrt(n),
            ci.Elev = se.Elev*1.96,
            m.d.Elev = mean(d.Elev, na.rm = T),
            sd.d.Elev = sd(d.Elev, na.rm = T),
            se.d.Elev = sd.d.Elev/sqrt(n),
            ci.d.Elev = se.d.Elev*1.96) %>% 
  ungroup() %>% 
  left_join(SETids)%>% 
  {. ->> summary.arms} %>% 
  group_by(Date, SET) %>% 
  summarize(n = length(m.Elev),
            m.m.Elev = mean(m.Elev, na.rm = T),
            sd.m.Elev = sd(m.Elev, na.rm = T),
            se.m.Elev = sd.m.Elev/sqrt(n),
            ci.m.Elev = se.m.Elev*1.96,
            m.m.d.Elev = mean(m.d.Elev, na.rm = T),
            sd.m.d.Elev = sd(m.d.Elev, na.rm = T),
            se.m.d.Elev = sd.m.d.Elev/sqrt(n),
            ci.m.d.Elev = se.m.d.Elev*1.96) %>% 
  ungroup() %>% 
  left_join(SETids) 

write_csv(slopes.pins, paste0(getwd(),"/Pin Slopes.csv"))
write_csv(slopes.arms, paste0(getwd(), "/Arm Slopes.csv"))
write_csv(slopes.SETs, paste0(getwd(), "/SET Slopes.csv"))

ggplot(summary.arms, aes(Date, m.d.Elev, color = SETArmPosition))+
  geom_line()+
  geom_errorbar(aes(min = m.d.Elev - ci.d.Elev, max = m.d.Elev + ci.d.Elev), width = 90)+
  geom_point(size = 3)+
  facet_grid(Site ~ Treatment + Location)+
  labs(x = NULL,
       y = "Mean Elevation Change +/- 95% CI (m)",
       color = "Arm Position", 
       title = "RC SETS Arm Position Mean Elevation Change")
ggsave(paste0(getwd(), "/Figures/SETs/RC SETs arm means.png"), width = 16, height = 9, units = "in", dpi = 300)

ggplot(summary.SETs, aes(decimal_date(Date), m.m.d.Elev, color = as.factor(Site), group = SET))+
  geom_line()+
  geom_errorbar(aes(min = m.m.d.Elev - ci.m.d.Elev, max = m.m.d.Elev + ci.m.d.Elev), width = .2, show.legend = F)+
  geom_point(size = 3)+
  # geom_abline(data = slopes.trt, aes(slope = mean.rate, intercept = intercept, linetype = "mean"), show.legend = F, size = .75, inherit.aes = F, color = "black")+
  # geom_segment(data = slopes.SETs, aes(x = first.Date, xend = last.Date, y = 0, yend = (last.Date-first.Date)*mean.mean.slope, linetype = "mean"))+
  # geom_segment(data = slopes.SETs, aes(x = first.Date, xend = last.Date, y = 0, yend = (last.Date-first.Date)*(mean.mean.slope+sd.mean.slope),  linetype = "+1 sd"), size = .75)+
  # geom_segment(data = slopes.SETs, aes(x = first.Date, xend = last.Date, y = 0, yend = (last.Date-first.Date)*(mean.mean.slope-sd.mean.slope),  linetype = "-1 sd"), size = .75)+
  # scale_linetype_manual(values = c(3, 2, 1))+
  facet_grid(Treatment ~ Location)+
  # scale_color_manual(values = gray.colors(4))+
  labs(x = NULL,
       y = "Mean Elevation Change +/- 95% CI (m)",
       color = "Site", 
       title = "RC SETS Site Mean Elevation Change")
ggsave(paste0(getwd(), "/Figures/SETs/RC SETs means.png"), width = 8, height = 6, units = "in", dpi = 300)

date.r <- range(summary.SETs$Date, na.rm = T)
size <- 80
newdf <- data.frame(Date = seq(date.r[1], date.r[2], length.out = size)) %>% 
  mutate(d.Date = decimal_date(Date))

if(exists("errordf")) {
  rm(errordf)
}
for (trt in c("Natural", "Sill")) {
  for(loc in c("upper", "lower")){
    tempdf <- filter(summary.SETs, Treatment == trt, Location == loc) %>% 
      mutate(d.Date = decimal_date(Date))
    slopes.temp <- filter(slopes.trt, Treatment == trt, Location == loc)
    model <- lm(m.m.d.Elev ~ d.Date, data = tempdf)
    modelFit <- data.frame(Treatment = trt, 
                           Location = loc,
                           Date = newdf$Date, d.Date = newdf$d.Date, 
                           predict(model, se = T, newdata = newdf)) %>% 
      mutate(upperBound = (slopes.temp$mean.rate * d.Date) + slopes.temp$intercept + 1.96 * se.fit,
             lowerBound = (slopes.temp$mean.rate * d.Date) + slopes.temp$intercept - 1.96 * se.fit)
    
    if(!exists("errordf")) {
      errordf <- modelFit
    } else{
      errordf <- rbind(errordf, modelFit)
    }
  }
}

ggplot(summary.SETs, aes(decimal_date(Date), m.m.d.Elev)) +
  geom_ribbon(data = errordf, inherit.aes = F,
              aes(x = d.Date,
                  ymin = lowerBound,
                  ymax = upperBound,
                  group = paste(Treatment, Location)),
              fill = "lightgray", alpha = 0.5) +
  geom_segment(data = slopes.trt,
               aes(x = first.Date, 
                   xend = last.Date, 
                   y = 0, 
                   yend = mean.rate*last.Date + intercept,
                   color = Treatment,
                   linetype = Location),
               show.legend = T,
               size = 1,
               inherit.aes = F) +
  geom_line(aes(color = Treatment, group = SET, linetype = Location), show.legend = F) +
  # scale_color_manual(values = gray.colors(3)) +
  # facet_grid(Treatment~Location)+
  #scale_fill_manual(values = gray.colors(4)) +
  geom_point(aes(decimal_date(Date), m.m.d.Elev, color = Treatment, shape = Site))+
  labs(color = "Treatment", x = NULL, y = "Elevation Change (m)")+
  guides(shape = guide_legend(override.aes = list(size = 3, linetype = NULL)),
         color = guide_legend(override.aes = list(size = 3, linetype = NULL)))
ggsave(paste0(getwd(), "/Figures/SETs/RC SETs means with mean rate and mean CI no facets.png"), width = 8, height = 6, units = "in", dpi = 300)

ggplot(filter(summary.SETs, Treatment == "Natural", Location == "lower"), aes(decimal_date(Date), m.m.d.Elev)) +
  geom_ribbon(data = filter(errordf, Treatment == "Natural", Location == "lower"),
              inherit.aes = F,
              aes(x = d.Date,
                  ymin = lowerBound,
                  ymax = upperBound,
                  group = paste(Treatment, Location)),
              fill = "lightgray", alpha = 0.5) +
  geom_segment(data = filter(slopes.trt, Treatment == "Natural", Location == "lower"),
               aes(x = first.Date, 
                   xend = last.Date, 
                   y = 0, 
                   yend = mean.rate*last.Date + intercept,
                   color = Treatment,
                   linetype = Location),
               show.legend = T,
               size = 1,
               inherit.aes = F) +
  geom_line(aes(color = Treatment, group = SET, linetype = Location), show.legend = F) +
  # scale_color_manual(values = gray.colors(3)) +
  geom_point(aes(decimal_date(Date), m.m.d.Elev, color = Treatment, shape = Site))+
  geom_smooth(method = "lm", aes(group = Site))+
  labs(color = "Treatment", x = NULL, y = "Elevation Change (m)")+
  guides(shape = guide_legend(override.aes = list(size = 3, linetype = NULL)),
         color = guide_legend(override.aes = list(size = 3, linetype = NULL)))
ggsave(paste0(getwd(), "/Figures/SETs/RC SETs means with mean rate and mean CI no facets.png"), width = 8, height = 6, units = "in", dpi = 300)

ggplot(summary.arms, aes(Date, m.d.Elev, color = SETArmPosition))+
  geom_line()+
  geom_line(data = summary.SETs, aes(Date, m.m.d.Elev, color = "SET Mean"))+
  geom_errorbar(inherit.aes = F, data = summary.SETs, aes(x = Date,
                                         min = m.m.d.Elev - ci.m.d.Elev, max = m.m.d.Elev + ci.m.d.Elev,
                                         color = "SET Mean"),
                width = 90)+
  geom_errorbar(aes(min = m.d.Elev - ci.d.Elev, max = m.d.Elev + ci.d.Elev), width = 90)+
  geom_point(data = summary.SETs, aes(Date, m.m.d.Elev, color = "SET Mean"),
             size = 2, alpha = 1)+
  geom_point(size = 2)+
  facet_grid(Site ~ Treatment + Location)+
  # scale_color_manual(values = c(viridisLite::viridis(9)[2:9], "black"))+
  labs(x = NULL,
       y = "Mean Elevation Change +/- 95% CI (m)",
       color = "Arm Position", 
       title = "RC SETS Arm Position Mean Elevation Change")
ggsave(paste0(getwd(), "/Figures/SETs/RC SETs arm and means.png"), width = 16, height = 9, units = "in", dpi = 300)

ggplot(summary.SETs, aes(decimal_date(Date), m.m.Elev))+
  geom_line(aes(color = Treatment, linetype = Location, group = SET), show.legend = T)+
  geom_errorbar(aes(min = m.m.Elev - se.m.Elev, max = m.m.Elev + se.m.Elev, color = Treatment), show.legend = F, width = .08) +
  geom_point(size = 3, aes(color = Treatment))+
  # scale_color_manual(values = c("darkgray", "black"))+
  labs(x = "Date", y = "Surface Elevation (m NAVD88 G-12B)", title = "Surface Elevation Measured by SETs")
ggsave(filename = paste0(getwd(), "/Figures/SETs/Mean Elevation RC SETs.png"), width = 9, height = 6, units = "in", dpi = 330)

ggplot(summary.SETs, aes(decimal_date(Date), m.m.d.Elev))+
  geom_line(aes(color = Treatment, linetype = Location, group = SET), show.legend = T)+
  geom_errorbar(aes(min = m.m.d.Elev - se.m.d.Elev, max = m.m.d.Elev + se.m.d.Elev, color = Treatment), show.legend = F, width = .08) +
  geom_point(size = 3, aes(color = Treatment))+
  # scale_color_manual(values = c("darkgray", "black"))+
  labs(x = "Date", y = "Surface Elevation Change (m)", title = "Change in Surface Elevation Measured by SETs") 
ggsave(filename = paste0(getwd(), "/Figures/SETs/Mean Elevation Change RC SETs.png"), width = 9, height = 6, units = "in", dpi = 330)

p <- ggplot(summary.SETs, aes(decimal_date(Date), m.m.d.Elev))+
  geom_errorbar(aes(min = m.m.d.Elev - se.m.d.Elev, max = m.m.d.Elev + se.m.d.Elev, color = Treatment), show.legend = F, width = .08) +
  geom_point(size = 3, aes(color = Treatment))

# ggsave(p, filename = "R:/CEE/RC shoreline/Data/SET Data/for R/SETs with average regression.png", width = 9, height = 6, units = "in", dpi = 330)
p.x <- layer_scales(p)$x$get_limits()
p.y <- layer_scales(p)$y$get_limits()

CIpolys <- slopes.trt %>% 
  mutate(x1 = p.x[1]-(p.x[2] - p.x[1]),
         x2 = p.x[2]+(p.x[2] - p.x[1]),
         y1 = mean.rate*x1 + intercept,
         y2= mean.rate*x2 + intercept, 
         xminu = x1, xminl = x1,
         xmaxu = x2, xmaxl = x2,
         yminl = y1 - CI.95.m, yminu = y1 + CI.95.m, 
         ymaxl = y2 + CI.95.m, ymaxu = y2 - CI.95.m) #%>% 

CIpolys <- data.frame(x = unlist(CIpolys[,names(CIpolys)[startsWith(names(CIpolys), prefix = "xm")]], use.names = F),
                 y = unlist(CIpolys[,names(CIpolys)[startsWith(names(CIpolys), prefix = "ym")]], use.names = F), 
                 group = paste(CIpolys$Treatment, CIpolys$Location), 
                 Location = CIpolys$Location,
                 Treatment = CIpolys$Treatment)

CIpolys.seg <- slopes.trt %>% 
  mutate(x1 = first.Date,
         x2 = last.Date,
         y1 = mean.rate*x1 + intercept,
         y2= mean.rate*x2 + intercept, 
         xminu = x1, xminl = x1,
         xmaxu = x2, xmaxl = x2,
         yminl = y1 - CI.95.m, yminu = y1 + CI.95.m, 
         ymaxl = y2 + CI.95.m, ymaxu = y2 - CI.95.m) #%>% 

CIpolys.seg <- data.frame(x = unlist(CIpolys.seg[,names(CIpolys.seg)[startsWith(names(CIpolys.seg), prefix = "xm")]], use.names = F),
                      y = unlist(CIpolys.seg[,names(CIpolys.seg)[startsWith(names(CIpolys.seg), prefix = "ym")]], use.names = F), 
                      group = paste(CIpolys.seg$Treatment, CIpolys.seg$Location), 
                      Location = CIpolys.seg$Location,
                      Treatment = CIpolys.seg$Treatment)

ggplot(summary.SETs, aes(decimal_date(Date), m.m.d.Elev))+
  #geom_polygon(data = CIpolys, aes(x = x, y = y, group = group),fill = "gray", alpha = 0.25)+
  #geom_abline(data = slopes.trt, aes(shape = NULL, slope = mean.rate, intercept = intercept, color = Treatment, linetype = Location), show.legend = F, size = 1.5)+
  geom_polygon(data = CIpolys.seg, aes(x = x, y = y, group = group), fill = "gray", alpha = 0.25)+
  geom_segment(data = slopes.trt, aes(y = 0, yend = mean.rate*last.Date+intercept, x = first.Date, xend = last.Date, color = Treatment, linetype = Location), inherit.aes = F, size = 1)+
  #geom_line(aes(color = Treatment, linetype = Location, group = SET), show.legend = T)+
  #geom_errorbar(aes(min = m.m.d.Elev - se.m.d.Elev, max = m.m.d.Elev + se.m.d.Elev, color = Treatment), show.legend = F, width = .08) +
  #geom_point(size = 3, aes(color = Treatment))+
  #scale_fill_manual(values = c("white", "black"))+
  scale_color_manual(values = c("darkgray", "black"))+
  coord_cartesian(xlim = p.x, ylim = p.y)+
  labs(x = "Date", y = "Surface Elevation Change (m)", title = "Change in Surface Elevation Measured by SETs")

ggplot(summary.SETs, aes(decimal_date(Date), Elev))+
  geom_polygon(data = df, aes(x = x, y = y, group = group, fill = Treatment), alpha = 0.25)+
  geom_abline(data = slopes.trt, aes(slope = mean.slope, intercept = mean.intercept - (sd.intercept)))+
  geom_abline(data = slopes.trt, 
              aes(shape = NULL, slope = mean.slope, intercept = mean.intercept, color = Treatment, linetype = Location), 
              size = 1.25, show.legend = F)+
  geom_line(aes(group = SET, linetype = Location))+
  geom_point(size = 3, aes(fill = Treatment), shape = 21)+
  scale_fill_manual(values = c("darkgray", "black"))+
  scale_color_manual(values = c("white", "black"))+
  scale_linetype_manual(values = c(1,2))+
  coord_cartesian(xlim = p.x, ylim = p.y)+
  labs(x = "Date", y = "Surface Elevation (m NAVD88 G-12B)", title = "Surface Elevation Measured by SETs")
  

ggplot(SET.summary, aes(decimal_date(Date), d.Elev, color = Treatment, linetype = Location))+
  # geom_segment(data = slopes.trt,
  #              aes(x = first.Date, y = 0, xend = 2020, yend = mean.slope * 2020 + intercept),
  #              size = 1.5, show.legend = F)+
  # # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0+CI.95, xend = 2020, yend = mean.slope * 2020 + intercept+CI.95), size = .5, show.legend = F)+
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0-CI.95, xend = 2020, yend = mean.slope * 2020 + intercept-CI.95), size = .5, show.legend = F)+
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0+qn, xend = 2020, yend = mean.slope * 2020 + intercept+qn), size = .5, show.legend = F)+
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0-qn, xend = 2020, yend = mean.slope * 2020 + intercept-qn), size = .5, show.legend = F)+
  # # 
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0+qn, xend = 2020, yend = mean.slope * 2020 + intercept+qn), size = .5, show.legend = F)+
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0-qn, xend = 2020, yend = mean.slope * 2020 + intercept-qn), size = .5, show.legend = F)+
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0, xend = 2020, yend = (mean.slope+CI.95) * 2020 + intercept), size = .5, show.legend = F)+
  # geom_segment(data = slopes.trt, aes(x = first.Date, y = 0, xend = 2020, yend = (mean.slope-CI.95) * 2020 + intercept), size = .5, show.legend = F)+
  # geom_abline(data = slopes.trt, 
  #             aes(slope = mean.slope, intercept = intercept, linetype = Location, color = Treatment), 
  #             size = 1, show.legend = F)+
  # facet_grid(Treatment~Location)+
  geom_line(aes(group = SET))+
  #geom_errorbar(aes(min = d.Elev - se, max = d.Elev + se), width = 0.1, linetype = 1, color = "black")+
  geom_point(size = 2.5, aes(fill = Treatment, shape = Site), color = "black")+
  #geom_point(size = 3, aes(x = decimal_date(Date), y = mean(d.Elev), group = paste(year(Date), month(Date), Treatment), color = Treatment), inherit.aes = F, shape = 21, fill = "white")+
  scale_color_manual(values = c("darkgray", "black"))+
  scale_fill_manual(values = c("darkgray", "black"))+
  scale_shape_manual(values = c(21, 22, 24))

jmpdata <- read.csv("R:/CEE/RC shoreline/Data/SET Data/for R/JMP output for quin.csv") %>% 
  left_join(SETids, by = c("Set" = "SET")) %>% 
  mutate(Date = as.Date(SAS.Date, origin="1960-01-01")) %>% 
  filter(Site %in% c('PI', 'NCMM', 'PKS'), Marsh.Type =="Fringing", Date > ymd("2004-11-01")) %>%
  rename("SET" = "Set", "Elev" = "elevation") %>% 
  select("Date",names(SETids), everything(), -"SAS.Date") %>% 
  arrange(Date) %>% 
  group_by(SET) %>% 
  mutate(d.Elev = Elev - first(Elev), 
         d.Date = Date - first(Date))

rdata <- select(summary.SETs, "Date", names(SETids), "Elev" = "m.m.Elev") %>% 
  arrange(Date) %>% 
  group_by(SET) %>% 
  mutate(d.Elev = Elev - first(Elev), 
         d.Date = Date - first(Date))

####
ggplot(jmpdata, aes(Date, Elev, color = as.factor(SET)))+
  geom_line(aes(group = SET))+
  geom_point()+
  labs(title = "jmpdata")

ggplot(rdata, aes(Date, Elev, color = as.factor(SET)))+
  geom_line(aes(group = SET))+
  geom_point()+
  labs(title = "rdata")
#######
ggplot(jmpdata, aes(Date, d.Elev, color = as.factor(SET)))+
  geom_line(aes(group = SET))+
  geom_point()+
  labs(title = "jmpdata")

ggplot(rdata, aes(Date, d.Elev, color = as.factor(SET)))+
  geom_line(aes(group = SET))+
  geom_point()+
  labs(title = "rdata")

rdata <- rdata[order(rdata$SET, rdata$Date),]
jmpdata <- jmpdata[order(jmpdata$SET, jmpdata$Date),]

which(jmpdata$Elev==rdata$Elev)

diff <-
  data.frame(
    Date = jmpdata$Date,
    SET = jmpdata$SET,
    jmp = jmpdata$Elev,
    r = rdata$Elev,
    diff = round(jmpdata$Elev - rdata$Elev, 4), 
    absdiff = abs(round(jmpdata$Elev - rdata$Elev, 4))
  ) %>%
  left_join(SETids) %>%
  select("Date", names(SETids), everything())

jmpanalysis <- read_excel("R:/CEE/SET Manuscript/Set analysis Summary.xlsx", sheet = "full record non-treatment SETs", n_max = 32) %>% 
  select(2:13) %>% 
  as.tbl() %>% 
  filter(!is.na(Site))
names(jmpanalysis) <- c("Site", "SET", names(jmpanalysis)[3:12])

p <- ggplot(summary.SETs, aes(decimal_date(Date), m.m.d.Elev))+
  geom_line(aes(group = SET, color = Treatment, linetype = Location), show.legend = F) +
  geom_point(size = 3, aes(color = Treatment))+
  geom_point(aes(color = Treatment))+
  geom_polygon(data = CIpolys.seg, aes(x = x, y = y, group = group), fill = "gray", alpha = 0.5)+
  geom_segment(data = slopes.trt, aes(y = 0, yend = mean.rate*last.Date+intercept, x = first.Date, xend = last.Date, color = Treatment, linetype = Location), inherit.aes = F, size = 1)+
  scale_color_manual(values = c("darkgray", "black"))+
  coord_cartesian(xlim = p.x, ylim = p.y)+
  labs(x = "Date", y = "Surface Elevation Change (m)", title = "Change in Surface Elevation Measured by SETs")

pct.x <- 0.001
pct.y <- 0.97

p.x <- layer_scales(p)$x$get_limits()
p.y <- layer_scales(p)$y$get_limits()

x.l <- (p.x[2]- p.x[1]) * pct.x + p.x[1]
x.r <- (p.x[2]- p.x[1]) * (pct.x + 0.5) + p.x[1]
y.u <- (p.y[2]- p.y[1]) * pct.y + p.y[1]
y.l <- (p.y[2]- p.y[1]) * (pct.y - 0.2) + p.y[1]
y.g <- (diff(seq(y.u, y.l, length.out = 5))[1])
x.g <- (diff(seq(x.l, x.r, length.out = 6))[1])

q <- p + 
  annotate(geom = "text", 
           x = rep(c(x.l, x.l+2*x.g,x.l+3.5*x.g), each = 5), y = rep(seq(y.u, y.l, by = y.g), times = 3),
           label = c(c("Treatment",paste0(slopes.trt$Treatment, "-",slopes.trt$Location)),
                     c("Rate (mm/yr)",paste(round(slopes.trt$mean.rate*1000, 2))),
                     c("95% CI (mm/yr)",paste(round(slopes.trt$CI.95.m*1000, 2)))),
           hjust = 0, vjust = 1)+
  #vseg
  annotate(geom = "segment",
         x = x.l + 1.85 * abs(x.g),
         xend = x.l + 1.85 * abs(x.g),
         y = y.u+.05*abs(y.g),
         yend =y.l-.75*abs(y.g))+
  #hseg
  annotate(geom = "segment",
           x = x.l,
           xend = x.r,
           y = y.u-abs(y.g)*.8,
           yend = y.u-abs(y.g)*.8)

ggsave(plot = q, filename =  paste0(getwd(), "/Figures/SETs/RC SETs rates with CI and table.png"), width = 9, height = 6, units = "in", dpi = 300)
ggsave(plot = p, filename =  paste0(getwd(), "/Figures/SETs/RC SETs rates with CI.png"), width = 9, height = 6, units = "in", dpi = 300)

library(ggthemes)
ggplot(slopes.trt, aes(paste0(sapply(Location, capitalize),"\n", Treatment), mean.rate*1000))+
  geom_errorbar(aes(
    ymin = #ifelse(mean.rate > 0, 
            #      mean.rate * 1000 * .5, 
                  (mean.rate * 1000) - (CI.95.m * 1000),#),
    ymax = #ifelse(mean.rate < 0, 
            #      .5 * mean.rate * 1000, 
                  (mean.rate * 1000) + (CI.95.m * 1000)#)
  ), width = 0.25)+
  geom_point(size = 4, aes(fill = Treatment, shape = Location), show.legend = F)+
  # geom_col(aes(fill = Treatment), color = "black")+
  geom_hline(yintercept = 0, size = 1)+
  scale_fill_manual(values = c("darkgray", "black"))+
  scale_shape_manual(values = c(21, 24))+
  labs(y = "Surface Elevation Change\nMean Rate +/- 95% CI (mm/yr)", x = NULL,
       title = "Surface Elevation Change at RC SETs")+
  theme_bw()+
  theme(panel.grid.major.x = element_blank())

ggsave(filename =  paste0(getwd(), "/Figures/SETs/RC SETs rates with CI points.png"),
       width = 5, height = 5, units = "in", dpi = 300)


library(lmerTest)
library(broom)
a1 <- aov(slopes.SETs$mean.mean.slope ~ slopes.SETs$Treatment*slopes.SETs$Location)
tidy(a1)
summary(a1)
tidy(pairwise.t.test(slopes.SETs$mean.mean.slope, paste(slopes.SETs$Treatment, slopes.SETs$Location),
                pool.sd = T, p.adjust.method = "none"))
tidy(TukeyHSD(a1))

lm(mean.rate ~ Treatment * Location, data = slopes.trt)
newdata = data.frame(Date = date_decimal(seq(2004, 2020, by = 0.25)))

predict(lm(data = summary.SETs, m.m.d.Elev ~ decimal_date(Date)), 
        newdata = newdata, 
        se.fit = TRUE)

require(graphics)

## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

## Prediction intervals, special cases
##  The first three of these throw warnings
w <- 1 + x^2
fit <- lm(y ~ x)
wfit <- lm(y ~ x, weights = w)
predict(fit, interval = "prediction")
predict(wfit, interval = "prediction")
predict(wfit, new, interval = "prediction")
predict(wfit, new, interval = "prediction", weights = (new$x)^2)
predict(wfit, new, interval = "prediction", weights = ~x^2)

##-- From  aov(.) example ---- predict(.. terms)
npk.aov <- aov(yield ~ block + N*P*K, npk)
(termL <- attr(terms(npk.aov), "term.labels"))
(pt <- predict(npk.aov, type = "terms"))
pt. <- predict(npk.aov, type = "terms", terms = termL[1:4])
stopifnot(all.equal(pt[,1:4], pt.,
                    tolerance = 1e-12, check.attributes = FALSE))
# }