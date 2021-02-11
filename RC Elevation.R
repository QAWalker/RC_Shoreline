library(tidyverse)
library(lubridate)
source(paste0(getwd(), "/helper scripts/sppnames.R"))

cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))

SiteDatums <- data.frame(Site = c("PKS", "PI", "NCMM"), MLW = c(-.276, -.545, -.537), MHW = c(.232, .424, .425))

if(!exists("spplist")) {
  spplist <- list()
} else{
  if (!is_list(spplist)) {
    spplist <- list()
  }
}
capitalize <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$pctcover.plotting <- c("Salt", "Spat", "Sspp", "oyster_live", "oyster_culch", "mussel")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$quant.plotting <- c("liveStem_m2", "HMean", "snails_m2", "BMtotal")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$rare.plotting <- c("Bfru", "Dspi", "Hspp", "Ifru", "Jroe", "Lcar", "Malb", "Slin", "Srob", "Ssem", "Vlut", "macroalgae")

# read in raw data
elev.raw <- read_xlsx(paste0(getwd(), "/Data/RC_Shoreline_Vegetation_Elevation_MSC.xlsx"), sheet = "RC Shoreline Elevation MASTER", na = c("#VALUE!", "-"))

# basic wrangling
elev <- elev.raw %>% 
  filter(!is.na(Month)) %>% 
  mutate(Date = ymd(Date), 
         Treatment = Treatment,
         Feature = Feature) %>% 
  droplevels() %>% 
  filter(Plot <= 20,
         Site %in% c("PKS", "PI", "NCMM"),
         Plot %in% c(-1, 0, 5, 10, 15, 20)
         ) %>% 
  group_by(Treatment, Plot, Transect, Site, Feature) %>% 
  arrange(Date) %>% 
  mutate(d.Corrected = Corrected - Corrected[1], #calculate the change from the first reading
         intial.date = Date[1]) %>% 
  ungroup()

elev.summary <- elev %>% 
  filter(Feature %in% c("Oyster", "None", "Behind"))%>% 
  group_by(SampTime, Treatment, Plot) %>% 
  summarize(Date = mean(Date), 
            n = sum(!is.na(Correction)), 
            elev.mean = mean(Corrected, na.rm = T),
            elev.sd = sd(Corrected, na.rm = T),
            elev.se = elev.sd/sqrt(n),
            elev.d.mean = mean(d.Corrected, na.rm = T),
            elev.d.sd = sd(d.Corrected, na.rm = T), 
            elev.d.se = elev.d.sd/sqrt(n)) %>% 
  ungroup()

elev.initial <- elev %>%
  filter(Feature!= "Adjacent") %>% 
  # mutate(Transect = Transect)) %>% 
  arrange(Date) %>% 
  group_by(Site, Treatment, Transect, Plot) %>% 
  summarize(elev.initial = Corrected[1], 
            initial.date = Date[1], 
            initial.year = year(Date[1])) %>% 
  ungroup() %>% 
  mutate(elev.bin = round(elev.initial, 1), 
         rel.elev= as.numeric(NA))

# left_join(elev, elev.initial[,c("Site", "Treatment", "Transect", "Plot", "initial.date")])

elev.summary.yr <- list()  
for (yr in unique(year(elev$intial.date))) {
  elev.summary.yr[[as.character(yr)]] <- elev %>% 
    filter(year(intial.date) == yr, 
           Feature %in% c("Oyster", "None", "Behind"))%>% 
    group_by(SampTime, Treatment, Plot) %>% 
    summarize(initial.year = yr, 
              Date = mean(Date), 
              n = sum(!is.na(Correction)), 
              elev.mean = mean(Corrected, na.rm = T),
              elev.sd = sd(Corrected, na.rm = T),
              elev.se = elev.sd/sqrt(n),
              elev.d.mean = mean(d.Corrected, na.rm = T),
              elev.d.sd = sd(d.Corrected, na.rm = T), 
              elev.d.se = elev.d.sd/sqrt(n)) %>% 
    ungroup()
}

elev.summary.allyrs <-
  bind_rows(
    elev.summary.yr[["2007"]],
    elev.summary.yr[["2008"]],
    elev.summary.yr[["2010"]],
    elev.summary.yr[["2018"]],
    elev.summary.yr[["2019"]]
  )

# write_csv(elev.summary.allyrs, path = paste0(getwd(), "/Data/R Output/Vegetation Elevation/elev.summary.allyrs.csv"))

#### Plotting ####
ggplot(elev, aes(x = Year, y = Corrected, group = paste(Treatment, Feature, Transect, Plot), color = Treatment)) +
  geom_line() +
  geom_point() +
  facet_grid(Plot~Site, scale = "free")

ggplot(elev, aes(x = Year, y = d.Corrected, group = paste(Site, Treatment, Feature, Transect, Plot), color = Treatment)) +
  geom_line() +
  geom_point() +
  facet_grid(Plot~Site, scale = "free")

ggplot(elev.summary, aes(Date, y = elev.d.mean, color = as.factor(Treatment)))+
  geom_errorbar(aes(min = elev.d.mean - elev.d.se, max = elev.d.mean + elev.d.se), width = 90)+
  geom_line()+
  geom_point(size = 3)+
  facet_wrap(Plot~., labeller = "label_both")+
  scale_color_manual(values = cbPalette[c(3,8)])+
  labs(title = "Vegetation Plot Elevation Change", 
       y = "Change in Mean Plot Elevation (m NAVD)\nMean ± SE", color = 'Treatment', x = NULL)
ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/Mean Elevation Change.png"),
       width = 9, height = 6, units = "in", dpi = 440)

ggplot(elev.summary, aes(Date, y = elev.mean, color = as.factor(Treatment)))+
  # geom_smooth(method = 'lm', se = F, aes(group = Treatment), color = "gray30") +
  geom_errorbar(aes(min = elev.mean - elev.se, max = elev.mean + elev.se), width = 90) +
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(Plot~., labeller = "label_both") +
  scale_color_manual(values = cbPalette[c(3,8)])+
  labs(title = "Vegetation Plot Elevation",
       y = "Plot Elevation (m NAVD)\nMean ± SE", color = 'Treatment', x = NULL)
ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/Mean Elevation.png"),
       width = 9, height = 6, units = "in", dpi = 440)

ggplot(elev.summary.allyrs, aes(Date, y = elev.d.mean, group = paste(initial.year, Treatment), color = Treatment, shape = factor(initial.year)))+
  geom_errorbar(aes(min = elev.d.mean - elev.d.se, max = elev.d.mean + elev.d.se), width = 90)+
  geom_line(size = 0.65)+
  geom_point(size = 3, aes(fill = Treatment), color = "black")+
  scale_color_manual(values = cbPalette[c(3,8)])+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  scale_shape_manual(values = c(21, 22, 23, 24, 25))+
  facet_wrap(Plot~., labeller = "label_both")+
  labs(title = "Vegetation Plot Elevation Change", 
       y = "Change in Mean Plot Elevation (m NAVD)\nMean ± SE",
       color = 'Treatment', x = NULL, shape = "Initial Survey Year")+
  guides(color = guide_legend(override.aes = list(shape = 22, color = "black", size = 4, linetype = NA)),
         shape = guide_legend(override.aes = list(color = "black", size = 4, fill = "black")))
ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/Mean Elevation change by initial year.png"),
       width = 9, height = 6, units = "in", dpi = 440)

for (S in SiteDatums$Site) {
  elev.initial[elev.initial$Site == S, "rel.elev"] = 
    (elev.initial[elev.initial$Site == S, "elev.initial"] -
       SiteDatums[SiteDatums$Site == S, "MHW"]) / (SiteDatums[SiteDatums$Site == S, "MHW"] -
                                                     SiteDatums[SiteDatums$Site == S, "MLW"])

}


elev.initial <- mutate(elev.initial, rel.elev.bin = ceiling(rel.elev/.25)*.25, 
                       Transect = factor(Transect))

VegData.elev <- left_join(VegData, elev.initial) 
VegData.elev.melt <- left_join(VegData.melt, elev.initial) 

VegData.elev.summary <- VegData.elev.melt %>%
  filter(!(Feature %in% c("Adjacent", "Eroded")), Site %in% c("PKS", "NCMM", "PI"), !is.nan(value)) %>% 
  group_by(Site, Treatment, elev.bin, SampTime, Season, variable) %>%
  summarize(
    Date = mean(Date),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    se = NA,
    n = length(value)#,
    # max = ifelse(is.na(mean(value)), NA, max(value, na.rm = T)),
    # min = ifelse(is.na(mean(value)), NA, min(value, na.rm = T))
  ) %>%
  ungroup() %>%
  mutate(se = ifelse(n == 1, NA, sd / sqrt(n)), 
         variable.sppname = NA, variable.commonname = NA, variable.axislabel = NA, Site.label = NA,
         Transect = "Mean ? SE") %>% 
  filter(T)

VegData.elev.summary.nosite <- VegData.elev.melt %>%
  filter(!(Feature %in% c("Adjacent", "Eroded")), Site %in% c("PKS", "NCMM", "PI"), !is.nan(value)) %>% 
  group_by(Treatment, elev.bin, SampTime, Season, variable) %>%
  summarize(
    Date = mean(Date),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    se = NA,
    n = length(value)#,
    # max = ifelse(is.na(mean(value)), NA, max(value, na.rm = T)),
    # min = ifelse(is.na(mean(value)), NA, min(value, na.rm = T))
  ) %>%
  ungroup() %>%
  mutate(se = ifelse(n == 1, NA, sd / sqrt(n))) %>% 
  filter(T)

VegData.TP.summary <- VegData.elev.melt %>%
  filter(!(Feature %in% c("Adjacent", "Eroded")), Site %in% c("PKS", "NCMM", "PI"), !is.nan(value)) %>% 
  group_by(Site, Treatment, rel.elev.bin, SampTime, Season, variable) %>%
  summarize(
    Date = mean(Date),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    se = NA,
    n = length(value)#,
    # max = ifelse(is.na(mean(value)), NA, max(value, na.rm = T)),
    # min = ifelse(is.na(mean(value)), NA, min(value, na.rm = T))
  ) %>%
  ungroup() %>%
  mutate(se = ifelse(n == 1, NA, sd / sqrt(n)), 
         variable.sppname = NA, variable.commonname = NA, variable.axislabel = NA, Site.label = NA,
         Transect = "Mean ? SE") %>% 
  filter(T)

VegData.TP.summary.nosite <- VegData.elev.melt %>%
  filter(!(Feature %in% c("Adjacent", "Eroded")), Site %in% c("PKS", "NCMM", "PI"), !is.nan(value), Season == "Summer") %>% 
  group_by(Treatment, rel.elev.bin, SampTime, Season, variable) %>%
  summarize(
    Date = mean(Date),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    se = NA,
    n = length(value)#,
    # max = ifelse(is.na(mean(value)), NA, max(value, na.rm = T)),
    # min = ifelse(is.na(mean(value)), NA, min(value, na.rm = T))
  ) %>%
  ungroup() %>%
  mutate(se = ifelse(n == 1, NA, sd / sqrt(n))) %>% 
  filter(T)

for (site in unique(elev$Site)) {
  ggplot(filter(elev, Site == site, Transect != 0), aes(x = Plot, y = Corrected, group = paste(Transect, Year), color = as.factor(Year))) +
    geom_line() +
    geom_point() +
    facet_wrap(~Treatment+Transect,  labeller = labeller(Transect = label_both))+
    labs(title = paste(site, "Elevations"), 
         y = "Plot Elevation (m NAVD88)",
         x = "Distance from Shoreline", 
         color = NULL)
  ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/", site, " transect profiles.png"),width = 9, height = 6, units = "in", dpi = 440)
  
  ggplot(filter(elev, Site == site, Transect != 0), aes(x = Year, y = Corrected, group = paste(Transect, Plot), color = as.factor(Plot))) +
    geom_line() +
    geom_point() +
    facet_wrap(~Treatment+Transect,  labeller = labeller(Transect = label_both))+
    labs(title = paste(site, "Elevations"), 
         y = "Plot Elevation (m NAVD88)",
         x = NULL, 
         color = "Plot")
  ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/", site, " transect profiles change.png"),width = 9, height = 6, units = "in", dpi = 440)
  
  ggplot(filter(elev, Site == site, Transect != 0), aes(x = Year, y = Corrected, color = as.factor(Transect))) +
    geom_line() +
    geom_point() +
    facet_grid(Treatment~Plot,  labeller = "label_both")+
    labs(title = paste(site, "Elevations"), 
         y = "Plot Elevation (m NAVD88)",
         x = NULL, 
         color = "Transect")
  ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/", site, " plot change.png"),width = 9, height = 6, units = "in", dpi = 440)
}

ggplot(elev, aes(Corrected))+
  geom_histogram()+
  facet_grid(Plot~Treatment)

ggplot(elev, aes(Corrected, fill = Treatment))+
  geom_histogram(binwidth = 0.05)+
  facet_grid(Plot~Treatment+Year)

table(year(elev.initial$initial.date))

ggplot(filter(elev.initial, ), aes(elev.initial, fill = as.factor(initial.year)))+
  geom_histogram(binwidth = .15)+
  facet_grid(Site~Treatment)+
  # scale_fill_manual(values = cbPalette[c(2:4,1)])+
  labs(title = "initial elevations (2007 or 2008)",
       subtitle = "15 cm bins",
       x = "initial elevation (m NAVD88)", 
       fill = "Year of Initial Survey")
# ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/initial elevations 15 cm bins.png"),width = 5, height = 4, units = "in", dpi = 150)

df <- left_join(VegData, elev.initial) 

df <- df[order(df$elev.initial),]

##### Elevation figures #####
elev.labeller <- function(s){paste0("Plot Elevation:\n", as.numeric(s)-.10, "m - ", as.numeric(s), "m")}

ggplot(elev.initial, aes(as.factor(Plot), fill = Treatment))+
  geom_bar()+
  facet_grid(elev.bin~Site)+
  labs(x = "Plot")
# ggsave(filename = paste0(getwd(), "/Graphs/Elevation/Plot Shoreline Distance and Starting Elevation with sites.png"),
#        width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), fill = Treatment))+
  geom_bar()+
  facet_grid(elev.bin~.)+
  labs(x = "Plot")
# ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Elevation/Plot Shoreline Distance and Starting Elevation.png"),
#        width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
  facet_grid(~Site)+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")
# ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/Plot Elevation by Distance from Shoreline boxplots with sites.png"),
#        width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")
# ggsave(filename = paste0(getwd(), "/Figures/Plot Elevation/Plot Elevation by Distance from Shoreline boxplots.png"),
#        width = 9, height = 6, units = "in", dpi = 330)

if(exists("summary.SETs")){
  SETs.initial <- summary.SETs %>% 
    arrange(Date) %>% 
    group_by(Site, Treatment, Location, SET) %>% 
    summarise(elev.initial = m.m.Elev[1], 
              date.initial = Date[1]) %>% 
    ungroup() %>% 
    mutate(Plot = "SETs",
           Treatment = ifelse(Treatment == "Natural", "Nat", Treatment))
  
  for (S in unique(SiteDatums$Site)) {
    SETs.initial[SETs.initial$Site == S, "rel.elev"] = 
      (SETs.initial[SETs.initial$Site == S, "elev.initial"] -
         SiteDatums[SiteDatums$Site == S, "MHW"]) / (SiteDatums[SiteDatums$Site == S, "MHW"] -
                                                       SiteDatums[SiteDatums$Site == S, "MLW"])
  }
  
  ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
    geom_point(data = SETs.initial, show.legend = T,
               aes(y = elev.initial, fill = Treatment), 
               position =  position_dodge(0.9),
               size = 3, shape = 21)+
    geom_boxplot(aes(fill = Treatment))+
    scale_color_manual(values = cbPalette[c(3,8)])+
    scale_x_discrete(limits = c(-1, 0, 5, 10, 15, 20, "SETs"))+
    scale_fill_manual(values = cbPalette[c(3,8)])+
    labs(x = "Distance from Shoreline (m)", 
         title = "Initial Elevation and Distance from Shoreline", 
         y = "Starting Elevation (m NAVD88)")
  ggsave(filename = paste0(getwd(),"/Figures/Plot Elevation/Plot Elevation by Distance from Shoreline boxplots with SETs.png"),
         width = 9, height = 6, units = "in", dpi = 330)
}

elev.initial.withallsites <- rbind(elev.initial, mutate(elev.initial, Site = "All Sites"))

ggplot(elev.initial.withallsites, aes(as.factor(Plot), elev.initial))+
  facet_grid(~Site)+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")

# ggsave(filename = paste0(getwd(),"/Figures/Plot Elevation/Plot Elevation by Distance from Shoreline boxplots with sites and all sites.png"),
#        width = 9, height = 6, units = "in", dpi = 330)

##spp = "Salt"
for(spp in unlist(spplist)){#}
  df.p.ns <- VegData.elev.summary.nosite %>% 
    filter(variable == spp, !is.na(elev.bin), !is.nan(mean),
           elev.bin <=0.6, elev.bin>=-0.4) #trimming out elev bins without both treatments
  
  sppname = sppnames$sppname[sppnames$abb==spp] 
  commonname = sppnames$commonname[sppnames$abb==spp] 
  axisname = sppnames$axis.label[sppnames$abb==spp] 
  unit = sppnames$unit[sppnames$abb==spp][[1]]
  unitname = sppnames$unitname[sppnames$abb==spp]
  suffix = "± SE" # Alt+0177 = ± (plus minus sign)
  
  ggplot(df.p.ns, aes(Date, mean, color = Treatment, group = Treatment))+
    geom_line(color = "black")+
    geom_errorbar(aes(min = mean - se, max = mean+se), color = "black", width = 90)+
    geom_point(size = 3)+
    facet_wrap(~elev.bin, labeller = as_labeller(elev.labeller))+
    labs(title = paste0(sppname, " (",commonname,") ", tolower(unitname), " by starting plot elevation"), 
         y = bquote("Mean "*.(axisname)~.(tolower(unitname))~"("*.(unit)~.(suffix)*")"),
         x = NULL)+
    theme(legend.position = c(.875, .165))
  
  ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/", axisname, " ", unitname," based on starting plot elevation.png"),
         width = 9, height = 6, units = "in", dpi = 330)
}


##### tidal position figures #####
rel.elev.labeller <- function(s){paste0("Tidal Range above MHW:\n", as.numeric(s)*100-25, "%  -  ", as.numeric(s)*100, "%")}
rel.elev.labeller.small <- function(s){paste0(as.numeric(s)*100-25, "%-\n", as.numeric(s)*100, "%")}
###
ggplot(elev.initial, aes(as.factor(Plot), fill = Treatment))+
  geom_bar()+
  facet_grid(rel.elev.bin~Site, labeller = labeller(rel.elev.bin = rel.elev.labeller.small))+
  scale_y_continuous(breaks = seq(0, 10, by = 2))+
  labs(x = "Plot Distance from Initial Shoreline (m)",
       title = "Plot Shoreline Distance and Starting Position in Tidal Frame")

# ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/Plot Shoreline Distance and Starting Position in Tidal Frame with sites.png"),
#        width = 9, height = 6, units = "in", dpi = 330)
###
ggplot(filter(elev.initial), aes(as.factor(Plot), fill = Treatment))+
  geom_bar()+
  facet_wrap(~rel.elev.bin, nrow = 3, labeller = as_labeller(rel.elev.labeller))+
  labs(x = "Plot Distance from Initial Shoreline (m)",
       title = "Plot Shoreline Distance and Starting Position in Tidal Frame")+
  theme(legend.position = c(.75, .165), legend.box = "horizontal")

# ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/Plot Shoreline Distance and Starting Position in Tidal Frame.png"),
#        width = 9, height = 6, units = "in", dpi = 330)

elev.initial.withallsites <- rbind(elev.initial, mutate(elev.initial, Site = "All Sites"))

ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
  geom_boxplot(aes(fill = Treatment))+
  facet_grid(~Site)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  scale_color_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Plot Shoreline Distance and Starting Position in Tidal Frame", 
       y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
  geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
  geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                              x = .5,
                              y = c(0.05, -.45, -.95)), 
            aes(x, y, label = name), 
            hjust = 0)

ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/Plot Shoreline Distance and Starting Position in Tidal Frame boxplots with sites.png"),
       width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
  geom_boxplot(aes(fill = Treatment))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Plot Shoreline Distance and Starting Position in Tidal Frame", 
       y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
  geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
  geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                              x = .5,
                              y = c(0.05, -.45, -.95)), 
            aes(x, y, label = name), 
            hjust = 0)

ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/Plot Shoreline Distance and Starting Position in Tidal Frame boxplots.png"),
       width = 9, height = 6, units = "in", dpi = 330)

if(exists("SETs.initial")){
  ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
    geom_boxplot(aes(fill = Treatment))+
    geom_point(data = SETs.initial, show.legend = F,
               aes(y = rel.elev, fill = Treatment), 
               position =  position_dodge(0.75),
               size = 3, shape = 21)+
    # geom_point(show.legend = T,
    #            aes(y = rel.elev, fill = Treatment), 
    #            position =  position_dodge(0.75),
    #            size = 3, shape = 21)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
    scale_fill_manual(values = cbPalette[c(3,8)])+
    # scale_fill_manual(values = c("white", "gray"))+
    scale_x_discrete(limits = c(-1, 0, 5, 10, 15, 20, "SETs"))+
    labs(x = "Distance from Shoreline (m)", 
         title = "Plot Shoreline Distance and Starting Position in Tidal Frame", 
         y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
    geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
    geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                                x = .5,
                                y = c(0.05, -.45, -.95)), 
              aes(x, y, label = name), 
              hjust = 0)
  
  ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/Plot Shoreline Distance and Starting Position in Tidal Frame boxplots with SETs.png"),
         width = 9, height = 6, units = "in", dpi = 330)
}
#### ####
for(spp in unlist(spplist)){
  df.tp.p.ns <- VegData.TP.summary.nosite %>% 
    filter(variable == spp, !is.na(rel.elev.bin), !is.nan(mean))
  
  sppname = sppnames$sppname[sppnames$abb==spp] 
  commonname = sppnames$commonname[sppnames$abb==spp] 
  axisname = sppnames$axis.label[sppnames$abb==spp] 
  unit = sppnames$unit[sppnames$abb==spp][[1]]
  unitname = sppnames$unitname[sppnames$abb==spp]
  suffix = "± SE" # Alt+0177 = plus minus
    
  ggplot(df.tp.p.ns, aes(Date, mean, color = Treatment, group = Treatment))+
    geom_line(color = "black")+
    geom_errorbar(aes(min = mean - se, max = mean+se), color = "black", width = 90)+
    geom_point(size = 3)+
    facet_wrap(~rel.elev.bin, nrow = 3, labeller = as_labeller(rel.elev.labeller))+
    labs(title = paste0(sppname, " (",commonname,") ", tolower(unitname), " by starting position in tidal frame"), 
         y = bquote("Mean "*.(axisname)~.(tolower(unitname))~"("*.(unit)~.(suffix)*")"),
         x = NULL)+
    theme(legend.position = c(.75, .165), legend.box = "horizontal")
  
  ggsave(filename = paste0(getwd(), "/Figures/Vegetation and Plot Elevation/", axisname, " ", unitname," based on starting relative position in tidal frame.png"),
         width = 9, height = 6, units = "in", dpi = 330)
}
