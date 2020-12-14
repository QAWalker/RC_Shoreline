#ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Color/
#ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Grayscale/
VegData.melt %>% 
  filter(variable == "Salt",
         Site %in% c("NCMM"),
         # Feature %in% c("None", "Oyster", "Behind"),
         Plot <= 20) %>% 
  ggplot(., aes(Date, value, color = paste(Transect, Treatment)))+
    geom_line()+
    geom_point()+
    facet_grid(Plot~Treatment+Feature)

ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
  facet_grid(~Site)+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Color/Plot Elevation by Distance from Shoreline boxplots with sites.png"),
       width = 9, height = 4, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Color/Plot Elevation by Distance from Shoreline boxplots.png"),
       width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
  facet_grid(~Site)+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(9,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Grayscale/Plot Elevation by Distance from Shoreline boxplots with sites.png"),
       width = 9, height = 4, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), elev.initial))+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = cbPalette[c(9,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Plot Elevation and Distance from Shoreline", 
       y = "Starting Elevation (m NAVD88)")
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Grayscale/Plot Elevation by Distance from Shoreline boxplots.png"),
       width = 9, height = 6, units = "in", dpi = 330)
##
## position in tidal frame #####
##

ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
  geom_boxplot(aes(fill = Treatment))+
  facet_grid(~Site)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Position in Tidal Frame and Distance from Shoreline", 
       y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
  geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
  geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                              x = .5,
                              y = c(0.05, -.45, -.95)), 
            aes(x, y, label = name), 
            hjust = 0)
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Color/Position in Tidal Frame by Distance from Shoreline boxplots with sites.png"),
       width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
  geom_boxplot(aes(fill = Treatment))+
  facet_grid(~Site)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  scale_fill_manual(values = cbPalette[c(9,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Position in Tidal Frame and Distance from Shoreline", 
       y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
  geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
  geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                              x = .5,
                              y = c(0.05, -.45, -.95)), 
            aes(x, y, label = name), 
            hjust = 0)
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Grayscale/Position in Tidal Frame by Distance from Shoreline boxplots with sites.png"),
       width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
  geom_boxplot(aes(fill = Treatment))+
  #facet_grid(~Site)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  scale_fill_manual(values = cbPalette[c(3,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Position in Tidal Frame and Distance from Shoreline", 
       y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
  geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
  geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                              x = .5,
                              y = c(0.05, -.45, -.95)), 
            aes(x, y, label = name), 
            hjust = 0)
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Color/Position in Tidal Frame by Distance from Shoreline boxplots.png"),
       width = 9, height = 6, units = "in", dpi = 330)

ggplot(elev.initial, aes(as.factor(Plot), rel.elev))+
  geom_boxplot(aes(fill = Treatment))+
  #facet_grid(~Site)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  scale_fill_manual(values = cbPalette[c(9,8)])+
  labs(x = "Distance from Shoreline (m)", 
       title = "Initial Position in Tidal Frame and Distance from Shoreline", 
       y = "relative position within tidal frame (% of tidal frame)\n(E - MHW / MHW - MLW)")+
  geom_hline(yintercept = c(0, -0.5, -1), linetype = "dotdash")+
  geom_text(data = data.frame(name = c("MHW", "MTL", "MLW"), 
                              x = .5,
                              y = c(0.05, -.45, -.95)), 
            aes(x, y, label = name), 
            hjust = 0)
ggsave(filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/Grayscale/Position in Tidal Frame by Distance from Shoreline boxplots.png"),
       width = 9, height = 6, units = "in", dpi = 330)

library(scales)
library(egg)

VegData.summary <- select(VegData.summary, -"variable.axislabel")

VegData.summary <- filter(VegData.melt, 
                                 Site %in% c("PKS", "PI", "NCMM"),
                                 Feature %in% c("Behind", "None", "Oyster"), 
                                 MarshType != "Non-Fringing",
                                 Season == "Summer") %>% 
  group_by(SampTime, Season, MarshType, Treatment, Plot, variable) %>% 
  summarise(Date = mean(Date, na.rm = T),
            mean = mean(value, na.rm = T), 
            sd = sd(value, na.rm = T),
            n = sum(!is.na(value)),
            se = sd/sqrt(n)) %>% 
  ungroup()

unique(VegData.summary$Treatment)
yearaxis <- function(s){paste0("'", substr(s, 3, 4))}
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))
spp = "Spat"

if(!exists("spplist")) {
  spplist <- list()
} else{
  if (!is_list(spplist)) {
    spplist <- list()
  }
}

#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$pctcover.plotting <- c("Spat", "Sspp", "oyster_live", "oyster_culch", "mussel")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$quant.plotting <- c("liveStem_m2", "HMean", "snails_m2")

for(spp in spplist$pctcover.plotting){#}
  if(T){
    plotData <- VegData.summary %>% 
      filter(Plot<=20,
             variable == "Salt") %>% 
      arrange(Treatment, Plot, variable, Date)
    
    saltpcv <- ggplot(plotData, aes(decimal_date(Date), mean/100, group = Treatment))+
      geom_errorbar(aes(min = mean/100 - se/100, max = mean/100 + se/100), width = 0.25)+
      geom_line()+
      geom_point(aes(fill = Treatment), shape = 21, size = 3)+
      facet_grid(.~Plot)+
      scale_fill_manual(values = c("darkgray", "white"))+
      labs(x = NULL, 
           y = expression(atop(paste(italic("S. alterniflora"), " Percent Cover"), "(mean ?? SE)")))+ 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      scale_x_continuous(breaks = seq(2007,2019, by = 3), labels = yearaxis)+
      theme(legend.position = "none", axis.title.y = element_text(size = rel(.85)))
    
    plotData <- VegData.summary %>% 
      filter(Plot<=20,
             variable == "liveStem_m2")
    
    saltdensity <- ggplot(plotData, aes(decimal_date(Date), mean, group = Treatment))+
      geom_errorbar(aes(min = mean - se, max = mean +se), width = 0.25)+
      geom_line()+
      geom_point(aes(fill = Treatment), shape = 21, size = 3)+
      facet_grid(.~Plot)+
      scale_fill_manual(values = c("darkgray", "white"))+
      scale_x_continuous(breaks = seq(2007,2019, by = 3), labels = yearaxis)+
      labs(x = NULL, 
           y = expression(atop(paste(italic("S. alterniflora"), " Density"), "(mean live stems "%.%m^-2~"?? SE)")))+
      theme(legend.position = "none",
            legend.justification = c(1,1),
            axis.title.y = element_text(size = rel(.85)),
            legend.box.background = element_rect(color = "black"),
            legend.box.margin = margin(1, 1, 1, 1))
    
    
    axisname <- unlist(sppnames$axis.label[sppnames$abb==spp])
    
    plotData <- VegData.summary %>% 
      filter(Plot<=20,
             variable == spp)
    
    salipcv <- ggplot(plotData, aes(decimal_date(Date), mean/100, group = Treatment))+
      geom_errorbar(aes(min = mean/100 - se/100, max = mean/100 + se/100), width = 0.25)+
      geom_line()+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      scale_x_continuous(breaks = seq(2007,2019, by = 3), labels = yearaxis)+
      geom_point(aes(fill = Treatment), shape = 21, size = 3)+
      facet_grid(.~Plot)+
      scale_fill_manual(values = c("darkgray", "white"))+
      labs(fill = NULL, x = NULL, 
           y = bquote(atop(paste(italic(.(axisname))," Percent Cover"),"(mean ?? SE)")))+ 
      theme(legend.position = "bottom", axis.title.y = element_text(size = rel(.85)))
    
    p <-
      ggarrange(saltpcv,
                   tag_facet(saltdensity, color = "transparent"),
                   tag_facet(salipcv, color = "transparent"))
    
    ggsave(p,
           filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/S. alt cover, S. alt density and ", axisname, " cover midpoint.png"),
           width = 9, height = 6, units = "in", dpi = 330)
  }  
}

spp = "snails_m2"
axisname <- unlist(sppnames$axis.label[sppnames$abb==spp])
plotData <- VegData.summary %>% 
  filter(Plot<=20,
         variable == "snails_m2")

codat <- plotData[plotData$mean==max(plotData$mean),]
calloutdf <- data.frame(Plot = codat$Plot, xend = decimal_date(codat$Date), x  = 2017, yend = signif(codat$mean, 3), y = 67.5, se = signif(codat$se, 3))

snailsdensity <- ggplot(plotData, aes(decimal_date(Date), mean, group = Treatment))+
  geom_errorbar(aes(min = mean - se, max = mean +se), width = 0.25)+
  geom_line()+
  geom_point(aes(fill = Treatment), shape = 21, size = 3)+
  facet_grid(.~Plot)+
  scale_fill_manual(values = c("darkgray", "white"))+
  scale_x_continuous(breaks = seq(2007,2019, by = 3), labels = yearaxis)+
  labs(fill = NULL, 
       x = NULL, 
       y = expression(atop(paste(italic("L. littorea"), " Density"), "(mean snails"%.%m^-2~"?? SE)")))+
  theme(legend.position = "bottom", axis.title.y = element_text(size = rel(.85)))+ 
  coord_cartesian(ylim = c(0, 82))+
  geom_segment(data = calloutdf, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = F, size = 1, color = "darkgray")+
  geom_label(data = calloutdf, aes(x = x, y = y, label = paste0("Sill 2012 =\n", yend, " ?? ", se)), 
            inherit.aes = F, hjust = .5, vjust = .5, size = rel(2), label.r = unit(.25, "mm"))

# p <- ggarrange(saltpcv,
#           tag_facet(saltdensity, color = "transparent"),
#           tag_facet(snailsdensity, color = "transparent"))
ggsave(ggarrange(saltpcv,
                 tag_facet(saltdensity, color = "transparent"),
                 tag_facet(snailsdensity, color = "transparent")),
       filename = paste0("R:/CEE/RC shoreline/VEG 2020 msc/Figures/S. alt cover, S. alt density and snail density midpoint.png"),
       width = 9, height = 6, units = "in", dpi = 330)

source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/sppnames.R")


