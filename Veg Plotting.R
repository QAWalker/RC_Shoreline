#rm(list = ls())

#run the data wrangling script to create our data frames for plotting
# source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/Master Veg Data Wrangling.R")

# set universal variables for plotting ####
# set theme for plotting 
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))

#create color-blind pallette for plotting
cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")
IMBpal <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")
if(!exists("spplist")) {
  spplist <- list()
} else{
  if (!is_list(spplist)) {
    spplist <- list()
  }
}

yearaxis <- function(s){paste0("'", substr(s, 3, 4))}

source(paste0(getwd(), "/species name helper functions.R"))

capitalize <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$pctcover.plotting <- c("Salt", "Spat", "Sspp", "oyster_live", "oyster_culch", "mussel")
#create a vector of categories that we want to plot (avoids creating too many figures for uncommon categories)
spplist$quant.plotting <- c("liveStem_m2", "HMean", "snails_m2")

sppabb = "Salt"
##### Natural vs Sill pct cover plots #####
for (sppabb in spplist$pctcover.plotting) {#}
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  tempdf.summary <-
    filter(
      VegData.summary,
      Season == "Summer",
      Plot <= 20,
      variable == sppabb
      )
  
  p <- ggplot(tempdf.summary, aes(Date, mean, fill = Treatment))+
    geom_line()+
    geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
    geom_point(size = 2.5, shape = 21)+
    facet_grid(.~Plot, labeller = "label_both")+
    scale_fill_manual(values = c("darkgray", "white"))+
    scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))
  
  if(commonname == speciesname){
    p <- p +
    labs(x = NULL, 
         y = expression("Mean Percent Cover "%+-%" SE"),
         title = paste(commonname, "Percent Cover"))
                        
  }else{
    p <- p +
      labs(x = NULL, 
           y = expression("Mean Percent Cover "%+-%" SE"),
           title = bquote(italic(.(speciesname))~"Percent Cover"))
  }
  
  ggsave(plot = p,
         paste0(getwd(), "/Figures/",speciesname, " Cover.png"),
         width = 10, height = 3, units = "in", dpi = 330)
}  
##### Natural vs Sill quantative plots #####
for (sppabb in spplist$quant.plotting) {#}
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  tempdf.summary <-
    filter(
      VegData.summary,
      Season == "Summer",
      Plot <= 20,
      variable == sppabb
    )
  p <- ggplot(tempdf.summary, aes(Date, mean, fill = Treatment))+
    geom_line()+
    geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
    geom_point(size = 2.5, shape = 21)+
    facet_grid(.~Plot)+
    scale_fill_manual(values = c("darkgray", "white"))
  
  if(sppabb=="HMean"){
    p <- p +
      labs(title = expression(italic("Spartina alterniflora")~"Height"), 
           y = expression("Mean Stem Height (cm) "%+-%" SE"),
           x = NULL)
  }
  
  if(sppabb=="liveStem_m2"){
    p <- p + 
      labs(title = expression(italic("Spartina alterniflora")~"Density"), 
           y = expression("Mean Live Stems "%.%m^-2%+-%" SE"),
           x = NULL)
  }
  
  if(sppabb=="snails_m2"){
    p <- p +
      labs(title = expression(italic("Littorina littorea")~"Density"), 
           y = expression("Mean Snails "%.%m^-2%+-%" SE"),
           x = NULL)
  }
  
  ggsave(plot = p, 
         paste0(getwd(), "/Figures/", speciesname, ifelse(sppabb == "HMean", " Height", " Density"), ".png"),
         width = 10, height = 3, units = "in", dpi = 330)
}
##### Natural vs Sill biomass plots #####
for (sppabb in c("BMMean", "BMtotal")) {#}
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  tempdf.summary <-
    filter(
      VegData.summary,
      Season == "Summer",
      Plot <= 20,
      variable == sppabb, 
      # Date>=ymd("20080101")
    )
  p <- ggplot(tempdf.summary, aes(Date, mean, fill = Treatment))+
    geom_line()+
    geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
    geom_point(size = 2.5, shape = 21)+
    facet_grid(.~Plot, labeller = "label_both")+
    scale_fill_manual(values = c("darkgray", "white"))
  
  if(sppabb == "BMMean"){
    p <- p + labs(title = expression(italic("Spartina alterniflora")~"Mean Plant Biomass"), 
                  y = expression("Mean Plant Biomass (g) "%+-%" SE"),
                  x = NULL)
  }else{
    p <- p + labs(title = expression(italic("Spartina alterniflora")~"Plot Biomass"), 
             y = expression("Mean Plot Biomass ("~g%.%m^-2~") "%+-%" SE"),
             x = NULL)
  }
  
  ggsave(plot = p, 
         paste0(getwd(), "/Figures/", speciesname, ifelse(sppabb == "BMMean", " Average Biomass", " Plot Biomass"), ".png"),
         width = 10, height = 3, units = "in", dpi = 330)
}
sppabb3 = "snails_m2"
##### Natural vs Sill 3 variable comparison plots #####
if(T){
  sppabb1 <- "Salt"
  sppabb2 <- "liveStem_m2"
  
  p1 <- filter(VegData.summary, Season == "Summer", Plot <= 20, variable == sppabb1) %>% 
    ggplot(aes(Date, mean, fill = Treatment))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
      geom_point(size = 2.5, shape = 21)+
      facet_grid(.~Plot, labeller = "label_both")+
      scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))+
      scale_fill_manual(values = c("darkgray", "white"))+
      labs(subtitle = expression(italic("Spartina alterniflora")~"Percent Cover"),
           y = expression("Mean Percent Cover "%+-%" SE"),
           x = NULL)+ 
      theme(axis.title=element_text(size=10))
  
  p2 <- filter(VegData.summary, Season == "Summer", Plot <= 20, variable == sppabb2) %>% 
    ggplot(aes(Date, mean, fill = Treatment))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
      geom_point(size = 2.5, shape = 21)+
      facet_grid(.~Plot, labeller = "label_both")+
      scale_fill_manual(values = c("darkgray", "white"))+
      labs(subtitle = expression(italic("Spartina alterniflora")~"Density"), 
           y = expression("Mean Live Stems "%.%m^-2%+-%" SE"),
           x = NULL)+ 
      theme(axis.title=element_text(size=10))
  
  for (sppabb3 in c("snails_m2", "HMean", "Sspp")) {#}
    
    p3 <- filter(VegData.summary, Season == "Summer", Plot <= 20, variable == sppabb3) %>% 
      ggplot(aes(Date, mean, fill = Treatment))+
        geom_line()+
        geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
        geom_point(size = 2.5, shape = 21)+
        facet_grid(.~Plot, labeller = "label_both")+
        scale_fill_manual(values = c("darkgray", "white"))+
        theme(axis.title=element_text(size=10))
    
      if (unitname(sppabb3) == "percent cover") {
        p3 <- p3 +
          labs(subtitle = (bquote(italic(.(speciesname(sppabb3)))~.(capitalize(unitname(sppabb3)[[1]])))), 
               y = bquote("Mean"~.(capitalize(unitname(sppabb3)))~" "%+-%" SE"),
               x = NULL)
      }else{
        p3 <- p3 +
          labs(subtitle = (bquote(italic(.(speciesname(sppabb3)))~.(capitalize(unitname(sppabb3))))), 
               y = bquote("Mean"~italic(.(axis.label(sppabb3)))~.(capitalize(unitname(sppabb3)))%.%m^-2%+-%" SE"),
               x = NULL)
      }
    
    ggsave(plot = ggpubr::ggarrange(p1, p2, p3, nrow = 3, common.legend = T, legend = "right", align = "v"),
           paste0(getwd(), "/Figures/Three Variable Compliations/", 
                  paste(
                    axis.label(sppabb1), unitname(sppabb1), ",",
                    axis.label(sppabb2), unitname(sppabb2), ",",
                    axis.label(sppabb3), unitname(sppabb3), ","),
                  ".png"),
           width = 10, height = 7, units = "in", dpi = 330)
  }
}
   

##### Natural vs Sill site comparison percent cover plots #####
for (sppabb in spplist$pctcover.plotting) {
  tempdf.summary.sites <- VegData.melt %>% 
    filter(MarshType == "Fringing", 
           Feature %in% c('Behind', "Oyster", "None"),
           Site %in% c("PKS", "PI", "NCMM"),
           Season == "Summer",
           Plot <= 20,
           variable == sppabb) %>% 
    dplyr::group_by(Site, Treatment, Plot, SampTime, Season, MarshType, variable) %>%
    dplyr::summarize(
      Date = mean(Date),
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      n = length(value),
      se = ifelse(n <= 1, NA, sd / sqrt(n))) %>%
    ungroup() 
  
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  p <- ggplot(tempdf.summary.sites, aes(Date, mean, fill = Treatment))+
    geom_line()+
    geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
    geom_point(size = 2.5, shape = 21)+
    facet_grid(Site~Plot, labeller = "label_both")+
    scale_fill_manual(values = c("darkgray", "white"))+
    scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))
  
  if(commonname == speciesname){
    p <- p +
      labs(x = NULL, 
           y = expression("Mean Percent Cover "%+-%" SE"),
           title = paste(commonname, "Percent Cover"))
    
  }else{
    p <- p +
      labs(x = NULL, 
           y = expression("Mean Percent Cover "%+-%" SE"),
           title = bquote(italic(.(speciesname))~"Percent Cover"))
  }
  
  ggsave(plot = p, 
         paste0(getwd(), "/Figures/Site Comparison/", speciesname, " Percent Cover.png"),
         width = 10, height = 6, units = "in", dpi = 330)

}

##### Natural vs Sill site comparison quantative plots #####
for (sppabb in spplist$quant.plotting) {#}
  tempdf.summary.sites <- VegData.melt %>% 
    filter(MarshType == "Fringing", 
           Feature %in% c('Behind', "Oyster", "None"),
           Site %in% c("PKS", "PI", "NCMM"),
           Season == "Summer",
           Plot <= 20,
           variable == sppabb) %>% 
    dplyr::group_by(Site, Treatment, Plot, SampTime, Season, MarshType, variable) %>%
    dplyr::summarize(
      Date = mean(Date),
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      n = length(value),
      se = ifelse(n <= 1, NA, sd / sqrt(n))) %>%
    ungroup() 
  
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  p <- ggplot(tempdf.summary.sites, aes(Date, mean, fill = Treatment))+
    geom_line()+
    geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
    geom_point(size = 2.5, shape = 21)+
    facet_grid(Site~Plot, labeller = "label_both")+
    scale_fill_manual(values = c("darkgray", "white"))
  
  if(sppabb=="HMean"){
    p <- p +
      labs(title = expression(italic("Spartina alterniflora")~"Height"), 
           y = expression("Mean Stem Height (cm) "%+-%" SE"),
           x = NULL)
  }
  
  if(sppabb=="liveStem_m2"){
    p <- p + 
      labs(title = expression(italic("Spartina alterniflora")~"Density"), 
           y = expression("Mean Live Stems "%.%m^-2%+-%" SE"),
           x = NULL)
  }
  
  if(sppabb=="snails_m2"){
    p <- p +
      labs(title = expression(italic("Littorina littorea")~"Density"), 
           y = expression("Mean Snails "%.%m^-2%+-%" SE"),
           x = NULL)
  }
  
  ggsave(plot = p, 
         paste0(getwd(), "/Figures/Site Comparison/", speciesname, ifelse(sppabb == "HMean", " Height", " Density"), ".png"),
         width = 10, height = 6, units = "in", dpi = 330)
}

##### Natural vs Sill site comparison percent cover plots #####
for (sppabb in c("BMMean", "BMtotal")) {#}
  tempdf.summary.sites <- VegData.melt %>% 
    filter(MarshType == "Fringing", 
           Feature %in% c('Behind', "Oyster", "None"),
           Site %in% c("PKS", "PI", "NCMM"),
           Season == "Summer",
           Plot <= 20,
           variable == sppabb) %>% 
    dplyr::group_by(Site, Treatment, Plot, SampTime, Season, MarshType, variable) %>%
    dplyr::summarize(
      Date = mean(Date),
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      n = length(value),
      se = ifelse(n <= 1, NA, sd / sqrt(n))) %>%
    ungroup() 
  
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  p <- ggplot(tempdf.summary.sites, aes(Date, mean, fill = Treatment))+
    geom_line()+
    geom_errorbar(aes(min = mean - se, max = mean + se), alpha = 0.85, width = 120)+
    geom_point(size = 2.5, shape = 21)+
    facet_grid(Site~Plot, labeller = "label_both")+
    scale_fill_manual(values = c("darkgray", "white"))
  
  if(sppabb == "BMMean"){
    calloutdf <- tempdf.summary.sites %>% 
      filter(round(mean) == 57) %>% 
      mutate(yend = 12.5,
             xend = ymd("20170701"))
    
    p <- p +
      labs(title = expression(italic("Spartina alterniflora")~"Mean Plant Biomass"), 
                  y = expression("Mean Plant Biomass (g) "%+-%" SE"),
                  x = NULL)+
      coord_cartesian(ylim = c(NA, 15))+
      geom_segment(data = calloutdf, aes(x = Date, y = mean, xend = xend, yend = yend), inherit.aes = F, size = 1, color = "darkgray")+
      geom_label(data = calloutdf, aes(x = xend, y = yend, label = paste0("Nat 2012 =\n", round(mean, 1), " ± ", round(se,1))), 
                 inherit.aes = F, hjust = .5, vjust = .5, size = rel(2), label.r = unit(.25, "mm"))
    
    
  }else{
    p <- p + labs(title = expression(italic("Spartina alterniflora")~"Plot Biomass"), 
                  y = expression("Mean Plot Biomass ("~g%.%m^-2~") "%+-%" SE"),
                  x = NULL)
  }
  
  ggsave(plot = p, 
         paste0(getwd(), "/Figures/Site Comparison/", speciesname, ifelse(sppabb == "BMMean", " Average Biomass", " Plot Biomass"), ".png"),
         width = 10, height = 6, units = "in", dpi = 330)
}
sppabb = "BMMean"
######################



