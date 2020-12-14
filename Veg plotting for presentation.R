rm(list = ls())
setwd("R:/CEE/RC shoreline/Data/Vegetation Data")

# Veg plotting for presentation
cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))

VegData <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/R data/VegData.csv")%>%
  filter(Site == "PKS" | Site == "PI" | Site == "MM" | Site == "NCMM" | Site == "HI") %>%
  mutate(SampTime = factor(SampTime,
                           levels = unique(.$SampTime)
                           [order(substr(
                             x = unique(.$SampTime),
                             start = 3,
                             stop = 6),
                             substr(x = gsub("WI", "1",
                                             gsub("SP", "2", 
                                                  gsub("SU", "3",
                                                       gsub("FA", "4",
                                                            unique(.$SampTime)
                                                       )
                                                  )
                                             )
                             ),
                             start = 1,
                             stop = 2
                             ))
                             ]),
         Plot = ifelse(!is.na(PlotAlt), PlotAlt, Plot))

PctCoverColNums <-
  c((which(names(VegData) == "Acri")):(which(names(VegData) == "Snails_per_area") - 1))

QuantColNums <- c(which(names(VegData) == "snails_m2"),
                  which(names(VegData) == "liveStem_m2"),
                  which(names(VegData) == "HMean"))

SppColNums <- c(PctCoverColNums, QuantColNums)

sppnames <-
  data.frame(
    stringsAsFactors = F,
    "abb" = names(VegData[SppColNums]),
    "sppname" = c(
      "Atriplex cristata",
      "Borrichia frutescens",
      "Baccharis halimifolia",
      "Chynaum palustre",
      "Distichlis spicata",
      "Hydrocotyle spp.",
      "Iva frutescens",
      "Juncus roemerianus",
      "Limonium carolinianum",
      "Melilotus alba",
      "Phytolacca americana",
      "Spartina alterniflora",
      "Suaeda linearis",
      "Spartina patens",
      "Scirpus robustus",
      "Solidago sempervirens",
      "Salicornia spp.",
      "Vigna luteola",
      "Macroalgae",
      "Mussel",
      "Other",
      "Upland",
      "Oyster",
      "Live Oyster",
      "Culch",
      "Rock",
      "Wrack",
      "Debris",
      "Littorina littorea",
      "Spartina alterniflora",
      "Spartina alterniflora"
    ),
    "commonname" = c(
      "Seabeach Orache",
      "Sea Ox-eye",
      "Groundsel Bush",
      "Milkweed Vine",
      "Seashore Saltgrass",
      "Pennywort",
      "High-tide Bush",
      "Black Needlerush",
      "Carolina Sea-Lavender",
      "White Sweet-Clover",
      "American Pokeweed",
      "Smooth Cordgrass",
      "Annual Seablite",
      "Saltmeadow Cordgrass",
      "Saltmarsh Bulrush",
      "Seaside Goldenrod",
      "Pickleweed",
      "Hairy-Pod Cowpea",
      "Macroalgae",
      "Mussel",
      "Other",
      "Upland",
      "Oyster",
      "Live Oyster",
      "Culch",
      "Rock",
      "Wrack",
      "Debris",
      "Common Periwinkle",
      "Smooth Cordgrass",
      "Smooth Cordgrass"
    ),
    "units" = c(
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "percent cover",
      "snails",
      "stems",
      "cm"
    ),
    "title.name" = c(
      "Atriplex cristata (Seabeach Orache) percent cover",
      "Borrichia frutescens (Sea Ox-eye) percent cover",
      "Baccharis halimifolia (Groundsel Bush) percent cover",
      "Chynaum palustre (Milkweed Vine) percent cover",
      "Distichlis spicata (Seashore Saltgrass) percent cover",
      "Hydrocotyle spp. (Pennywort) percent cover",
      "Iva frutescens (High-tide Bush) percent cover",
      "Juncus roemerianus (Black Needlerush) percent cover",
      "Limonium carolinianum (Carolina Sea-Lavender) percent cover",
      "Melilotus alba (White Sweet-Clover) percent cover",
      "Phytolacca americana (American Pokeweed) percent cover",
      "Spartina alterniflora (Smooth Cordgrass) percent cover",
      "Suaeda linearis (Annual Seablite) percent cover",
      "Spartina patens (Saltmeadow Cordgrass) percent cover",
      "Scirpus robustus (Saltmarsh Bulrush) percent cover",
      "Solidago sempervirens (Seaside Goldenrod) percent cover",
      "Salicornia spp. (Pickleweed) percent cover",
      "Vigna luteola (Hairy-Pod Cowpea) percent cover",
      "Macroalgae percent cover",
      "Mussel percent cover",
      "Other  percent cover",
      "Upland percent cover",
      "Crassostrea virginica (Oyster) percent cover",
      "Crassostrea virginica (Oyster) percent cover",
      "Culch percent cover",
      "Rock percent cover",
      "Wrack percent cover",
      "Debris percent cover",
      "Littorina littorea (Common Periwinkle) Density",
      "Spartina alterniflora (Smooth Cordgrass) Density",
      "Spartina alterniflora (Smooth Cordgrass) Stem Height"
    ),
    "axis.label" = c(
      "A. cristata",
      "B. frutescens",
      "B. halimifolia",
      "C. palustre",
      "D. spicata",
      "Hydrocotyle spp.",
      "I. frutescens",
      "J. roemerianus",
      "L. carolinianum",
      "M. alba",
      "P. americana",
      "S. alterniflora",
      "S. linearis",
      "S. patens",
      "S. robustus",
      "S. sempervirens",
      "Salicornia spp.",
      "V. luteola",
      "Macroalgae",
      "Mussel",
      "Other",
      "Upland",
      "Oyster",
      "Live Oyster",
      "Culch",
      "Rock",
      "Wrack",
      "Debris",
      "L. littorea Density",
      "S. alterniflora Density",
      "Mean S. alterniflora height"
    )
  )

VegData.melt <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/R data/VegData_melt.csv", stringsAsFactors = F)%>% 
  mutate(SampTime = factor(SampTime,
                           levels = unique(.$SampTime)
                           [order(substr(
                             x = unique(.$SampTime),
                             start = 3,
                             stop = 6),
                             substr(x = gsub("WI", "1",
                                             gsub("SP", "2", 
                                                  gsub("SU", "3",
                                                       gsub("FA", "4",
                                                            unique(.$SampTime)
                                                       )
                                                  )
                                             )
                             ),
                             start = 1,
                             stop = 2
                             ))
                             ]),
         Date = ymd(Date)) %>% 
  filter(TRUE)

for (sppabb in as.character(unique(VegData.melt$variable))) {
  VegData.melt$variable.sppname[VegData.melt$variable==sppabb] <- sppnames$sppname[sppnames$abb==sppabb]
  VegData.melt$variable.commonname[VegData.melt$variable==sppabb] <- sppnames$commonname[sppnames$abb==sppabb]
  VegData.melt$units[VegData.melt$variable==sppabb] <- sppnames$units[sppnames$abb==sppabb]
  VegData.melt$variable.titlename[VegData.melt$variable==sppabb] <- sppnames$title.name[sppnames$abb==sppabb]
  VegData.melt$variable.axislabel[VegData.melt$variable==sppabb] <- sppnames$axis.label[sppnames$abb==sppabb]
}

VegData.summary.trt <- VegData.melt %>%
  dplyr::select(-variable.axislabel) %>% 
  filter(Site == "PKS" | Site == "PI" | Site == "MM" | Site == "NCMM" | Site == "HI",
         Feature != "Eroded", Feature != "Adjacent") %>% 
  group_by(Treatment, Plot, SampTime, Season, variable) %>%
  summarize(
    Date = mean(Date),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    se = NA,
    n = length(value),
    max = ifelse(is.na(mean(value)), NA, max(value, na.rm = T)),
    min = ifelse(is.na(mean(value)), NA, min(value, na.rm = T))
  ) %>%
  ungroup() %>%
  mutate(se = ifelse(n == 1, NA, sd / sqrt(n)), 
         variable.sppname = NA, variable.commonname = NA, variable.axislabel = NA, Site.label = NA,
         Transect = "Mean ? SE")

#add columns useful for plotting and labeling
for (spp in unique(VegData.summary.trt$variable)) {
  VegData.summary.trt$variable.sppname[VegData.summary.trt$variable == spp] <-
    sppnames$sppname[sppnames$abb == spp]
  VegData.summary.trt$variable.commonname[VegData.summary.trt$variable == spp] <-
    sppnames$commonname[sppnames$abb == spp]
  VegData.summary.trt$variable.axislabel[VegData.summary.trt$variable == spp] <-
    sppnames$axis.label[sppnames$abb == spp]
  rm(spp)
}

#order the df to be more logical
VegData.summary.trt <-
  dplyr::select(VegData.summary.trt, 
                starts_with("Treatment"),
                Plot:Season,
                starts_with("variable"),
                everything())
sppabb  <-  "Salt"

tempdf.summary <-
  filter(VegData.summary.trt,
         variable == sppabb,
         Season != "Fall")

speciesname <-
  as.character(unique(tempdf.summary$variable.sppname))
commonname <-
  as.character(unique(tempdf.summary$variable.commonname))
axisname <-
  as.character(unique(tempdf.summary$variable.axislabel))

unique(tempdf.summary$mean)

p <-
  ggplot(filter(tempdf.summary, Plot < 25), aes(Date, mean)) +
    geom_line(aes(group = Treatment)) +
    geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
    geom_point(aes(color = Treatment), size = 3, show.legend = F) +
    scale_color_manual(values = cbPalette) +
    facet_wrap(~ Plot, labeller = labeller(Plot = label_both)) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    labs(color = "Treatment",
         x = NULL,
         y = paste(axisname, "percent cover", "(Mean ? SE)"))
         #y = expression(paste("L. littorea Density (Mean ", ~Snails%.%m^-2," ? SE)")))

p

ggsave(
  p,
  filename = paste0(
    getwd(),
    "/Graphs/For Presentations/All Plots/",
    "All Treatments Mean ",
    speciesname,
    " ",
    "Density.png"
  ),
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300
)

sppabb  <-  "liveStem_m2"
tempdf.summary <-
  filter(VegData.summary.trt,
         variable == sppabb,
         Season != "Fall")

speciesname <-
  tempdf.summary$variable.sppname[[1]]
commonname <-
  tempdf.summary$variable.commonname[[1]]
axisname <- 
  tempdf.summary$variable.axislabel[[1]]
suffix <- "(Mean ? SE)"

p <-
  ggplot(filter(tempdf.summary, Plot <= 20), aes(Date, mean))+
  geom_line(aes(group = Treatment)) +
  geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
  geom_point(aes(color = Treatment), size = 3, show.legend = F) +
  scale_color_manual(values = cbPalette) +
  facet_grid(~ Plot, labeller = labeller(Plot = label_both)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(color = "Treatment",
       x = NULL,
       y = bquote(.(axisname)~.(suffix)))
p

ggsave(
  p,
  filename = paste0(
    getwd(),
    "/Graphs/For Presentations/All Plots/",
    "All Treatments Mean ",
    speciesname,
    " ",
    if_else(grepl(x = sppabb, pattern = "_m2"),
            "Density ",
            "Mean Height "),
    ".png"
  ),
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300
)

#### Plots without trt facets for graphs Of treatment means ####
# pct cover data
for (sppabb in spplist$pctcover.plotting) {#}
  tempdf.summary <-
    filter(VegData.summary.trt,
           variable == sppabb,
           Season != "Fall")
  
  speciesname <-
    as.character(unique(tempdf.summary$variable.sppname))
  commonname <-
    as.character(unique(tempdf.summary$variable.commonname))
  axisname <-
    as.character(unique(tempdf.summary$variable.axislabel))
  
  p <-
    ggplot(filter(tempdf.summary, Plot < 25), aes(Date, mean)) +
    geom_line(aes(group = Treatment)) +
    geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
    geom_point(aes(color = Treatment, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
    scale_color_manual(values = cbPalette) +
    facet_grid(~ Plot, labeller = labeller(Plot = label_both)) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    labs(title = "All Sites", 
         subtitle = paste0("Mean ", speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"), 
         color = "Treatment",
         x = NULL, 
         y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)"))
  
  ggsave(
    p,
    filename = paste0(
      getwd(),
      "/Graphs/For Presentations/All Plots/",
      "All Treatments Mean ",
      speciesname,
      " ",
      "Cover",
      " ",
      "no facets.png"
    ),
    width = 16,
    height = 9,
    units = "in",
    dpi = 300
  )
  
  q <- 
    ggplot(filter(tempdf.summary, Plot == -1 | Plot == 0 | Plot == 5), aes(Date, mean))+
    geom_line(aes(group = Treatment)) +
    geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
    geom_point(aes(color = Treatment, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
    scale_color_manual(values = cbPalette) +
    facet_grid(~ Plot, labeller = labeller(Plot = label_both)) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    labs(title = "All Sites", 
         subtitle = paste0("Mean ", speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"), 
         color = "Treatment",
         x = NULL, 
         y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)"))
  
  ggsave(
    q,
    filename = paste0(
      getwd(),
      "/Graphs/For Presentations/-1, 0, 5 Plots Only/",
      "All Treatments Mean ",
      speciesname,
      " ",
      "Cover",
      " ",
      "no facets.png"
    ),
    width = 16,
    height = 9,
    units = "in",
    dpi = 300
  )
}
# quant data
for (sppabb in spplist$quant.plotting) {#}
  
  tempdf.summary <-
    filter(
      VegData.summary.trt,
      variable == sppabb,
      Season != "Fall"
    )
  
  if (dim(tempdf.summary)[1] > 1) {
    
    speciesname <-
      tempdf.summary$variable.sppname[[1]]
    commonname <-
      tempdf.summary$variable.commonname[[1]]
    axisname <- 
      tempdf.summary$variable.axislabel[[1]]
    suffix <- "Mean ? SE"
    
    p <-
      ggplot(filter(tempdf.summary, Plot <= 20), aes(Date, mean))+
      geom_line(aes(group = Treatment)) +
      geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
      geom_point(aes(color = Treatment, shape = mean > 0), size = 3) +
      scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
      scale_color_manual(values = cbPalette) +
      facet_grid(~ Plot, labeller = labeller(Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste0("All Sites"),
           subtitle = paste0(speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ", if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height")))),
           color = "Season",
           x = NULL,
           y = bquote(atop(.(axisname),.(suffix)))
      )
    #SAVE#
    ggsave(
      p,
      filename = paste0(
        getwd(),
        "/Graphs/For Presentations/All Plots/",
        "All Sites Mean ",
        speciesname,
        " ",
        if_else(grepl(x = sppabb, pattern = "_m2"),
                "Density ",
                "Mean Height "),
        "no facets.png"
      ),
      width = 16,
      height = 9,
      units = "in",
      dpi = 300
    )
    
    
    q <-
      ggplot(filter(tempdf.summary, Plot == 0 | Plot == -1 | Plot == 5), aes(Date, mean)) +
      geom_line(aes(group = Treatment)) +
      geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
      geom_point(aes(color = Treatment, shape = mean > 0), size = 3) +
      scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
      scale_color_manual(values = cbPalette) +
      facet_grid(~ Plot, labeller = labeller(Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste0("All Sites"),
           subtitle = paste0(speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ", if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height")))),
           color = "Season",
           x = NULL,
           y = bquote(atop(.(axisname),.(suffix)))
      )
    
    ggsave(
      q,
      filename = paste0(
        getwd(),
        "/Graphs/For Presentations/-1, 0, 5 Plots Only/",
        "All Sites Mean ",
        speciesname,
        " ",
        if_else(grepl(x = sppabb, pattern = "_m2"),
                "Density",
                "Mean Height"),
        " no facets.png"
      ),
      width = 16,
      height = 9,
      units = "in",
      dpi = 300
    )
  }
}
