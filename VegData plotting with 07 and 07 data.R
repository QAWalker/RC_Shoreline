#rm(list = ls())

#run the data wrangling script to create our data frames for plotting
source("R:/CEE/RC shoreline/Data/Vegetation Data/R Analysis files/Master Veg Data Wrangling.R")

# set universal variables for plotting ####
# set theme for plotting 
theme_set(theme_bw() + theme(legend.key = element_rect(color = "white")))

#create color-blind pallette for plotting
cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")

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

#### Plotting all the Data -- pct cover categories ####
#loop through site, treatments, and features of those treatments to plot each species in the vector above
for(site in c("HI", "PKS", "PI", "NCMM")) {
  for (trt in unique(filter(masterVegData, Site == site)$Treatment)) {
    for (feature in unique(filter(masterVegData, Site == site, Treatment == trt)$Feature)) {
      #for(i in SppColNums){}
      for (sppabb in spplist$pctcover.plotting) {#}
        tempdf.melt <-
          filter(
            masterVegData.melt,
            Site == site,
            Treatment == trt,
            Feature == feature,
            variable == sppabb,
            Transect != "4a"
          )
        tempdf.summary <-
          filter(
            masterVegData.summary,
            Site == site,
            Treatment == trt,
            Feature == feature,
            Plot != 25,
            variable == sppabb
          )
        
        sitename <- sitenames$name[sitenames$abb == site]
        speciesname <- sppnames$sppname[sppnames$abb==sppabb]
        commonname <- sppnames$commonname[sppnames$abb==sppabb]
        axisname <- sppnames$axis.label[sppnames$abb==sppabb]
        
        p <- ggplot(tempdf.melt, aes(Date, value)) +
          geom_line(aes(group = paste(Transect, Plot, Treatment))) +
          geom_point(aes(color = Season, shape = value > 0), size = 3) +
          geom_errorbar(data = tempdf.summary, aes(x = Date, y = NULL, ymin = mean - se, ymax = mean + se), width = 120) +
          geom_line(data = tempdf.summary, aes(Date, mean, group = paste(Transect, Plot, Treatment))) +
          geom_point(data = tempdf.summary, aes(Date, mean, color = Season, shape = mean > 0), size = 3) +
          facet_grid(Transect ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
          scale_color_manual(values = c("Spring" = cbPalette[3], "Summer" = cbPalette[2], "Fall" = cbPalette[4], "Winter" = cbPalette[5]), drop = T, breaks = levels(tempdf.melt$Season)) +
          coord_cartesian(ylim = c(0, 100)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
          scale_shape_manual(values = c(1, 16), limits = c(F, T))+
          labs(title = paste0(sitename, " ", trt, if_else(tolower(feature) == "none", "", paste0("-", feature))),
               subtitle = paste0(speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"),
               color = "Season",
               x = NULL,
               y = paste(as.character(axisname), "percent cover\n(upper % of category)"),
               shape = "Cover > 0%")
        p
        
        ggsave(p, 
               filename = paste0(
                 "R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Site/All Data - With 06 and 07/All Plots/",
                 site, " ",
                 trt,
                 if_else(tolower(feature) == "none", " ", paste0("-", feature, " ")),
                 speciesname, " Cover points.png"),
               width = 16, height = 9, units = "in", dpi = 300)
        
        q <-
          ggplot(filter(tempdf.melt, Plot == "5" | Plot == "0" | Plot == "-1"), aes(Date, value)) +
          geom_line(aes(group = paste(Transect, Plot, Treatment))) +
          geom_point(aes(color = Season, shape = value > 0), size = 3) +
          geom_errorbar(data = filter(tempdf.summary, Plot == "5" | Plot == "0" | Plot == "-1"), aes(x = Date, y = NULL, ymin = mean - se, ymax = mean + se), width = 120) +
          geom_line(data = filter(tempdf.summary, Plot == "5" | Plot == "0" | Plot == "-1"), aes(Date, mean, group = paste(Transect, Plot, Treatment))) +
          geom_point(data = filter(tempdf.summary, Plot == "5" | Plot == "0" | Plot == "-1"), aes(Date, mean, color = Season, shape = mean > 0), size = 3) +
          facet_grid(Transect_f ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
          scale_color_manual(values = c("Spring" = cbPalette[3], "Summer" = cbPalette[2], "Fall" = cbPalette[4], "Winter" = cbPalette[5]), drop = T, breaks = levels(tempdf.melt$Season)) +
          scale_shape_manual(values = c(1, 16), limits = c(F, T))+
          coord_cartesian(ylim = c(0, 100)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
          labs(title = paste0(sitename, " ", trt, if_else(tolower(feature) == "none", "", paste0("-", feature))),
               subtitle = paste0(speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"),
               color = "Season",
               x = NULL,
               y = paste(as.character(axisname), "percent cover\n(upper % of category)"),
               shape = "Cover > 0%")
        
        ggsave(q,
               filename = paste0(
                 "R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Site/All Data - With 06 and 07/-1, 0, 5 Plots Only/",
                 site, " ", trt,
                 if_else(tolower(feature) == "none", " ", paste0("-", feature, " ")),
                 speciesname, " Cover points.png"), 
               width = 16, height = 9, units = "in", dpi = 300)
      }
    }
  }
}

#### Plotting all the Data -- quant data ####
# this differs from previous loop by removing the restriction on the y axis
# By Site, all data and a mean
# loop through site, treatments, and features of those treatments to plot each species in the vector above
unique(VegData$Site)
for(site in c("PKS", "PI", "NCMM")) {
  for (trt in unique(filter(masterVegData, Site == site)$Treatment)) {
    for (feature in unique(filter(masterVegData, Site == site, Treatment == trt)$Feature)) {
      for (sppabb in spplist$quant.plotting) {
        #}
        tempdf.melt <-
          filter(
            masterVegData.melt,
            Site == site,
            Treatment == trt,
            Feature == feature,
            variable == sppabb,
            Season != "Fall"
          )
        tempdf.summary <-
          filter(
            masterVegData.summary,
            Site == site,
            Treatment == trt,
            Feature == feature,
            Plot != 25,
            variable == sppabb,
            Season != "Fall"
          )
        
        sitename <- sitenames$name[sitenames$abb == site]
        speciesname <- sppnames$sppname[sppnames$abb==sppabb]
        commonname <- sppnames$commonname[sppnames$abb==sppabb]
        axisname <- sppnames$axis.label[sppnames$abb==sppabb]
        
        p <-
          ggplot(tempdf.melt, aes(Date, value)) +
          geom_line(aes(group = paste(Transect, Plot, Treatment))) +
          geom_point(aes(color = Season, shape = value > 0), size = 3) +
          geom_errorbar(
            data = tempdf.summary,
            aes(
              x = Date,
              y = NULL,
              ymin = mean - se,
              ymax = mean + se
            ),
            width = 120
          ) +
          geom_line(data = tempdf.summary, aes(Date, mean, group = paste(Transect, Plot, Treatment))) +
          geom_point(data = tempdf.summary,
                     aes(Date, mean, color = Season, shape = mean > 0),
                     size = 3) +
          facet_grid(Transect_f ~ Plot,
                     labeller = labeller(Transect = label_value, Plot = label_both)) +
          scale_color_manual(
            values = c(
              "Spring" = cbPalette[3],
              "Summer" = cbPalette[2],
              "Fall" = cbPalette[4],
              "Winter" = cbPalette[5]
            ),
            drop = T,
            breaks = levels(tempdf.melt$Season)
          ) +
          scale_shape_manual(values = c(1, 16), limits = c(F, T))+
          theme(axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 0.95
          )) +
          labs(
            title = paste0(
              sitename,
              " ",
              trt,
              if_else(tolower(feature) == "none", "", paste0("-", feature))
            ),
            subtitle = paste0(
              speciesname,
              if_else(
                speciesname == commonname,
                " ",
                paste0(
                  " (",
                  commonname,
                  ") ",
                  if_else(
                    grepl(x = sppabb, pattern = "_m2"),
                    "Density",
                    "Mean Height"
                  )
                )
              )
            ),
            color = "Season",
            x = NULL,
            y = bquote(.(axisname)),
            shape = "value > 0%"
          )
        
        ggsave(
          p,
          filename = paste0(
            "R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Site/All Data - With 06 and 07/All Plots/",
            site, " ", trt,
            if_else(tolower(feature) == "none", " ", paste0("-", feature, " ")),
            speciesname, " ",
            if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height"), ".png"),
          width = 16, height = 9, units = "in", dpi = 300)
        
        q <-
          ggplot(filter(tempdf.melt, Plot == "5" |
                          Plot == "0" | Plot == "-1"),
                 aes(Date, value)) +
          geom_line(aes(group = paste(Transect, Plot, Treatment))) +
          geom_point(aes(color = Season, shape = value > 0), size = 3) +
          geom_errorbar(
            data = filter(tempdf.summary, Plot == "5" |
                            Plot == "0" |
                            Plot == "-1"),
            aes(
              x = Date,
              y = NULL,
              ymin = mean - se,
              ymax = mean + se
            ),
            width = 120
          ) +
          geom_line(
            data = filter(tempdf.summary, Plot == "5" |
                            Plot == "0" |
                            Plot == "-1"),
            aes(Date, mean, group = paste(Transect, Plot, Treatment))
          ) +
          geom_point(
            data = filter(tempdf.summary, Plot == "5" |
                            Plot == "0" |
                            Plot == "-1"),
            aes(Date, mean, color = Season, shape = mean > 0),
            size = 3
          ) +
          facet_grid(Transect_f ~ Plot,
                     labeller = labeller(Transect = label_value, Plot = label_both)) +
          scale_shape_manual(values = c(1, 16), limits = c(F, T))+
          scale_color_manual(
            values = c(
              "Spring" = cbPalette[3],
              "Summer" = cbPalette[2],
              "Fall" = cbPalette[4],
              "Winter" = cbPalette[5]
            ),
            drop = T,
            breaks = levels(tempdf.melt$Season)
          ) +
          theme(axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 0.95
          )) +
          labs(
            title = paste0(
              sitename,
              " ",
              trt,
              if_else(tolower(feature) == "none", "", paste0("-", feature))
            ),
            subtitle = paste0(
              speciesname,
              if_else(
                speciesname == commonname,
                " ",
                paste0(
                  " (",
                  commonname,
                  ") ",
                  if_else(
                    grepl(x = sppabb, pattern = "_m2"),
                    "Density",
                    "Mean Height"
                  )
                )
              )
            ),
            color = "Season",
            x = NULL,
            y = bquote(.(axisname)),
            shape = "value > 0%"
          )
        
        ggsave(
          q,
          filename = paste0(
            "R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Site/All Data - With 06 and 07/-1, 0, 5 plots only/",
            site,
            " ",
            trt,
            if_else(tolower(feature) == "none", " ", paste0("-", feature, " ")),
            speciesname,
            " ",
            if_else(
              grepl(x = sppabb, pattern = "_m2"),
              "Density",
              "Mean Height"
            ),
            ".png"
          ),
          width = 16,
          height = 9,
          units = "in",
          dpi = 300
        )
      }
    }
  }
}

#### Plotting Just Means -- pct cover categories ####
# By site with just the means
for(site in c( "PI", "PKS", "NCMM")) {
  for (sppabb in spplist$pctcover.plotting) {#}
    
    tempdf.summary <-
      filter(
        masterVegData.summary,
        Site == site,
        variable == sppabb
      ) %>% 
      droplevels()
    
    sitename <- sitenames$name[sitenames$abb == site]
    speciesname <- sppnames$sppname[sppnames$abb==sppabb]
    commonname <- sppnames$commonname[sppnames$abb==sppabb]
    axisname <- sppnames$axis.label[sppnames$abb==sppabb]
    
    p <- ggplot(tempdf.summary, aes(Date, mean))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
      geom_point(aes(color = Season, shape = mean > 0), size = 3)+
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = c(
          "Spring" = cbPalette[3],
          "Summer" = cbPalette[2],
          "Fall" = cbPalette[4],
          "Winter" = cbPalette[5]
        ))+
      facet_grid(paste(Treatment, "-", Feature) ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
      coord_cartesian(ylim = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      labs(
        title = paste0(sitename, " "), 
        subtitle = paste0("Mean ", 
                          speciesname,
                          if_else(
                            speciesname == commonname, " ",
                            paste0(" (", commonname, ") ")), 
                          "Percent Cover"), 
        color = "Season",
        x = NULL,
        y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)")
      )
    
    ggsave(
      p,
      filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Site/Means - With 06 and 07/All Plots/",
                        site, " ",
                        "Mean ",
                        speciesname, " ",
                        "Cover",
                        ".png"),
      width = 16, height = 9, units = "in", dpi = 300)
    
    
    q <- ggplot(filter(tempdf.summary, Plot == -1 | Plot == 0 | Plot == 5), aes(Date, mean))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
      geom_point(aes(color = Season, shape = mean > 0), size = 3)+
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = c(
          "Spring" = cbPalette[3],
          "Summer" = cbPalette[2],
          "Fall" = cbPalette[4],
          "Winter" = cbPalette[5]
        ))+
      facet_grid(
        paste(Treatment, "-", Feature) ~ Plot,
        labeller = labeller(Transect = label_value, Plot = label_both)
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      labs(
        title = paste0(sitename, " "), 
        subtitle = paste0("Mean ", 
                          speciesname,
                          if_else(
                            speciesname == commonname, " ",
                            paste0(" (", commonname, ") ")), 
                          "Percent Cover"), 
        color = "Season",
        shape = "mean > 0",
        x = NULL,
        y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)")
      )
    
    ggsave(
      q,
      filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Site/Means - With 06 and 07/-1, 0, 5 plots only/",
                        site, " ",
                        "Mean ",
                        speciesname, " ",
                        "Cover",
                        ".png"),
      width = 16, height = 9, units = "in", dpi = 300)
  }
}

#### Plotting Just Means -- quant data ####
# By site with just the means
for(site in c("PI", "PKS", "NCMM")) {
  for (sppabb in spplist$quant.plotting) {#}
    
    tempdf.summary <-
      filter(
        masterVegData.summary,
        Site == site,
        variable == sppabb,
        Season != "Fall"
      )
    
    sitename <- sitenames$name[sitenames$abb == site]
    speciesname <- sppnames$sppname[sppnames$abb==sppabb]
    commonname <- sppnames$commonname[sppnames$abb==sppabb]
    axisname <- sppnames$axis.label[sppnames$abb==sppabb]
    
    suffix <- "Mean ? SE"
    
    p <- ggplot(tempdf.summary, aes(Date, mean))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
      geom_point(aes(color = Season, shape = mean > 0), size = 3)+
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = 
          c(
            "Spring" = cbPalette[3],
            "Summer" = cbPalette[2],
            "Fall" = cbPalette[4],
            "Winter" = cbPalette[5]
          )
      )+
      facet_grid(paste(Treatment, "-", Feature) ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      labs(
        title = sitename, 
        subtitle = paste0("Mean ",
                          speciesname,
                          if_else(
                            speciesname == commonname,
                            " ",
                            paste0(" (", commonname, ") ", if_else(
                              grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height"
                            ))
                          )), 
        color = "Season",
        shape = "mean > 0",
        x = NULL,
        y = bquote(atop(.(axisname),.(suffix)))
      )
    
    ggsave(
      p,
      filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data", "/Graphs/By Site/Means - With 06 and 07/All Plots/",
                        site, " ",
                        "Mean ",
                        speciesname, " ",
                        if_else(
                          grepl(x = sppabb, pattern = "_m2"), 
                          "Density",
                          "Mean Height"),
                        ".png"),
      width = 16, height = 9, units = "in", dpi = 300)
    
    
    q <- ggplot(filter(tempdf.summary, Plot == -1 | Plot == 0 | Plot == 5), aes(Date, mean))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
      geom_point(aes(color = Season, shape = mean > 0), size = 3)+
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = c(
          "Spring" = cbPalette[3],
          "Summer" = cbPalette[2],
          "Fall" = cbPalette[4],
          "Winter" = cbPalette[5]
        ))+
      facet_grid(paste(Treatment, "-", Feature) ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      labs(
        title = paste0(sitename, " "), 
        subtitle = paste0("Mean ", speciesname, if_else(
          speciesname == commonname, " ",
          paste0(" (", commonname, ") ", 
                 if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height")))), 
        color = "Season",
        x = NULL,
        y = bquote(atop(.(axisname),.(suffix)))
      )
    
    ggsave(
      q,
      filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data", "/Graphs/By Site/Means - With 06 and 07/-1, 0, 5 plots only/",
                        site, " ",
                        "Mean ",
                        speciesname, " ",
                        if_else(
                          grepl(x = sppabb, pattern = "_m2"), 
                          "Density",
                          "Mean Height"),
                        ".png"),
      width = 16, height = 9, units = "in", dpi = 300)
  }
}

### intratreatment plots ####
# By treatment, just means both quant and pct cover data
for (trt in unique(masterVegData.summary$Treatment)) {
  for (sppabb in spplist$pctcover.plotting) {#}
    
    tempdf.summary <-
      filter(
        masterVegData.summary,
        Treatment == trt,
        variable == sppabb, 
        Site == "HI" | Site == "NCMM" | Site == "PI" | Site == "MM" | Site == "PKS"
      )
    
    sitename <- sitenames$name[sitenames$abb == site]
    speciesname <- sppnames$sppname[sppnames$abb==sppabb]
    commonname <- sppnames$commonname[sppnames$abb==sppabb]
    axisname <- sppnames$axis.label[sppnames$abb==sppabb]
    
    suffix <- "Mean ? SE"
    
    p <- ggplot(tempdf.summary, aes(Date, mean))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
      geom_point(aes(color = Season, shape = mean > 0), size = 3)+
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = 
          c(
            "Spring" = cbPalette[3],
            "Summer" = cbPalette[2],
            "Fall" = cbPalette[4],
            "Winter" = cbPalette[5]
          )
      )+
      facet_grid(paste0(Site, " ", Treatment, "-", Feature) ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
      coord_cartesian(ylim = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      labs(
        title = paste0("All ", trt, " Sites"), 
        subtitle = paste0("Mean ", 
                          speciesname,
                          if_else(
                            speciesname == commonname, " ",
                            paste0(" (", commonname, ") ")), 
                          "Percent Cover"), 
        color = "Season",
        x = NULL,
        y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)")
      )
    
    ggsave(
      p,
      filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data", "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
                        "All ",
                        trt, " Sites Mean ",
                        speciesname, " ",
                        "Cover",
                        ".png"),
      width = 16, height = 9, units = "in", dpi = 300)
    
    q <- ggplot(filter(tempdf.summary, Plot == -1 | Plot == 0 | Plot == 5), aes(Date, mean))+
      geom_line()+
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
      geom_point(aes(color = Season, shape = mean > 0), size = 3)+
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = c(
          "Spring" = cbPalette[3],
          "Summer" = cbPalette[2],
          "Fall" = cbPalette[4],
          "Winter" = cbPalette[5]
        ))+
      facet_grid(paste0(Site, " ", Treatment, "-", Feature) ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
      coord_cartesian(ylim = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      labs(
        title = paste0("All ", trt, " Sites"), 
        subtitle = paste0("Mean ", 
                          speciesname,
                          if_else(
                            speciesname == commonname, " ",
                            paste0(" (", commonname, ") ")), 
                          "Percent Cover"), 
        color = "Season",
        x = NULL,
        y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)")
      )
    
    ggsave(
      q,
      filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data", "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 Plots Only/",
                        "All ",
                        trt, " Sites Mean ",
                        speciesname, " ",
                        "Cover",
                        ".png"),
      width = 16, height = 9, units = "in", dpi = 300)
  }
  
  for (sppabb in spplist$quant.plotting) {#}
    tempdf.summary <-
      filter(
        masterVegData.summary,
        Treatment == trt,
        variable == sppabb,
        Site == "HI" | Site == "NCMM" | Site == "PI" | Site == "MM" | Site == "PKS",
        Season!="Fall"
      )
    
    if(dim(tempdf.summary)[1]>1){
      sitename <- sitenames$name[sitenames$abb == site]
      speciesname <- sppnames$sppname[sppnames$abb==sppabb]
      commonname <- sppnames$commonname[sppnames$abb==sppabb]
      axisname <- sppnames$axis.label[sppnames$abb==sppabb]
      
      suffix <- "Mean ? SE"
      
      
      p <- ggplot(filter(tempdf.summary, Season != "Fall"), aes(Date, mean))+
        geom_line()+
        geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
        geom_point(aes(color = Season, shape = mean > 0), size = 3)+
        scale_shape_manual(values = c(1, 16), limits = c(F, T))+
        scale_color_manual(
          values = c(
            "Spring" = cbPalette[3],
            "Summer" = cbPalette[2],
            "Fall" = cbPalette[4],
            "Winter" = cbPalette[5]
          ))+
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = paste0("All ", trt, " Sites"),
             subtitle = paste0("Mean ", speciesname, if_else(
               speciesname == commonname, " ",
               paste0(" (", commonname, ") ", 
                      if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height")))), 
             color = "Season",
             x = NULL,
             y = bquote(.(axisname)~.(suffix)))
      
      if (length(unique(tempdf.summary$Feature)) > 2) {
        p <-
          p + facet_grid(
            paste(Site, Treatment, "-", Feature) ~ Plot,
            labeller = labeller(Transect = label_value, Plot = label_both)
          )
      } else{
        p <-
          p + facet_grid(paste(Site, Treatment) ~ Plot)#, labeller = labeller(Transect = label_value, Plot = label_both))
      }
      
      ggsave(
        p,
        filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data",
                          "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
                          "All ",
                          trt, " Sites Mean ",
                          speciesname, " ",
                          if_else(
                            grepl(x = sppabb, pattern = "_m2"), 
                            "Density",
                            "Mean Height"),
                          ".png"),
        width = 16, height = 9, units = "in", dpi = 300)
      
      
      q <- ggplot(filter(tempdf.summary, Season != "Fall", Plot==0|Plot==-1|Plot==5),
                  aes(Date, mean))+
        geom_line()+
        geom_errorbar(aes(min = mean - se, max = mean + se), width = 120)+
        geom_point(aes(color = Season, shape = mean > 0), size = 3)+
        scale_shape_manual(values = c(1, 16), limits = c(F, T))+
        scale_color_manual(
          values = c(
            "Spring" = cbPalette[3],
            "Summer" = cbPalette[2],
            "Fall" = cbPalette[4],
            "Winter" = cbPalette[5]
          ))+
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = paste0("All ", trt, " Sites"),
             subtitle = paste0("Mean ", speciesname, if_else(
               speciesname == commonname, " ",
               paste0(" (", commonname, ") ", 
                      if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height")))), 
             color = "Season",
             x = NULL,
             y = bquote(.(axisname)~.(suffix)))
      
      if (length(unique(tempdf.summary$Feature)) > 1) {
        q <-
          q + facet_grid(
            paste(Site, trt, "-", Feature) ~ Plot,
            labeller = labeller(Transect = label_value, Plot = label_both)
          )
      } else{
        q <-
          q + facet_grid(paste(Site, trt) ~ Plot,
                         labeller = labeller(Transect = label_value, Plot = label_both))
      }
      
      ggsave(q,
             filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data",
                               "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 plots only/",
                               "All ",
                               trt, " Sites Mean ",
                               speciesname, " ",
                               if_else(
                                 grepl(x = sppabb, pattern = "_m2"), 
                                 "Density",
                                 "Mean Height"),
                               ".png"),
             width = 16, height = 9, units = "in", dpi = 300)
    }
  }
}

#### Graphs Of treatment-feature means ####
# pct cover data
for (sppabb in spplist$pctcover.plotting) {#}
  tempdf.summary <- filter(masterVegData.summary.trt.ftr, variable == sppabb)
  
  sitename <- sitenames$name[sitenames$abb == site]
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  p <-
    ggplot(filter(tempdf.summary, Plot < 25), aes(Date, mean)) +
    geom_line() +
    geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
    geom_point(aes(color = Season, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c(1, 16), limits = c(F, T))+
    scale_color_manual(values = c("Spring" = cbPalette[3], "Summer" = cbPalette[2], "Fall" = cbPalette[4], "Winter" = cbPalette[5])) +
    facet_grid(paste0(Treatment, "-", Feature) ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    labs(title = "All Sites", 
         subtitle = paste0("Mean ", speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"),
         color = "Season",
         x = NULL,
         y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)"))
  
  ggsave(
    p,
    filename = paste0(
      "R:/CEE/RC shoreline/Data/Vegetation Data",
      "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
      "All Treatments and Features Mean ",
      speciesname,
      " ",
      "Cover",
      ".png"
    ),
    width = 16,
    height = 9,
    units = "in",
    dpi = 300
  )
  
  q <-
    ggplot(filter(tempdf.summary, Plot == -1 | Plot == 0 | Plot == 5), aes(Date, mean))+
    geom_line() +
    geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
    geom_point(aes(color = Season, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c(1, 16), limits = c(F, T))+
    scale_color_manual(
      values = c(
        "Spring" = cbPalette[3],
        "Summer" = cbPalette[2],
        "Fall" = cbPalette[4],
        "Winter" = cbPalette[5]
      )
    ) +
    facet_grid(
      paste0(Treatment, "-", Feature) ~ Plot,
      labeller = labeller(Transect = label_value, Plot = label_both)
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 0.95
    )) +
    labs(
      title = "All Sites",
      subtitle = paste0(
        "Mean ",
        speciesname,
        if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")),
        "Percent Cover"
      ),
      color = "Season",
      x = NULL,
      y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)")
    )
  
  ggsave(
    q,
    filename = paste0(
      "R:/CEE/RC shoreline/Data/Vegetation Data",
      "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 Plots Only/",
      "All Treatments and Features Mean ",
      speciesname,
      " ",
      "Cover",
      ".png"
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
      masterVegData.summary.trt.ftr,
      variable == sppabb,
      Season != "Fall"
    )
  
  if (dim(tempdf.summary)[1] > 1) {
    
    sitename <- sitenames$name[sitenames$abb == site]
    speciesname <- sppnames$sppname[sppnames$abb==sppabb]
    commonname <- sppnames$commonname[sppnames$abb==sppabb]
    axisname <- sppnames$axis.label[sppnames$abb==sppabb]
    
    suffix <- "Mean ? SE"
    
    p <-
      ggplot(tempdf.summary, aes(Date, mean)) +
      geom_line() +
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
      geom_point(aes(color = Season, shape = mean > 0), size = 3) +
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = 
          c(
            "Spring" = cbPalette[3],
            "Summer" = cbPalette[2],
            "Fall" = cbPalette[4],
            "Winter" = cbPalette[5]
          )
      ) +
      facet_grid(paste0(Treatment, "-", Feature) ~ Plot,
                 labeller = labeller(Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = paste0("All Sites"),
        subtitle = paste0(
          speciesname,
          if_else(
            speciesname == commonname,
            " ",
            paste0(" (",
                   commonname,
                   ") ",
                   if_else(
                     grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height"
                   ))
          )
        ),
        color = "Season",
        x = NULL,
        y = bquote(.(axisname)~.(suffix))
      )
    
    ggsave(
      p,
      filename = paste0(
        "R:/CEE/RC shoreline/Data/Vegetation Data",
        "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
        "All Sites and Features Mean ",
        speciesname,
        " ",
        if_else(grepl(x = sppabb, pattern = "_m2"),
                "Density",
                "Mean Height"),
        ".png"
      ),
      width = 16,
      height = 9,
      units = "in",
      dpi = 300
    )
    
    
    q <-
      ggplot(filter(tempdf.summary, Plot == 0 | Plot == -1 | Plot == 5), aes(Date, mean)) +
      geom_line() +
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
      geom_point(aes(color = Season, shape = mean > 0), size = 3) +
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = c(
          "Spring" = cbPalette[3],
          "Summer" = cbPalette[2],
          "Fall" = cbPalette[4],
          "Winter" = cbPalette[5]
        )
      ) +
      facet_grid(paste0(Treatment, "-", Feature) ~ Plot,
                 labeller = labeller(Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = paste0("All Sites"),
        subtitle = paste0(
          speciesname,
          if_else(
            speciesname == commonname,
            " ",
            paste0(" (",
                   commonname,
                   ") ",
                   if_else(
                     grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height"
                   )
            )
          )
        ),
        color = "Season",
        x = NULL,
        y = bquote(.(axisname)~.(suffix))
      )
    
    ggsave(
      q,
      filename = paste0(
        "R:/CEE/RC shoreline/Data/Vegetation Data",
        "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 plots only/",
        "All Sites and Features Mean ",
        speciesname,
        " ",
        if_else(grepl(x = sppabb, pattern = "_m2"),
                "Density",
                "Mean Height"),
        ".png"
      ),
      width = 16,
      height = 9,
      units = "in",
      dpi = 300
    )
  }
}

#### Graphs Of treatment means ####
## removed eroded and sill adjacent 
# pct cover data
for (sppabb in spplist$pctcover.plotting) {#}
  tempdf.summary <-
    filter(masterVegData.summary.trt,
           variable == sppabb)
  
  sitename <- sitenames$name[sitenames$abb == site]
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  p <-
    ggplot(filter(tempdf.summary, Plot < 25), aes(Date, mean)) +
    geom_line() +
    geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
    geom_point(aes(color = Season, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
    scale_color_manual(values = c("Spring" = cbPalette[3], "Summer" = cbPalette[2], "Fall" = cbPalette[4], "Winter" = cbPalette[5])) +
    facet_grid(Treatment ~ Plot, labeller = labeller(Plot = label_both)) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    labs(title = "All Sites", subtitle = paste0("Mean ", speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"), color = "Season", x = NULL, y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)"))
  
  ggsave(
    p,
    filename = paste0(
      "R:/CEE/RC shoreline/Data/Vegetation Data",
      "/Graphs/By Treatment/Means/All Plots/",
      "All Treatments Mean ",
      speciesname,
      " ",
      "Cover",
      ".png"
    ),
    width = 16,
    height = 9,
    units = "in",
    dpi = 300
  )
  
  q <- 
    ggplot(filter(tempdf.summary, Plot == -1 | Plot == 0 | Plot == 5), aes(Date, mean))+
    geom_line() +
    geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
    geom_point(aes(color = Season, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16))+
    scale_color_manual(values = c("Spring" = cbPalette[3], "Summer" = cbPalette[2], "Fall" = cbPalette[4], "Winter" = cbPalette[5])) +
    facet_grid(Treatment ~ Plot, labeller = labeller(Transect = label_value, Plot = label_both)) +
    coord_cartesian(ylim = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    labs(title = "All Sites", subtitle = paste0("Mean ", speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ")), "Percent Cover"), color = "Season", x = NULL, y = paste(axisname, "Percent Cover (Mean ? SE)\n(upper % of category)"))
  
  ggsave(
    q,
    filename = paste0(
      "R:/CEE/RC shoreline/Data/Vegetation Data",
      "/Graphs/By Treatment/Means/-1, 0, 5 Plots Only/",
      "All Treatments Mean ",
      speciesname,
      " ",
      "Cover",
      ".png"
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
      masterVegData.summary.trt.ftr,
      variable == sppabb,
      Season != "Fall"
    )
  
  if (dim(tempdf.summary)[1] > 1) {
    
    sitename <- sitenames$name[sitenames$abb == site]
    speciesname <- sppnames$sppname[sppnames$abb==sppabb]
    commonname <- sppnames$commonname[sppnames$abb==sppabb]
    axisname <- sppnames$axis.label[sppnames$abb==sppabb]
    
    suffix <- "Mean ? SE"
    
    p <-
      ggplot(tempdf.summary, aes(Date, mean)) +
      geom_line() +
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
      geom_point(aes(color = Season, shape = mean > 0), size = 3) +
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = 
          c(
            "Spring" = cbPalette[3],
            "Summer" = cbPalette[2],
            "Fall" = cbPalette[4],
            "Winter" = cbPalette[5]
          )
      ) +
      facet_grid(paste0(Treatment, "-", Feature) ~ Plot,
                 labeller = labeller(Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = paste0("All Sites"),
        subtitle = paste0(
          speciesname,
          if_else(
            speciesname == commonname,
            " ",
            paste0(" (",
                   commonname,
                   ") ",
                   if_else(
                     grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height"
                   ))
          )
        ),
        color = "Season",
        x = NULL,
        y = bquote(.(axisname)~.(suffix))
      )
    
    ggsave(
      p,
      filename = paste0(
        "R:/CEE/RC shoreline/Data/Vegetation Data",
        "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
        "All Sites Mean ",
        speciesname,
        " ",
        if_else(grepl(x = sppabb, pattern = "_m2"),
                "Density",
                "Mean Height"),
        ".png"
      ),
      width = 16,
      height = 9,
      units = "in",
      dpi = 300
    )
    
    
    q <-
      ggplot(filter(tempdf.summary, Plot == 0 | Plot == -1 | Plot == 5), aes(Date, mean)) +
      geom_line() +
      geom_errorbar(aes(min = mean - se, max = mean + se), width = 120) +
      geom_point(aes(color = Season, shape = mean > 0), size = 3) +
      scale_shape_manual(values = c(1, 16), limits = c(F, T))+
      scale_color_manual(
        values = c(
          "Spring" = cbPalette[3],
          "Summer" = cbPalette[2],
          "Fall" = cbPalette[4],
          "Winter" = cbPalette[5]
        )
      ) +
      facet_grid(paste0(Treatment, "-", Feature) ~ Plot,
                 labeller = labeller(Plot = label_both)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = paste0("All Sites"),
        subtitle = paste0(
          speciesname,
          if_else(
            speciesname == commonname,
            " ",
            paste0(" (",
                   commonname,
                   ") ",
                   if_else(
                     grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height"
                   )
            )
          )
        ),
        color = "Season",
        x = NULL,
        y = bquote(.(axisname)~.(suffix))
      )
    
    ggsave(
      q,
      filename = paste0(
        "R:/CEE/RC shoreline/Data/Vegetation Data",
        "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 plots only/",
        "All Sites Mean ",
        speciesname,
        " ",
        if_else(grepl(x = sppabb, pattern = "_m2"),
                "Density",
                "Mean Height"),
        ".png"
      ),
      width = 16,
      height = 9,
      units = "in",
      dpi = 300
    )
  }
}

#### Plots without trt facets for graphs Of treatment means ####
# pct cover data
for (sppabb in spplist$pctcover.plotting) {#}
  tempdf.summary <-
    filter(masterVegData.summary.trt,
           variable == sppabb,
           Season != "Fall")
  
  sitename <- sitenames$name[sitenames$abb == site]
  speciesname <- sppnames$sppname[sppnames$abb==sppabb]
  commonname <- sppnames$commonname[sppnames$abb==sppabb]
  axisname <- sppnames$axis.label[sppnames$abb==sppabb]
  
  p <-
    ggplot(filter(tempdf.summary, Plot < 25), aes(Date, mean)) +
    geom_line(aes(group = Treatment)) +
    geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
    geom_point(aes(color = Treatment, shape = mean > 0), size = 3) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
    scale_color_manual(values = cbPalette) +
    facet_wrap(~ Plot, labeller = labeller(Plot = label_both)) +
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
      "R:/CEE/RC shoreline/Data/Vegetation Data",
      "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
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
      "R:/CEE/RC shoreline/Data/Vegetation Data",
      "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 Plots Only/",
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
      masterVegData.summary.trt,
      variable == sppabb,
      Season != "Fall"
    )
  
  if (dim(tempdf.summary)[1] > 1) {
    
    sitename <- sitenames$name[sitenames$abb == site]
    speciesname <- sppnames$sppname[sppnames$abb==sppabb]
    commonname <- sppnames$commonname[sppnames$abb==sppabb]
    axisname <- sppnames$axis.label[sppnames$abb==sppabb]
    
    suffix <- "Mean ? SE"
    
    p <-
      ggplot(filter(tempdf.summary, Plot <= 20), aes(Date, mean))+
        geom_line(aes(group = Treatment)) +
        geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120) +
        geom_point(aes(color = Treatment, shape = mean > 0), size = 3) +
        scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
        scale_color_manual(values = cbPalette) +
        facet_wrap(~ Plot, labeller = labeller(Plot = label_both)) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = paste0("All Sites"),
             subtitle = paste0(speciesname, if_else(speciesname == commonname, " ", paste0(" (", commonname, ") ", if_else(grepl(x = sppabb, pattern = "_m2"), "Density", "Mean Height")))),
             color = "Treatment",
             x = NULL,
             y = bquote(atop(.(axisname),.(suffix)))
        )
    #p#
    ggsave(
      p,
      filename = paste0(
        "R:/CEE/RC shoreline/Data/Vegetation Data",
        "/Graphs/By Treatment/Means - With 06 and 07/All Plots/",
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
        "R:/CEE/RC shoreline/Data/Vegetation Data",
        "/Graphs/By Treatment/Means - With 06 and 07/-1, 0, 5 plots only/",
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

spps = c("Salt", "liveStem_m2", "HMean")

tempdf.summary <- masterVegData.summary.trt %>% 
  filter(variable %in% spps, 
         Season == "Summer", 
         Plot <= 20) %>% 
  select(-c("variable.sppname", "variable.commonname", "variable.axislabel"))

tempdf.summary <- tempdf.summary[!(tempdf.summary$variable=="liveStem_m2"&tempdf.summary$SampTime=="SU2008"),]

tempdf.summary <- left_join(tempdf.summary, sppnames, by =c("variable" = "abb")) 

tempdf.summary <- mutate(tempdf.summary, 
                         facet_lab = paste("Mean ", tolower(unitname), "\n", "(", unit, " ? SE", ")", sep = ""))

tempdf.summary <- mutate(tempdf.summary, facet_lab = paste(axis.label, tolower(unitname), "\n", unit, "? SE", sep = "~"))

string <- tempdf.summary$facet_lab[[1]]

make_label <- function(string){
  s <- unlist(strsplit(string, split = "@"))
  bquote("Mean "*.(s[1])~.(tolower(s[2]))~.(s[3])*"("*.(s[4])~.(s[5])*")")
}

plot_labeller <- function(value) {
  do.call(expression, lapply(levels(value), make_label))
}
# rm(multivaraxislabs)
# spp = "Salt"
# sppname = sppnames$sppname[sppnames$abb==spp] 
# commonname = sppnames$commonname[sppnames$abb==spp] 
# axisname = sppnames$axis.label[sppnames$abb==spp] 
# unit = sppnames$unit[sppnames$abb==spp][[1]]
# unitname = sppnames$unitname[sppnames$abb==spp]
# <- suffix = "? SE"
  
ggplot(tempdf.summary, aes(Date, mean, color = Treatment))+
  geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120, color = "black") +
  geom_line(color = "black", aes(group = Treatment))+
  geom_point(size = 2)+
  facet_grid(facet_lab~Plot, scales = "free_y", switch = "y")+
  labs(y = NULL, 
       x = NULL, 
       title = "S. alterniflora density, height and percent cover")+
  scale_color_manual(values = cbPalette[c(3,8)])+
  theme(strip.background.y = element_blank(), 
        strip.placement = "outside")

ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/S. alterniflora density, height and percent cover through time.png"),
       width = 10, height = 6, units = "in", dpi = 330)

unique(tempdf.summary$variable)

ggplot(filter(masterVegData.summary.trt, Plot == 1), aes(Date, mean, color = Treatment))+
  geom_errorbar(aes(min = mean - se, max = mean + se, group = Treatment), width = 120, color = "black") +
  geom_line(color = "black", aes(group = Treatment))+
  geom_point(size = 2)+
  facet_grid(variable~Plot, scales = "free_y")+
  scale_color_manual(values = cbPalette[c(3,8)])+
  theme(strip.background.y = element_blank(), 
        strip.placement = "outside")
