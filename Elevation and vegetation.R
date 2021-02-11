source(paste0(getwd(), "/helper scripts/species name helper functions.R"))
df <- VegData.melt %>% 
  filter(Season == "Summer") %>% 
  mutate(Transect = as.numeric(as.character(Transect))) %>% 
  left_join(elev,
            by = c("Year", "Site", "Treatment", "Transect", "Plot")) %>% 
  filter(!is.na(Corrected))

spp = "liveStem_m2"

for(spp in c(spplist$pctcover.plotting, spplist$quant.plotting)){#}
  temp.df <- filter(df, 
                    variable == spp)
  
  # suffix = "± SE" #Alt+0177
  
  # temp.df$Corrected2 <- temp.df$Corrected^2
  # quadmodel <- lm(value ~ Corrected + Corrected2, data = temp.df)
  # summary(quadmodel)
  
  # cvalues <- seq(range(temp.df$Corrected)[1], range(temp.df$Corrected)[2], by = 0.1)
  
  # pdct <- predict(quadmodel, list(Corrected=cvalues, Corrected2=cvalues^2))
  # plot(temp.df$Corrected, temp.df$value, pch = 16)
  # lines(cvalues, pdct, col = 'blue')
  
  # ggplot(temp.df, aes(Corrected, value))+
  #   # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, color = "black")+
  #   geom_point(size = 3, alpha = .75, aes(color = as.factor(Site)))+
  #   scale_color_manual(values = cbPalette)+
  #   labs(y = bquote(.(axis.label(spp))~.(tolower(unitname(spp)))),
  #        x = "Plot Elevation (m NAVD88)",
  #        color = NULL, 
  #        title = paste0(speciesname(spp), " (",commonname(spp),") ", tolower(unitname(spp)), " and plot elevation"))
  # 
  ggplot(temp.df, aes(Corrected, value))+
    # geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black")+
    geom_point(size = 3, alpha = .75, aes(color = as.factor(Site)))+
    facet_grid(Treatment~.)+
    scale_color_manual(values = cbPalette)+
    labs(y = bquote(italic(.(axis.label(spp)))~.(tolower(unitname(spp)))),
         x = "Plot Elevation (m NAVD88)",
         color = NULL, 
         title = bquote(italic(.(speciesname(spp)))~"("*.(commonname(spp))*")"~.(tolower(unitname(spp)))~"and plot elevation"))
  
  ggsave(filename = paste0(getwd(),"/Figures/Vegetation and Plot Elevation/Veg vs Elev scatters/", axis.label(spp), " ", unitname(spp)," and Elevation.png"),
         width = 10, height = 8, units = "in", dpi = 330)
  
}


