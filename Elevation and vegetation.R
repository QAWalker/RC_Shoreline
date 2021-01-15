df <- VegData.melt %>% 
  filter(Season == "Summer") %>% 
  mutate(Transect = as.numeric(as.character(Transect))) %>% 
  left_join(elev,
            by = c("SampTime","Year", "Site", "Treatment", "Transect", "Plot")) %>% 
  filter(!is.na(Corrected))
spp = "BMtotal"
for(spp in unlist(spplist)){#}
  temp.df <- filter(df, 
                    variable == spp)
  
  sppname = sppnames$sppname[sppnames$abb==spp] 
  commonname = sppnames$commonname[sppnames$abb==spp] 
  axisname = sppnames$axis.label[sppnames$abb==spp] 
  unit = sppnames$unit[sppnames$abb==spp][[1]]
  unitname = sppnames$unitname[sppnames$abb==spp]
  suffix = "± SE" #Alt+0177
  
  temp.df$Corrected2 <- temp.df$Corrected^2
  quadmodel <- lm(value ~ Corrected + Corrected2, data = temp.df)
  summary(quadmodel)
  
  cvalues <- seq(range(temp.df$Corrected)[1], range(temp.df$Corrected)[2], by = 0.1)
  
  pdct <- predict(quadmodel, list(Corrected=cvalues, Corrected2=cvalues^2))
  plot(temp.df$Corrected, temp.df$value, pch = 16)
  lines(cvalues, pdct, col = 'blue')
  
  ggplot(temp.df, aes(Corrected, value))+
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)+
    #geom_line(aes(group = Year))+
    geom_point(size = 3, alpha = .75, aes(color = as.factor(Site)))+
    facet_grid(Plot~Treatment)+
    # scale_color_manual(values = cbPalette)+
    labs(y = bquote(.(axisname)~.(tolower(unitname))),
         x = "Plot Elevation (m NAVD88)",
         color = NULL, 
         title = paste0(sppname, " (",commonname,") ", tolower(unitname), " and plot elevation"))
  
  ggsave(filename = paste0("R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/By Elevation/", axisname, " ", unitname," and Elevation.png"),
         width = 9, height = 6, units = "in", dpi = 330)
  
}

ggplot(elev.initial, aes(elev.initial, rel.elev, color = Site))+
  geom_point()

df <- left_join(elev, elev.initial)

oysterplots <- filter(VegData.melt, 
                      Site %in% c('PKS', 'NCMM', 'PI', 'HI', 'CL', 'MM'),
                      Plot == -1 |Plot == 0,
                      Treatment == "Nat", Feature != "Eroded",
                      variable %in% c("oyster", "oyster_live", "oyster_culch")) %>% 
  group_by(Site, Treatment, Feature, variable, Plot, SampTime) %>% 
  summarize(Date = mean(Date, na.rm = T), 
            mean = mean(value, na.rm = T), 
            sd = sd(value, na.rm = T), 
            n = length(value), 
            se = sd/sqrt(n)) %>% 
  ungroup()

ggplot(oysterplots, aes(Date, mean, color = variable))+
  geom_errorbar(aes(min = mean - se, max = mean +se))+
  geom_line()+
  geom_point()+
  facet_wrap(~paste(Site, Feature,Plot))+
  labs(y = "mean percent cover +/- SE")
ggsave(filename = "R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/Nat Sites -1, 0 Plot Oyster Cover.png", 
       width = 9, height = 6, units = "in", dpi = 330)

oysterplots <- filter(VegData.melt, 
                      Site %in% c('PKS', 'NCMM', 'PI', 'HI', 'CL', 'MM'),
                      Plot == -1 |Plot == 0,
                      Treatment == "Nat", Feature != "Eroded",
                      variable %in% c( "oyster_live", "oyster_culch")) %>% 
  group_by(Site.label, Treatment, Feature, variable, Plot) %>% 
  summarize(#Date = mean(Date, na.rm = T), 
            mean = mean(value, na.rm = T), 
            sd = sd(value, na.rm = T), 
            n = length(value), 
            se = sd/sqrt(n))

knitr::kable(head())

oysterplots <- oysterplots[order(oysterplots$Site, oysterplots$Date, oysterplots$Feature, oysterplots$Plot, oysterplots$variable, oysterplots$SampTime, na.last = T),names(oysterplots)!="Date"]

write.csv(oysterplots, "R:/CEE/RC shoreline/Data/Vegetation Data/Graphs/Nat Sites -1, 0 Plot Oyster Cover.csv", row.names = F)

filter(VegData.melt, 
       Site %in% c('CL'),
       Plot == -1 |Plot == 0,
       #Treatment == "Nat",
       value != 0,
       variable %in% c('oyster', "oyster_live", "oyster_culch")) %>% 
  select(SampTime, Site.label, Treatment, Feature, variable, Transect, Plot, value) %>% 
  knitr::kable()
