rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
cbPalette <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9", "#999999")
odd <- function(x) x%%2 != 0 
even <- function(x) x%%2 == 0 

drought.pdsi <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/pdsi drought time_series.csv", stringsAsFactors = F) %>% 
  mutate(Date = mdy(paste(Month, 1, Year))) %>% select(Date, pdsi)

drought.spi <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/spi drought time_series.csv", stringsAsFactors = F) %>% 
  mutate(Date = mdy(Date))
  
hurricanes <- read.csv("R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data/NC hurricanes 2008 - 2018 from wiki.csv", stringsAsFactors = F) %>% 
  mutate(Date = mdy(Date)) %>% 
  select(Date, Name, Type, MaxStrength)

for (run in 1) {
  drought <- 1
  moddrought <- 1
  sevdrought <- 1
  
  for(i in 2:length(drought.pdsi$pdsi)){
    if((drought.pdsi$pdsi[[i]] > 0 & drought.pdsi$pdsi[[i-1]] < 0) | (drought.pdsi$pdsi[[i]] < 0 & drought.pdsi$pdsi[[i-1]] > 0)){
      drought <- c(drought, i)
    }
    if((drought.pdsi$pdsi[[i]] > -2 & drought.pdsi$pdsi[[i-1]] < -2) | (drought.pdsi$pdsi[[i]] < -2 & drought.pdsi$pdsi[[i-1]] > -2)){
      moddrought <- c(moddrought, i)
    }
    if((drought.pdsi$pdsi[[i]] > -4 & drought.pdsi$pdsi[[i-1]] < -4) | (drought.pdsi$pdsi[[i]] < -4 & drought.pdsi$pdsi[[i-1]] > -4)){
      sevdrought <- c(sevdrought, i) 
    }
  }
  rm(run)
}

pdsi.coords <- data.frame(xmin = drought.pdsi$Date[drought[odd(1:length(drought))]], 
                          xmax = drought.pdsi$Date[drought[even(1:length(drought))]],
                          ymin = -2,
                          ymax = 0, 
                          type = "Drought") %>%
  rbind(.,
        data.frame(xmin = drought.pdsi$Date[moddrought[odd(1:length(moddrought))]], 
                   xmax = drought.pdsi$Date[moddrought[even(1:length(moddrought))]],
                   ymin = -4,
                   ymax = -2, 
                   type = "Moderate Drought")) %>% 
  rbind(., 
        data.frame(xmin = drought.pdsi$Date[sevdrought[odd(1:length(sevdrought))]], 
                   xmax = drought.pdsi$Date[sevdrought[even(1:length(sevdrought))]],
                   ymin = -Inf,
                   ymax = -4, 
                   type = "Severe Drought"))


ggplot(drought.pdsi, aes(Date, pdsi))+
  geom_hline(
    data = data.frame(y = c(0,-2,-4)),
    aes(yintercept = y),
    color = "gray",
    size = .75) +
  geom_text(
    data = data.frame(
      x = dmy("15/1/2008"),
      y = c(0,-2,-4),
      label = c("Drought", "Moderate Drought", "Severe Drought")),
    aes(x = x, y = y, label = label),
    size = 6,
    color = "gray", 
    hjust = 0,
    vjust = 1)+
  geom_vline(
    data = filter(hurricanes, Type == "Hurricane"),
    aes(xintercept = Date),
    size = 1,
    color = "firebrick",
    alpha = 0.5) +
  geom_text_repel(
    data = filter(hurricanes, Type == "Hurricane"),
    aes(
      label = paste0(Name, "\n (cat. ", MaxStrength, ")"),
      x = Date,
      y = 5),
    hjust = 0,
    size = 3)+
  scale_size_continuous(breaks = 1:5, limits = c(1, 5)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(-6, 6), expand = FALSE)+
  scale_y_continuous(breaks = seq(-6, 6, by = 2))+
  labs(
    title = "Hurricanes and Drought Index",
    subtitle = "Central Coastal Plain NC 2008 - 2018", 
    x = NULL,
    y = "Palmer Drought Severity Index", 
    size = "Max Strength") +
  theme_bw()

ggsave(
  filename = "All Hurricanes and Drought Index 2008-2018.png", 
  path = "R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data", 
  width = 9, height = 6, dpi = 330, units = "in")


ggplot() +
  geom_rect(data = pdsi.coords,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            alpha = 1) +
  geom_rect(data = data.frame(xmin = dmy(01012011), xmax = dmy(01012015)),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "Accelerated SLR"),
            alpha = 0.5) +
  geom_rect(data = filter(hurricanes, Name == "Matthew" | Name == "Irene" | Name == "Florence"),
            aes(xmin = Date, xmax = Date + 14, ymin = -Inf, ymax = Inf, fill = "Major Hurricane"),
            alpha = 1) +
  geom_text_repel(data = filter(hurricanes, Name == "Matthew" | Name == "Irene" | Name == "Florence"),
                  aes(label = paste0(Name, "\n(cat. ", MaxStrength, ")"), x = Date, y = 5.5),
                  hjust = 1.5, size = 4) +
  #geom_vline(data = filter(hurricanes, type == "Hurricane"), aes(xintercept = date), size = 1, color = "black", alpha = 0.5) +
  #geom_text_repel(data = filter(hurricanes, type == "Hurricane"), aes(label = paste0(name, "\n (cat. ", maxstrength, ")"), x = date, y = 0), hjust = 0, size = 3)+
  geom_line(data = drought.pdsi, aes(Date, pdsi), size = 1)+
  coord_cartesian(ylim = c(-6, 6), expand = FALSE) +
  scale_x_date(limits = c(dmy(01012008), dmy(01012019))) +
  scale_fill_manual(
    breaks = c("Drought",
               "Moderate Drought",
               "Severe Drought",
               "Accelerated SLR", 
               "Major Hurricane"),
    values = c(
      "Drought" = cbPalette[[1]],
      "Moderate Drought" =  cbPalette[[4]],
      "Severe Drought" = cbPalette[[5]],
      "Accelerated SLR" = cbPalette[[8]], 
      "Major Hurricane" = cbPalette[[2]])) +
  labs(
    title = "Hurricanes, SLR, and Drought Index",
    subtitle = "Central Coastal Plain NC 2008 - 2018",
    x = NULL,
    y = "Palmer Drought Severity Index",
    fill = "Event") +
  theme_bw()

ggsave(
  filename = "Major Hurricanes, Drought and SLR 2008-2018 2.png", 
  path = "R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data", 
  width = 9, height = 6, dpi = 330, units = "in")

ggplot(drought.pdsi, aes(Date, pdsi))+
  geom_hline(
    data = data.frame(y = c(0,-2,-4)),
    aes(yintercept = y),
    color = "gray",
    size = .75
  ) +
  geom_text(
    data = data.frame(
      x = dmy("15/1/2008"),
      y = c(0,-2,-4),
      label = c("Drought", "Moderate Drought", "Severe Drought")
    ),
    aes(x = x, y = y, label = label),
    size = 6,
    color = "gray", 
    hjust = 0,
    vjust = 1
  )+
  geom_vline(data = filter(hurricanes, Type == "Hurricane"), aes(xintercept = Date), size = 1, color = "firebrick", alpha = 0.5) +
  geom_text_repel(data = filter(hurricanes, Type == "Hurricane"), aes(label = paste0(Name, "\n (cat. ", MaxStrength, ")"), x = Date, y = 5), hjust = 0, size = 3)+
  scale_size_continuous(breaks = 1:5, limits = c(1, 5)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(-6, 6), expand = FALSE)+
  scale_y_continuous(breaks = seq(-6, 6, by = 2))+
  labs(
    title = "Hurricanes and Drought Index",
    subtitle = "Central Coastal Plain NC 2008 - 2018", 
    x = NULL,
    y = "Palmer Drought Severity Index", size = "Max Strength"
  ) +
  theme_bw()

ggsave(
  filename = "All Hurricanes and Drought Index 2008-2018.png", 
  path = "R:/CEE/RC shoreline/Data/Vegetation Data/Drought, Hurricane, and Event Data", 
  width = 9, height = 6, dpi = 330, units = "in")

ggplot()+
  geom_point(data = drought.spi, aes(Date, spi), size = 1, color = "red")+
  geom_point(data = drought.pdsi, aes(Date, pdsi), size = 1)+
  theme_bw()

ggplot() +
  #geom_rect(data = pdsi.coords, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type), alpha = 1) +
  #geom_rect(data = data.frame(xmin = dmy(01012011), xmax = dmy(01012015)), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "Accelerated SLR"), alpha = 0.5) +
  #geom_rect(data = filter(hurricanes, name == "Matthew" | name == "Irene" | name == "Florence"), aes(xmin = date, xmax = date + 14, ymin = -Inf, ymax = Inf, fill = "Major Hurricane"), alpha = 1) +
  #geom_text_repel(data = filter(hurricanes, Name == "Matthew" | Name == "Irene" | Name == "Florence"), aes(label = paste0(Name, "\n(cat. ", MaxStrength, ")"), x = Date, y = 5.5), hjust = 1.5, size = 4) +
  #geom_vline(data = filter(hurricanes, type == "Hurricane"), aes(xintercept = date), size = 1, color = "black", alpha = 0.5) +
  #geom_text_repel(data = filter(hurricanes, type == "Hurricane"), aes(label = paste0(name, "\n (cat. ", maxstrength, ")"), x = date, y = 0), hjust = 0, size = 3)+
  geom_line(data = drought.spi, aes(Date, spi), size = 1)+
  #coord_cartesian(ylim = c(-6, 6), expand = FALSE) +
  #scale_x_date(limits = c(dmy(01012008), dmy(01012019))) +
  scale_fill_manual(
    breaks = c(
      "Drought",
      "Moderate Drought",
      "Severe Drought",
      "Accelerated SLR",
      "Major Hurricane"
    ),
    values = c(
      "Drought" = cbPalette[[1]],
      "Moderate Drought" =  cbPalette[[4]],
      "Severe Drought" = cbPalette[[5]],
      "Accelerated SLR" = cbPalette[[8]],
      "Major Hurricane" = cbPalette[[2]]
    )
  ) +
  labs(title = "Hurricanes, SLR, and Drought Index", subtitle = "Carteret County, NC", x = NULL, y = "SPI", fill = "Event") +
  theme_bw()
