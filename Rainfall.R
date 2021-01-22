library(tidyverse)
library(reshape2)
library(lubridate)
library(readr)

surveyDates <- VegData %>% 
  group_by(Year, Site, SampTime) %>% 
  summarize(firstsurvey = range(ymd(paste(Year, Month, Day, sep = "-")))[1],
            lastsurvey = range(ymd(paste(Year, Month, Day, sep = "-")))[2]) %>% 
  ungroup()

# names <- names(read.csv("R:/CEE/RC shoreline/Data/Meterological Data/Beaufort Michael J Smith field.csv"))
# MetData.bft <-
#   read_csv(
#     "R:/CEE/RC shoreline/Data/Meterological Data/Beaufort Michael J Smith field.csv",
#     #col_types = rep("c", length(names)),
#     col_names = names,
#     na = c("", "-9999")
#   )[-1,]

MetData.bft <- read_excel(paste0(getwd(), "/Data/RC_Shoreline_Environmental_MSC.xlsx"), sheet = "BFT MJS Field") %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  select(-"STATION", -"NAME")

rainfall <- MetData.bft %>% 
  melt(id.vars = "DATE") %>% 
  filter(variable == "PRCP") %>% 
  mutate(YEAR = year(DATE), 
         MONTH = month(DATE)) %>% 
  select(DATE, YEAR, MONTH, RAIN.mm = value) %>% 
  
  as_tibble()

rainfall.monthly <- rainfall %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(RAIN.mm  = sum(RAIN.mm, na.rm = T)) %>% 
  ungroup()

rainfall.yearly <- rainfall %>% 
  group_by(YEAR) %>% 
  summarise(RAIN.mm  = sum(RAIN.mm, na.rm = T)) %>% 
  ungroup()

rainfall.growingseason <- rainfall %>% 
  filter(DATE >= ymd(paste(YEAR, 04, 01, sep = "-")) & DATE <= ymd(paste(YEAR, 09, 30, by = "-"))) %>% 
  group_by(YEAR) %>% 
  summarise(RAIN.mm  = sum(RAIN.mm, na.rm = T)) %>% 
  ungroup()

surveyDates$RAIN.mm <- sapply(X = surveyDates$firstsurvey, FUN = function(date){
  sum(rainfall$RAIN.mm[rainfall$DATE >= ymd(paste(year(date), 04, 01, sep = "-")) &
                              rainfall$DATE <= date], na.rm = T)
})

ggplot(rainfall.monthly, aes(ymd(paste(YEAR, MONTH, 01)), RAIN.mm/10/100))+
  geom_bar(stat = "identity") +
  theme_bw()

ggplot(rainfall.yearly, aes(YEAR, RAIN.mm/10/100))+
  geom_bar(stat = "identity") +
  theme_bw()+
  labs(y = "Rain (m)")

ggplot(rainfall.growingseason, aes(YEAR, RAIN.mm))+
  geom_bar(stat = "identity") +
  theme_bw()+
  labs(y = "Rain (m)")

ggplot(filter(rainfall.monthly, YEAR >= 2006), aes(ymd(paste(YEAR, MONTH, 01)), RAIN.mm/10/100))+
  geom_bar(stat = "identity")+
  labs(x = "Date",
       y = "Precipitation (m)")+
  theme_bw()

ggplot(filter(rainfall.yearly, YEAR >= 2006), aes(YEAR, RAIN.mm/10/100))+
  geom_bar(stat = "identity")+
  labs(x = "Date",
       y = "Precipitation (m)")+
  theme_bw()

#install.packages("toOrdinal")
library(toOrdinal)

ggplot(filter(surveyDates, Site %in% c("PKS", "PI", "NCMM"), substr(as.character(SampTime), 0, 2)=="SU"), aes(Year, RAIN.mm, fill = Site))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(
    aes(label = paste(month.name[month(firstsurvey)],  sapply(
      day(firstsurvey), toOrdinal
    ), sep = " ")),
    position = position_dodge(width = 0.9),
    angle = 90,
    hjust = 1.1,
    size = 2, 
    color = "white"
  )+
  scale_fill_grey()+
  scale_x_continuous(breaks = seq(2006, 2019, by = 2))+
  #geom_point(size = 3)+
  theme_bw()+
  labs(x = NULL, 
       y = "Rainfall (mm)", 
       title = "Growing Season Rainfall (April - Survey Date)",
       subtitle = "Recorded at Michael J. Smith Field, Beaufort")

ggsave(paste0(getwd(),"/Figures/Environmental/Growing Season Rainfall Beaufort.png"), 
       width = 8, height = 4.5, units = "in", dpi = 330)

