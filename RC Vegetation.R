# master vegwrangling takes the file with the 06 and 07 data added to the 08-19
# data and this script will transform it to the right data based on the column
# survery method 

# rm(list = ls())
require(stringi)
library(tidyverse)
require(lubridate)
library(readxl)

# read in useful functions
source(paste0(getwd(), "/ncvs2value.R"))
source(paste0(getwd(), "/ncvs2bb.R"))
source(paste0(getwd(), "/bb2value.R"))

plotelevationlabeller <- function(string) {
  string <- paste("Plot Elevation: ", string, " m NAVD88", sep = "")
  string
}

#### read in and wrangle data ####
# read in data from working directory 
nms <- names(read_excel(paste0(getwd(), "/RC_Shoreline_Vegetation_Datamaster.xlsx"), n_max = 0, sheet = "VEG_DATABASE"))
ct <- ifelse(nms %in% c("other", "upland"), "text", "guess")
ct[which(nms %in% c(paste0("H", 1:10), "HMean"))] <- "numeric"

VegData <- readxl::read_excel(path = paste0(getwd(), "/RC_Shoreline_Vegetation_Datamaster.xlsx"),
                              sheet = "VEG_DATABASE", trim_ws = F, col_types = ct,
                              na = c("NA", "", ".", "?", "*", "NR", "*missing*", "missing value")) %>% 
  select(-c('other', 'upland')) %>%
  # filter(Site == "PKS" | Site == "PI" | Site == "NCMM") %>% 
  dplyr::mutate(SampTime = factor(SampTime,
                                  levels = unique(.$SampTime)[order(
                                    substr(
                                      x = unique(.$SampTime),
                                      start = 3,
                                      stop = 6
                                    ),
                                    substr(
                                      x = gsub("WI", "1",
                                               gsub("SP", "2",
                                                    gsub("SU", "3",
                                                         gsub("FA", "4",
                                                              unique(.$SampTime))
                                                    ))),
                                      start = 1,
                                      stop = 2
                                    ))]), 
                Date = mdy(paste(Month, Day, Year)),
                Transect = factor(Transect, levels = unique(as.numeric(Transect))[order(as.numeric(unique(Transect)))]),
                Plot = ifelse(as.numeric(NominalPlot) == as.numeric(Plot), as.numeric(Plot), as.numeric(NominalPlot)),
                Season = factor(substr(SampTime, 1, 2), levels = c("WI", "SP", "SU", "FA")),
                Season = fct_recode(Season, Winter = "WI", Spring = "SP", Summer = "SU", Fall = "FA")
  )%>%
  dplyr::select(Date, Month:SampTime, Season, CoverMethod, Site:Plot, everything(), -c(Notes:`Flag resolved?`)) %>% 
  arrange(SampTime, Site, Treatment, Feature, Transect, Plot) %>% 
  filter(T)

# add the 06 and 07 data to the 08-present data
VegData <- read.csv(paste0(getwd(), "/2006-2007 Veg Data.csv"), stringsAsFactors = F, strip.white = T, 
                        na.strings = c("NA", "", ".", "?", "*", "NR", "*missing*", "missing value")) %>% 
  select(-c('other', 'upland')) %>%
  filter(!is.na(Month)) %>% 
  rename(NominalPlot = PlotAlt) %>% 
  mutate(SampTime = factor(SampTime,
                           levels = unique(.$SampTime)[order(
                             substr(
                               x = unique(.$SampTime),
                               start = 3,
                               stop = 6
                               ),
                             substr(
                               x = gsub("WI", "1",
                                        gsub("SP", "2",
                                             gsub("SU", "3",
                                                  gsub("FA", "4",
                                                       unique(.$SampTime))
                                                  ))),
                               start = 1,
                               stop = 2
                               ))]), 
         Date = mdy(paste(Month, Day, Year)),
         Transect = factor(Transect, levels = unique(as.numeric(Transect))[order(as.numeric(unique(Transect)))]),
         Season = factor(substr(SampTime, 1, 2), levels = c("WI", "SP", "SU", "FA")),
         Season = fct_recode(Season, Winter = "WI", Spring = "SP", Summer = "SU", Fall = "FA"),
         CoverMethod = "BB", 
         NominalPlot = ifelse(is.na(NominalPlot), Plot, NominalPlot), 
         Plot = ifelse(as.numeric(NominalPlot) == as.numeric(Plot), as.numeric(Plot), as.numeric(NominalPlot))) %>%
  select(names(VegData)[which(names(VegData) %in% names(.))]) %>% 
  bind_rows(VegData) %>%
  arrange(SampTime, Site, Treatment, Feature, Transect, Plot)

##### Biomass #####
# this function returns the biomass in grams by inputting the height (H) in cm
# from Jim Morris, received Sept 2009
# I found this information in the metadata tab in R:/DCERP/Coastal Wetlands/Vegetation/MCBCL_Vegetation_Data_Master.xls
biomass <- function(H, a = 0.03467, b = -0.000903, c = 0.0000142){
  w = ifelse(!is.na(H), a*H + b*(H^2) + c*(H^3), NA)
  return(w)
}

HColNums <- which(names(VegData) %in% paste0("H", 1:10))

for (i in 1:10) {
  VegData[, paste0("BM",i)] <- biomass(VegData[,paste0("H", i)])
}

BMColNums <- which(names(VegData) %in% paste0("BM", 1:10))

for (i in 1:length(VegData$HMean)) {
  VegData$BMMean[i] <- mean(as.numeric(VegData[i, BMColNums]), na.rm = T)
}

VegData <- VegData %>% 
  mutate(BMtotal = liveStem_m2 * BMMean)

##### ID the columns of Data that we want for analysis #####
PctCoverColNums <-
  c((which(names(VegData) == "Acri")):(which(names(VegData) == "Snails_per_area") - 1))

QuantColNums <- which(names(VegData) %in% c("snails_m2", "liveStem_m2", "HMean", "BMMean", "BMtotal"))
 
SppColNums <- c(PctCoverColNums, QuantColNums)

## pass the percent cover values to a function that converts to the high limit of that category
# and to a function that converts the cvs values to a bb category
for (i in PctCoverColNums) {
  nm <- names(VegData)[i] 
  if (!paste0(nm ,"_category") %in% names(VegData)) {
    VegData[ ,paste0(nm , "_category")] <- VegData[i]
  }
  VegData[which(VegData$CoverMethod == "CVS"), i] <- 
    sapply(
      VegData[which(VegData$CoverMethod == "CVS"), i],
      function(d){ncvs2value(d, method = "mid", output = "numeric")}
    )
  VegData[which(VegData$CoverMethod == "BB"), i] <- 
    sapply(
      VegData[which(VegData$CoverMethod == "BB"), i],
      function(d){bb2value(d, method = "mid", output = "numeric")}
    )
  # convert to bb via our bb key
  VegData[which(VegData$CoverMethod == "CVS"), paste0(nm, "_bb")] <- 
    sapply(
      VegData[which(VegData$CoverMethod == "CVS"), paste0(nm, "_category")],
      function(d){ncvs2bb(d)}
    )
  VegData[which(VegData$CoverMethod == "BB"), paste0(nm, "_bb")] <- 
    VegData[which(VegData$CoverMethod == "BB"), paste0(nm, "_category")]
  
  rm(i, nm)
}

#Create a df used to lookup common names and plotting tools for each species
#note: won't work properly if new species are added to the dataset without updating the common name and axis label
source(paste0(getwd(), "/sppnames.R"))

#create a df for adding complete site names for plotting
sitenames <-
  data.frame(stringsAsFactors = F, 
             "abb" = c(
               "MM",
               "NCMM",
               "PI",
               "PKS",
               "MB",
               "ZB",
               "HI",
               "NRMC",
               "CL"
             ),
             "name" = c(
               "Middle Marsh",
               "N.C. Maritime Museum",
               "Pivers Island",
               "Pine Knoll Shores",
               "Masonboro",
               "Zeke's Basin",
               "Harkers Island",
               "Newport River Mill Creek",
               "Cape Lookout"
             )
  )


#### melt the data into a long form to better work with it in ggplot2 ####
require(reshape2)
VegData.melt <- VegData %>%
  melt(
    id.vars = 1:which(names(.) == "Plot"),
    measure.vars = SppColNums,
    factorsAsStrings = F
  ) 

VegData.melt <- dplyr::select(VegData.melt, starts_with("Site"),
                              Feature:Season,
                              starts_with("variable"),
                              everything())

#create a summary data.frame for means of each plot distance at each treatment, feature and site on each sample time
VegData.summary <- VegData.melt %>%
  filter(MarshType == "Fringing", 
         Feature %in% c('Behind', "Oyster", "None"),
         Site %in% c("PKS", "PI", "NCMM")) %>% 
  dplyr::group_by(Treatment, Plot, SampTime, Season, MarshType, variable) %>%
  dplyr::summarize(
    Date = mean(Date),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    se = ifelse(n <= 1, NA, sd / sqrt(n))) %>%
  ungroup() 

# save the data as .csv
# write_csv(x = VegData, path = paste0(getwd(), "/VegData.csv"))
# write_csv(x = VegData.melt, path = paste0(getwd(), "/VegData_melt.csv"))
# write_csv(x = VegData.summary, path = paste0(getwd(), "/VegData_summary.csv"))
# write_csv(x = sitenames, path = paste0(getwd(), "/sitenames.csv"))
# write_csv(x = sppnames, path = paste0(getwd(), "/sppnames.csv"))
