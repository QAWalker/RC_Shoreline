source(paste0(getwd(), "/helper scripts/sppnames.R"))

speciesname <- function(sppabb){
  sppnames$sppname[sppnames$abb==sppabb]
}

commonname <- function(sppabb){
  sppnames$commonname[sppnames$abb==sppabb]
}

axis.label <- function(sppabb){
  sppnames$axis.label[sppnames$abb==sppabb]
}

unitname <- function(sppabb){
  sppnames$unitname[sppnames$abb==sppabb]
}

unittype <- function(sppabb){
  sppnames$unit[sppnames$abb==sppabb]
}
