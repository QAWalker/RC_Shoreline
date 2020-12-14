#' Replace braun-blanquet values with  % cover value
#'
#' \code{bb2value} returns a vector or cover category %s
#'
#' reads in a vector of % cover categories and outputs the equivalent cover %
#' can choose if you want the high bounds of the category, the middle or the low
#' bounds and reorders levels so that the highest, 5 is first on plots
#' 
#' 
#' In bb cover scheme goes from 0 - 5 with some irregular categories for rare species.
#' The categories are defined as they are in Kenworth 1993 and Furman 2018
#' 
#' 0: 0%
#' 1: 0.2 - 5%
#' 2: 5 - 25%
#' 3: 25 - 50%
#' 4: 50 - 75%
#' 5: 75 - 100%
#'   
#' rare species categories are 'solitary' and 'few' (<5 shoots)
#' in some sources these categories are, respeciviely, "r" and" +" 
#' in others they are 0.1 and 0.5. 
#' this function reads in both and gives an output range of 0.01 - 0.04% and 0.04 - 0.2%
#' 
#' @param x a vector of bb cover categories.
#' @param method one of "high", "mid", or "low"
#' @return a vector of numerics
#' @export
bb2value <- function(x, method = "high", output = "numeric"){
  #re-orders the levels in descending order and removes levels != 10:0. 
  x = factor(x, levels = c(5:1, 0.5, 0.1, "+", "r", 0))
  method = tolower(method)
  if (method == "high") {
    levels(x) <- c(100, 75, 50, 25, 5, 0.2, 0.04, 0.2, 0.04, 0)
  }else{
    if(method == "mid"){
      levels(x) <- c(87.5, 62.5, 37.5, 15, 2.6, 0.125, 0.025, 0.125, 0.025, 0)
    }else{
      if(method == "low"){
        levels(x) <- c(75, 50, 25, 5, 0.2, 0.04, 0.01, 0.04, 0.01, 0)
      }else{
        stop("error: Unknown method. Choose 'high', 'mid', or 'low'")
      }
    }
  }
  if (output == "numeric") {
    x <- as.numeric(as.character(x))
  }
  return(x)
}
