#' Replace NCVS values with  % cover value
#'
#' \code{ncvs2value} returns a vector or cover category %s
#'
#' reads in a vector of % cover categories and outputs the equivalent cover %
#' can choose if you want the high bounds of the category, the middle or the low
#' bounds and reorders levels so that the highest, 10 is first on plots
#' 
#' @param x a vector of ncvs cover categories.
#' @param method one of "high", "mid", or "low"
#' @return a vector of numerics
#' @export
ncvs2value <- function(x, method = "high", output = "numeric"){
  #re-orders the levels in descending order and removes levels != 10:0. 
  x = factor(x, levels = c(10:0))
  method = tolower(method)
  if (method == "high") {
    levels(x) <- c(100,95,75,50,25,10,5,2,1,0.1,0)
  }else{
    if(method == "mid"){
      levels(x) <- c(97.50, 85.00, 62.50, 37.50, 17.50, 7.50, 3.50, 1.50, 0.55, 0.05, 0)
    }else{
        if(method == "low"){
          levels(x) <- c(95, 75, 50, 25, 10, 5, 2, 1, 0.1, 0.01, 0)
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
