#' Convert NCVS categories to BB categories
#'
#' @param x 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
ncvs2bb <- function(x, output = "numeric"){
  #re-orders the levels in descending order and removes levels != 10:0. 
  x = factor(x, levels = c(10:0))
  
  levels(x) <- c(5, 5, 4, 3, 2, 2, 1, 0.5, 0.5, 0.1, 0)
  
  if (output == "numeric") {
    x <- as.numeric(as.character(x))
  }
  return(x)
}