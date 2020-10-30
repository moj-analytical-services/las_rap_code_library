#' Choosing a different shade of blue
#' 
#' @description Choices of different shades of blue
#'
#' @param x Different shade of blue. Can be an integer or a vector of integers between 1 and 9.
#' 1 represents the brightest shade and 9 represents the darkest shade of blue
#
#' @examples
# blue(2) gives you colour #deebf7, which is a light shade of blue
# blue(c(5, 7)) gives you colours #6baed6 and #2171b5
#' 
#' @return Different shade of blue.
#'
#' @export

blue <- function(x) {
  
  vector <- c('#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b')
  
  if (!is.numeric(x)){
    stop("x must be a number or numeric vector")
  }
  
  else { return(vector[x]) }
  
  }

