#' Choose between automatic or manual source script.
#'
#' @description Function to choose between automatic or manual source script.
#'
#' @param auto Needs to be dataframe of a CSV from the alpha-legal-aid-statistics-team AWS bucket.
#
#' @examples
# source_r(auto = TRUE)
#' 
#' @return Quarters mentioned in bulletin.
#'
#' @export

source_r <- function(auto) {
  if(auto == TRUE){
    source("~/las_rap/source_automatic.R")
  }
  
  else if(auto == FALSE){
   source("~/las_rap/source_manual.R")
  }
  
  else{
    stop("Please input TRUE or FALSE")
  }
}