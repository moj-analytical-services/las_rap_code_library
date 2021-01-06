#' Choose between automatic or manual source script.
#'
#' @description You can choose between automatic or manual source script. The source script collects the name and coverage of the quarters mentioned in the bulletin.
#'
#' @param auto Input TRUE if you want to run automatic source script or FALSE if you want to run manual source script.
#
#' @examples
# source_r(auto = TRUE)
# source_r(auto = FALSE)
#' 
#' @return Name and coverage of quarters mentioned in bulletin.
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
    stop("Please input TRUE or FALSE. This function is case-sensitive.")
  }
}