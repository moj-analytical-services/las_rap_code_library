#' Check if las CSV used is the latest one.
#'
#' @description Function to check if a las CSV is the latest one.
#'
#' @param las_data Needs to be dataframe of a CSV from the alpha-legal-aid-statistics-team AWS bucket.
#
#' @examples
# up_to_date(las_data)
#' 
#' @return If the tables match, the output will come up as "TRUE". Otherwise this function confirms which columns have "TRUE" and "FALSE" matches.
#'
#' @export

up_to_date <- function(las_data) {
  latest <- lasrap::load_las(NULL)
  
  compare(las_data, latest)
}