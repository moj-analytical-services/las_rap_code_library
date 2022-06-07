#' Labels quarters on the x-axis
#'
#' @description Labels quarters 1-4 as "Apr-Jun", "Jul-Sep", "Oct-Dec" and "Jan-Mar" respectively.
#'
#' @param fq Financial quarter
#
#' @examples
#' qtr_label("2021q4") outputs Jan-Mar
#' qtr_label("2022q1") outputs Apr-Jun
#' 
#' @return Either "Apr-Jun", "Jul-Sep", "Oct-Dec" or "Jan-Mar"
#'
#' @export

qtr_label <- function(fq){
  
  # Check that quarter is not null
  if  (any(is.null(fq))) {
    
    stop("Input to qtr_label is NULL")
    
  }
  
  # Check that quarter is not NA
  if (any(is.na(fq))) {
    
    stop("Input to qtr_label is NA")
    
  }
  
  # Check that quarter is a character
  if (any(!is.character(fq))) {
    
    stop("Please provide quarter as a character")
    
  }
  
  # Check that quarter is written in the correct format
  if (any(nchar(fq) != 6)) {
    
    stop("Financial quarters needs to be in the form of YYYYqq. Inputs such as 2021q3 or 2020q1 are accepted")
  }
  
  coverage <- c("Apr-Jun", "Jul-Sep", "Oct-Dec", "Jan-Mar")
  
  quarter <- as.numeric(substring(fq, 6))
  
  # Check that only q1, q2, q3 or q4 are input
  if (any(quarter %in% c(1:4) == FALSE)) {
    stop("Please choose between q1, q2, q3 or q4")
    
  }
  
  else{
    qtr_full <- base::ifelse(quarter %in% c(1:4),
                             paste(coverage[quarter]), NA)
    return(qtr_full)}
}
