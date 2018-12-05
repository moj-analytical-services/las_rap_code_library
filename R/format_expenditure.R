#' @title Format expenditure
#'
#' @description Convert value to amount for publication
#'
#' @details Generic method to convert value into a £m (also could look at amount and do k, £ so could be expanded)
#'
#' @param value An amount of expenditure in £'s
#'
#' @return A character object.
#'
#' @examples
#'
#' format_expenditure(5100000) #returns £5.1m
#'
#' @export



format_expenditure <- function(value) {

  tryCatch({
    
    # Check that only one value is passed to format_perc() at a time and raise
    # an error otherwise.
    
    if (length(value) > 1) {
      
      stop(
        "Input to format_expenditure is not a single value. ",
        "Most likely you have tried to pass a vector, ",
        "list, or df to format_expenditure()",
        call. = FALSE
      )
      
    } else if  (is.null(value)) {
      
      # Check that value is not null, and raise an error if it is
      
      stop("Input to format_expenditure is NULL", call. = FALSE)
      
    } else if (is.na(value)) {
      
      # Check that value is not null, and raise and error if it is
      
      stop("Input to format_expenditure is NA", call. = FALSE)
      
    } else {
      
      # If checks of function pass, then run the main body of the function, and
      # return and output.
      
      value <- paste("£",(round(abs(as.numeric(value)/1000000), 1)), "m", sep = "")
      return(value)
      
      
    }
  }
  , warning = function(war){
    warning(war)
    
  }
  , error = function(err){
    
    err$message <- paste("While formatting expenditure", err, sep = " ")
    stop(err)
  })
}
