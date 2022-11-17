# decimal_to_word

#

#' Takes a deminal and returns a string describing the decimal.

#' @param decimal The proportion you want describing in words 
#' @examples
#' word_to_decimal(0.3567) #retunrs "around a third "
#' @export
#'

decimal_to_word <- function(decimal){
  tryCatch({
    # Check that only one value is passed to format_perc() at a time and raise
    # an error otherwise.
    if (decimal > 1 | decimal < 0) {
      stop(
        "Input should be a decimal less than 1 and greater than 0 ",
        call. = FALSE
      )
    } else if  (is.null(decimal)) {
      # Check that input is not null, and raise an error if it is
      stop("Input is NULL", call. = FALSE)
    } else if (is.na(decimal)) {
      # Check that input is not null, and raise and error if it is
      stop("Input is NA", call. = FALSE)
    } else if (!is.numeric(decimal)) {
      stop("Input is not a character", call. = FALSE)
    } else {
      # If checks of function pass, then run the main body of the function, and
      # return and output.
      # BODY --------------------------------------------------------------------
      decimal <-round(decimal, 2) 
      
      dplyr::case_when(
        decimal < 0.1 ~ "less than one tenth ",
        decimal < 0.14 ~ "one tenth ",
        decimal < 0.22 ~ "around a fifth ",
        decimal < 0.3 ~ "one quarter ",
        decimal < 0.4 ~ "around a third ",
        decimal < 0.55 ~ "around a half ",
        decimal < 0.6 ~ "just less than two thirds ",
        decimal < 0.7 ~ "around two thirds",
        decimal < 0.8 ~ "around three quarters " ,
        decimal < 0.9 ~ "over three quarters ",
        decimal >= 0.9 ~ "Almost all "
      )
      
    }
  }, warning = function(war){
    warning(war)
  }, error = function(err){
    err$message <- paste("While producing the correct phrase", err, sep = " ")
    stop(err)
  })
}
