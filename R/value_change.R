# value_change
#
#' Returns a formatted value, difference or percentage difference. Differences are calculated from the most recent quarter and the quarter specified in the command. Uses functions: value,
#' format_num and format_perc in the same package.
#' @seealso \code{\link{value}}
#' @seealso \code{\link{format_num}}
#' @seealso \code{\link{format_perc}}
#'
#' @param tibble A filter tibble
#' @param reference_date Either the current date, previous quarter date of previous year date
#' @param type Either a value, difference, or percentage difference
#' @param format logical value to enable formating
#' @param volval string to decide summing volume workload or value (expenditure)
#' @examples
#' value_change(tibble, "current", "value", TRUE) (returns the total number of observations for the most recent quarter with formatting)
#' value_change(tibble, "previous_q", "perc_diff", FALSE) (returns the perecentage difference between observations in the current and previous quarter, without any formatting)

#
# This function filters a dataset and returns totals based on a given criteria
#

library(dplyr)

value_change <- function (tibble, reference_date, type, format, volval) {

  volval2 <- dplyr::enquo(volval)
  
  value1 <- as.numeric(lasrap::value(tibble, "current", !!volval2))
  value2 <- as.numeric(lasrap::value(tibble, reference_date, !!volval2))


  if      (type == "value") {
    out <- (value2)
    }
  else if (type == "diff") {
    out <- (value1 - value2)
    }
  else if (type == "perc_diff") {
    out <- ifelse(value2 == 0, 1, ( (value1 - value2) / value2))
    }
  else
    print("Please enter a type as 'value', diff' or perc_diff'" )


  if (format == TRUE) {
    if (type == "perc_diff") {
      out <- out %>% mojrap::format_perc()
      }
    else (out <- out %>% mojrap::format_num())
  }


  if (out == "0%") {
    out <- "0%" #value_change(tibble, reference_date, "diff", TRUE)
    } # If there is a 0% value change true absolute difference
  if (out == "0") {
    out <- 0 #print("ERROR: There has been no change")
    } # If there is still no change, show an error message

  out

}
