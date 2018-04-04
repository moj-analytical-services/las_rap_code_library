# value
#
#' Takes a filtered dataset and returns the total of a specified quarter.
#
#' @param tibble A filter tibble
#' @param reference_date Either the current date, previous quarter date of previous year date
#' @param volval string to decide summing volume workload or value (expenditure)
#' @examples
#' value(tibble, "current") (returns the total number of observations for the most recent quarter)
#' value(tibble, "previous_q") (returns the total number of observations for the previous quarter)

#
# This function filters a dataset and returns totals based on a given criteria

value <- function (tibble, reference_date, volval) {
  
  volval2 <- enquo(volval)
  group <- dplyr::group_by(tibble, qtr)
  
  tibble2 <- dplyr::summarize(group, volval = sum(!!volval2))

  if      (reference_date == "current")    {
    sum(tibble2[tibble2 == quarter1, volval])
    }
  else if (reference_date == "previous_q") {
    sum(tibble2[tibble2 == quarter2, volval])
    }
  else if (reference_date == "previous_y") {
    sum(tibble2[tibble2 == quarter3, volval])
    }
  else    print(
    "Please enter a reference_date as 'current', previous_q' or 'previous_y'")

}

