# value
#
#' Takes a filtered dataset and returns the total of a specified quarter.
#
#' @param tibble A filter tibble
#' @param reference_date Either the current date, previous quarter date of previous year date
#' @examples
#' value(tibble, "current") (returns the total number of observations for the most recent quarter)
#' value(tibble, "previous_q") (returns the total number of observations for the previous quarter)

#
# This function filters a dataset and returns totals based on a given criteria

# need to look at swirl to learn dplyr


value <- function (tibble, reference_date) {

  group <- dplyr::group_by(tibble, qtrdte)
  
  tibble2 <- dplyr::summarize(group, volume = sum(volume))

  if      (reference_date == "current")    {
    sum(tibble2[tibble2 == extract1, "volume"])
    }
  else if (reference_date == "previous_q") {
    sum(tibble2[tibble2 == extract2, "volume"])
    }
  else if (reference_date == "previous_y") {
    sum(tibble2[tibble2 == extract3, "volume"])
    }
  else    print(
    "Please enter a reference_date as 'current', previous_q' or 'previous_y'")

}
