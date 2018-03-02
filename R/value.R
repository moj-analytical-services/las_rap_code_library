# value
#
#' Takes a filtered prison dataset and returns the total of a specified quarter.
#
#' @param tibble A filter tibble
#' @param reference_date Either the current date, previous quarter date of previous year date
#' @examples
#' value(tibble, "current") (returns the total number of observations for the most recent quarter)
#' value(tibble, "previous_q") (returns the total number of observations for the previous quarter)

#
# This function filters a dataset and returns totals based on a given criteria
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

value <- function (tibble, reference_date) {

  group <- dplyr::group_by(tibble, extrdate)
  tibble2 <- dplyr::summarize(group, sum_pop = sum(sum_pop))

  if      (reference_date == "current")    {
    sum(tibble2[tibble2 == extract1, "sum_pop"])
    }
  else if (reference_date == "previous_q") {
    sum(tibble2[tibble2 == extract2, "sum_pop"])
    }
  else if (reference_date == "previous_y") {
    sum(tibble2[tibble2 == extract3, "sum_pop"])
    }
  else    print(
    "Please enter a reference_date as 'previous_q' or 'previous_y'")

}
