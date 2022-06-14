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
#' value(tibble, "previous_y") (returns the total number of observations for the previous years quarter)
#' value(tibble, "whole_yr") (returns the total number of observations for the cumulative year to date)
#' value(tibble, "previous_2y") (returns the total number of observations for the same quarter 2 years before)

#
# This function filters a dataset and returns totals based on a given criteria

library(dplyr)

value <- function (tibble, reference_date, volval) {
  
  volval2 <- dplyr::enquo(volval)
  
  group <- dplyr::group_by(tibble, qtr)
  
  tibble2 <- dplyr::summarize(group, volval = sum(!!volval2, na.rm = TRUE))

  if      (reference_date == "current")    {
    as.double(tibble2 %>% dplyr::filter(qtr == quarter1) %>% dplyr::select(volval)) %>% tidyr::replace_na(0)
    # sum(tibble2[tibble2 == quarter1,volval])
    }
  else if (reference_date == "previous_q") {
    as.double(tibble2 %>% dplyr::filter(qtr == quarter2) %>% dplyr::select(volval)) %>% tidyr::replace_na(0)
    # sum(tibble2[tibble2 == quarter2])
    }
  else if (reference_date == "previous_y") {
    as.double(tibble2 %>% dplyr::filter(qtr == quarter3) %>% dplyr::select(volval)) %>% tidyr::replace_na(0)
    # sum(tibble2[tibble2 == quarter3])
  }
  else if (reference_date == "whole_yr") {
    as.double(tibble2 %>% dplyr::filter(qtr == quarter1) %>% dplyr::select(volval))+as.double(tibble2 %>% dplyr::filter(qtr == quarter2) %>% dplyr::select(volval))+as.double(tibble2 %>% dplyr::filter(qtr == quarter3ago) %>% dplyr::select(volval))+as.double(tibble2 %>% dplyr::filter(qtr == quarter4ago) %>% dplyr::select(volval))  %>% 
      tidyr::replace_na(0)
  }
  else if (reference_date == "previous_2y") {
    as.double(tibble2 %>% dplyr::filter(qtr == quarter5) %>% dplyr::select(volval)) %>% tidyr::replace_na(0)
    # sum(tibble2[tibble2 == quarter3])
  }
  else    print(
    "Please enter a reference_date as 'current', 'previous_q', 'whole_yr' , 'previous_y' or 'previous_2y'")

}




