# incdecsyn_sentence

#

#' Returns a string that quantifies a change and provides the value of the change. Uses synonyms for increase/decrease

#' @param tibble a tibble

#' @param reference_data the date you wish to compare with the current date

#' @param type either difference or percentage diffence

#' @param tense which tense you want the quantifying string to be in

#' @examples

#' incdecsyn_sentence(tibble, "previous_q", "perc_diff", "past") returns "decreased by X%"

#' @seealso \code{\link{incdecsyn}}

#'

incdecsyn_sentence <- function (tibble, reference_data, type, tense, volval) {
  
  volval2 <- dplyr::enquo(volval)
  
  int1 <- lasrap::value_change(tibble, reference_data, type, FALSE, !!volval2)
  
  int2 <- lasrap::value_change(tibble, reference_data, type, TRUE, !!volval2)
  
  
  
  if (substr(int2, 1, 1) == "8" | substr(int2, 1, 2) == "18") {a = "an"}
  
  else {a = "a"}
  
  
  
  if (tense %in% c("past", "present")) {
    
    paste(incdecsyn(as.numeric(int1), tense), int2)
    
  }
  
  
  
  else if (tense %in% c("plural", "singular")) {
    
    paste(a, int2, incdecsyn(as.numeric(int1), tense))}
  
  
  
  else {print("You have not provided a valid tense")}
  
  
  
}

