# incdec_sentence

#

#' Returns a string that quantifies a change and provides the value of the change

#' @param tibble a tibble

#' @param reference_data the date you wish to compare with the current date

#' @param type either difference or percentage diffence

#' @param tense which tense you want the quantifying string to be in

#' @examples

#' incdec_sentence(tibble, "previous_q", "perc_diff", "past") returns "decreased by X%"

#' @seealso \code{\link{incdec}}

#'



incdec_sentence <- function (tibble, reference_data, type, tense) {
  
  
  
  int1 <- value_change(tibble, reference_data, type, FALSE)
  
  int2 <- value_change(tibble, reference_data, type, TRUE)
  
  
  
  if (substr(int2, 1, 1) == "8" | substr(int2, 1, 2) == "18") {a = "an"}
  
  else {a = "a"}
  
  
  
  if (tense %in% c("past", "present")) {
    
    paste(incdec(as.numeric(int1), tense), int2)
    
  }
  
  
  
  else if (tense %in% c("plural", "singular")) {
    
    paste(a, int2, incdec(as.numeric(int1), tense))}
  
  
  
  else {print("You have not provided a valid tense")}
  
  
  
}