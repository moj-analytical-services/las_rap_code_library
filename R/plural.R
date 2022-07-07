# plural

#

#' Returns a string with an "s" added if plural or 0

#' @param value the value

#' @param string the word that needs pluralising 

#' @examples

#' plural(1, "claim") plural(3, "start")

#'


plural <- function(value, string) {
  if (value>1 | value==0) {
    paste0(string,"s")
  } else {
    paste0(string)
  }
}
