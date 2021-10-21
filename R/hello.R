#' hello()
#'
#' @description Prints a Hello message to any name that was input to this function.
#'
#' @param name
#
#' @examples
# hello('A2J') will print "Hello A2J!"
#' 
#' @return Hello message
#'
#' @export

hello <- function(name) {
  
  message <- paste0("Hello ", name, "!")
  
  print(message)
}
