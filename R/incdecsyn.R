# incdecsyn

#

#' Returns a string that quantifies a change and uses synonyms for increase/decrease

#' @param value value

#' @param tense which tense you want the quantifying string to be in

#' @examples

#' incdecsyn(3456,"past") incdecsyn(-273, "singular") 

#'

incdecsyn <- function(value, tense) {
  
  # Check input arguments
  
  if  (is.null(value) | is.null(tense)) {
    
    # Check that input is not null, and raise an error if it is
    
    stop("Input to incdec is NULL", call. = FALSE)
    
  } else if (is.na(value) | is.na(tense)) {
    
    # Check that input is not null, and raise and error if it is
    
    stop("Input to incdec is NA", call. = FALSE)
    
  } else if (is.character(value) | !is.character(tense)) {
    
    stop("Input to incdec is a character", call. = FALSE)
    
  } else if (!tense %in% c("past", "present", "singular", "plural")) {
    
    stop("Please provide a 'tense' as either present, past, singular or plural")
  }
  
  else {
    
    # MAIN BODY --------------------------------------------------------------------
    
    value <- as.numeric(value) # converts strings into a number
    
    if (tense == "present") {
      if (value > 0) {
        sample((c("up by", "rising by", "increasing by")),1)
      }
      else if (value < 0) {
        sample((c("falling by", "down by", "decreasing by")),1)
      }
      else stop("ERROR")
    }
    
    else if (tense == "past") {
      if (value > 0) {
        sample((c("was up by", "has risen by", "increased by")),1)
      }
      else if (value < 0) {
        sample((c("has fallen by", "is down by", "decreased by")),1)
      }
      else ("was unchanged")
    }
    
    else if (tense == "singular") {
      if (value > 0) {
        "increase"
      }
      else if (value < 0) {
        sample((c("fall", "drop", "decrease")),1)
      }
      else stop("ERROR")
    }
    
    else if (tense == "plural") {
      if (value > 0) {
        "increases"
      }
      else if (value < 0) {
        "decreases"
      }
      else stop("ERROR")
    }
    
  }
  
}