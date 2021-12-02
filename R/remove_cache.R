#' @title remove_cache
#'
#' @description This function deletes the cache folder complementing the bulletin output.
#'
#'
#'
#' @details When the 0_LAS_Bulletin.Rmd script is run, R automatically saves a cache folder that complements the script. For an accurate bulletin output, it is best to delete the cache folder.
#' 
#' The \code{remove_cache} function gives you the choice to delete the cache folder.
#'
#' @param remove Either TRUE or FALSE.
#'
#' @return Deletes the cache folder if input is TRUE and does not delete it if input is FALSE. 
#'
#' @examples
#'
#' remove_cache(TRUE)
#' remove_cache(FALSE)
#'
#' @export
#' 

remove_cache <- function(remove) {
  
  remove <- as.character(remove)
  
  if(!remove %in% c(TRUE, FALSE)){
    stop("Please input either a TRUE or FALSE")
  }
  
  else if(remove == TRUE){
    
    # Deletes 0_LAS_Bulletin_cache if it exists
    if (file.exists("0_LAS_Bulletin_cache")) unlink("0_LAS_Bulletin_cache", recursive = TRUE)
    
  }
  
  
}
