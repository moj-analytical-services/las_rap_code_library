#' Loads specific CSV from alpha-legal-aid-statistics-team AWS bucket
#'
#' @description Function to load CSV from alpha-legal-aid-statistics-team AWS bucket.
#'
#' @param file Needs to be either NULL or the name of a CSV that is in the AWS bucket
#
#' @examples
# load_las(NULL) will give you the latest CSV
# load_las("lasq418r_v3.csv") will give you version 3 of the las CSV dated on 2018Q4
#' 
#' @return CSV from alpha-legal-aid-statistics-team AWS bucket 
#'
#' @export

load_las <- function(file) {
  
  # This collects the name of the latest LAS CSV
  s3tools::get_credentials()
  allCSVs <- s3tools::list_files_in_buckets(bucket_filter = 'alpha-legal-aid-statistics-team')$filename
  allCSVs <- allCSVs[which(str_sub(allCSVs,-4) == '.csv')]
  
  latest_yr <- str_sub(allCSVs, 6, 7) %>% max()
  
  latest_CSV <- allCSVs[which(str_sub(allCSVs,6, 7) == latest_yr)] %>%
    max()
  
  
  if(is.null(file))
  {
    file <- latest_CSV
    file_path <- paste0("alpha-legal-aid-statistics-team/", file)
    output <- s3tools::s3_path_to_full_df(file_path)
    return(output)
  }
  
  else if(file %in% allCSVs)
  {
    file_path <- paste0("alpha-legal-aid-statistics-team/", file)
    output <- s3tools::s3_path_to_full_df(file_path)
    return(output)
  }
  
  else 
  {print(paste0("Please enter either NULL or the name of a CSV that is in the AWS bucket"))}
  
}