#' Loads specific CSV from alpha-legal-aid-statistics-team AWS bucket and allows you to pick specific schemes and categories.
#'
#' @description Function to load CSV from alpha-legal-aid-statistics-team AWS bucket and filter by scheme and category. Input for all parameters is case-sensitive.
#'
#' @param file Needs to be either NULL or the name of a CSV that is in the AWS bucket. By default, this function loads the latest CSV.
#' @param choose_scheme Needs to be a valid scheme. By default, this function selects all avaiable schemes.
#' @param choose_cat Needs to be a valid category. By default, this function chooses all available categories depending on the chosen scheme.
#
#' @examples
# load_las_vroom(file = NULL) will give you the latest CSV
# load_las_vroom(file = "lasq418r_v3.csv") will give you version 3 of the las CSV dated on 2018Q4
# load_las_vroom(choose_scheme = "Legal Help") will give you the latest figures on Legal Help
#
# load_las_vroom(choose_scheme = "Crime Higher", choose_cat = c("hc - Higher Courts", "hc - Crown Courts")) will give
# you the latest Higher Courts and Crown Courts figures from the Crime Higher scheme
#' 
#' @return Data from las CSVs
#'
#' @export

load_las_vroom <- function(file = NULL, choose_scheme = all_scheme, choose_cat = all_cat) {
  
  # This collects the name of the latest LAS CSV
  s3tools::get_credentials()
  allCSVs <- s3tools::list_files_in_buckets(bucket_filter = 'alpha-legal-aid-statistics-team')$filename
  allCSVs <- allCSVs[which(stringr::str_sub(allCSVs,-4) == '.csv')]
  
  latest_yr <- max(stringr::str_sub(allCSVs, 6, 7))
  
  latest_CSV <- max(allCSVs[which(stringr::str_sub(allCSVs,6, 7) == latest_yr)])
  
  all_scheme <- c("Crime Higher", "Crime Apps and Grants", "Crime Lower", 
                  "Exceptional Case Funding", "Mediation", "Civil Representation", "Legal Help", 
                  "Central Funds", "Crown Court", "Higher Courts", "Overall Expenditure")
  
  scheme_exists <- is.element(choose_scheme, all_scheme)
  
  if(is.element("FALSE", scheme_exists)){
    stop("Please select valid scheme")
  }
  
  else if(is.null(file))
  {
    file <- latest_CSV
    file_path <- paste0("alpha-legal-aid-statistics-team/", file)
    output <- s3tools::read_using(FUN=vroom::vroom, s3_path=file_path)
    output <- dplyr::filter(output, scheme %in% choose_scheme)
    all_cat <- unique(output$category)
    
    cat_exists <- is.element(choose_cat, all_cat)
  }
  
  else if(!file %in% allCSVs)
  {
    stop("Please use a CSV from the alpha-legal-aid-statistics-team AWS bucket")
  }
  
  else if(file %in% allCSVs)
  {
    file_path <- paste0("alpha-legal-aid-statistics-team/", file)
    output <- s3tools::read_using(FUN=vroom::vroom, s3_path=file_path)
    output <- dplyr::filter(output, scheme %in% choose_scheme)
    all_cat <- unique(output$category)
    
    cat_exists <- is.element(choose_cat, all_cat)
  }
  
  else 
  {stop("Please enter either NULL or the name of a CSV that is in the AWS bucket")}
  
  
  if(is.element("FALSE", cat_exists)){
    stop("Please select correct category")
  }
  else{
    output <- dplyr::filter(output, category %in% choose_cat)
  }
  
  return(output)
}