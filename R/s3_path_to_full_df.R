#' Read data from s3 with automatic processing 
#' 
#' @param s3_path path to the s3 file bucket/folder/file.csv 
#' @param ...  arguments passed to read.csv or read_excel. 
#'
#' @return Dataframe
#'
#' @examples lasrap::s3_path_to_full_df("alpha-test-team/mpg.csv")
#' 
#' if you are using a file with .gz, .bz or .xz extension, please use
#' botor::s3_read directly
#' 
#' 
s3_path_to_full_df <- function(s3_path, ...) {
  # trim s3:// if included by the user
  s3_path <- paste0('s3://', gsub('^s3://', "", s3_path))
  # fileexts accepted by s3_read
  accepted_direct_fileext <- c('csv' = read.csv, 
                               'json' = jsonlite::fromJSON,
                               'jsonl' = jsonlite::stream_in,
                               'rds' = readRDS,
                               'sas7bdat' = haven::read_sas,
                               'sav' = haven::read_spss,
                               'dta' = haven::read_stata)
  # specify all other accepted filetypes
  excel_filepaths <- c('xlsx', 'xls', 'xlsm')
  accepted_fileext <- c(names(accepted_direct_fileext), excel_filepaths)
  fileext <- tolower(tools::file_ext(s3_path))
  # error if invalid filepath is entered
  if(!grepl(paste0('(?i)', accepted_fileext, collapse = "|"), fileext)) {
    stop(paste0("Invalid filetype entered. Please confirm that your file",
                " extension is one of the following: ", 
                paste0(accepted_fileext, collapse = ', '), ". \n ",
                "Alternatively, use botor directly to read in your file."))
  }
  # if we are using a function accepted by s3_read, then use that to parse 
  # the data
  if(grepl(paste0('(?i)', names(accepted_direct_fileext), collapse = "|"), 
           fileext)) {
    # read from s3 using our designated method
    tryCatch({
      botor::s3_read(s3_path, fun = accepted_direct_fileext[[tolower(fileext)]])
    },
    error = function(cond){
      stop("\nError, file cannot be parsed. \nYou either don't have access to this bucket, or are using an invalid s3_path argument (the s3_path you've entered needs correcting).")
    })
    
  } else {
    tryCatch({
      lasrap::read_using(FUN = readxl::read_excel, s3_path = s3_path, ...)
    },
    error = function(cond){
      stop("\nError, file cannot be parsed. \nYou either don't have access to this bucket, or are using an invalid s3_path argument (the s3_path you've entered needs correcting).")
    })
    
  }
}