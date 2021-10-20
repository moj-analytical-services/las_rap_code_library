#' Read from S3 using a particular function
#'
#' @param FUN a function to parse the data into
#' @param ... arguments for said function 
#' @param s3_path path to the s3 file bucket/folder/file.txt
#'
#' @return Dataframe
#' @export 
#'
#' @examples lasrap::read_using(FUN=readxl::read_excel, s3_path="alpha-test-team/mpg.xlsx")

read_using <- function(FUN, s3_path, ...) {
  # trim s3:// if included by the user
  s3_path <- paste0("s3://", gsub('^s3://', "", s3_path))
  botor::s3_read(s3_path, FUN, ...)
}
