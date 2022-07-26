# read_template

#

#' Uses botor to read in s3 ppt template 
#' 
#' @param FUN officer::read_pptx

#' @param s3_path s3 path to read

#' @examples

#' legal_aid_slides <- read_template(FUN=officer::read_pptx, s3_path="alpha-legal-aid-statistics-team/Legal_Aid_Slides_Template.pptx")

#'

read_template <- function(FUN, s3_path, ...) {
  # trim s3:// if included by the user
  s3_path <- paste0("s3://", gsub('^s3://', "", s3_path))
  # find fileext
  file_ext <- paste0('.', tolower(tools::file_ext(s3_path)))
  # download file to tempfile()
  tmp <- botor::s3_download_file(s3_path, 
                                 tempfile(fileext = file_ext), 
                                 force = TRUE)
  FUN(tmp, ...)
}