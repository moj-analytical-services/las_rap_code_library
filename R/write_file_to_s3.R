#' Save file in s3 bucket
#'
#' @param local_file_path File path of document in working directory
#' @param s3_path path to the s3 file 
#' @param overwrite
#' @param multipart
#'
#' @export 
#'
#' @examples lasrap::write_file_to_s3("data/T_2_1.xlsx", 'alpha-app-legalaidtools/T_2_1.xlsx', overwrite = TRUE)

write_file_to_s3 <- function(local_file_path, s3_path, overwrite=FALSE, 
                             multipart = "unused") {
  # ensure s3:// is present if not already
  s3_path <- paste0("s3://", gsub("^s3://", "", s3_path))
  if (overwrite || !(botor::s3_exists(s3_path))) {
    tryCatch(
      botor::s3_upload_file(local_file_path, s3_path),
      error = function(c) {
        message(paste0("Could not upload ", local_file_path, " to ", s3_path),
                appendLF = TRUE)
        stop(c, appendLF = TRUE)
      }
    )
    
  } else {
    stop("File already exists and you haven't set overwrite = TRUE, stopping")
  }
}