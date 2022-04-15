#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

stdz_zips <- function(zip_str){
  ifelse( nchar(zip_str) > 5, 
          paste0( substr(zip_str, 1,5), "-", substr(zip_str, 6,9)), 
          as.character(zip_str))
}
