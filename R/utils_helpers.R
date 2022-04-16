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

add_star_to_end <- function(str_field, keep_same = FALSE){
  str_field <- stringr::str_trim(str_field)
  ifelse( keep_same == FALSE & nchar(str_field) > 0,
          stringr::str_c(str_field, "*"),
          str_field
  )
}
