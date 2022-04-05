#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

stdz_npi_output <- function(flat_npi_df, npi_type = "Individual"){
  if(npi_type=="Individual"){
    dplyr::transmute(flat_npi_df, 
                     NPI = npi, 
                     Name = glue::glue("{basic_first_name} {basic_last_name}"),
                     `NPI Type` = npi_type,
                     `Primary Practice Address` = glue::glue("{addresses_address_1} {addresses_address_2} 
                                                 {addresses_city},{addresses_state} {stdz_zips(addresses_postal_code)}"),
                     Phone = addresses_telephone_number,
                     `Primary Taxonomy` = taxonomies_desc
    )
  } else if(npi_type=="Organization"){
    dplyr::transmute(flat_npi_df, 
                     NPI = npi, 
                     Name = glue::glue("{basic_organization_name}"),
                     `NPI Type` = npi_type,
                     `Primary Practice Address` = glue::glue("{addresses_address_1} {addresses_address_2} 
                                                 {addresses_city},{addresses_state} {stdz_zips(addresses_postal_code)}"),
                     Phone = addresses_telephone_number,
                     `Primary Taxonomy` = taxonomies_desc
    )
  }

}
