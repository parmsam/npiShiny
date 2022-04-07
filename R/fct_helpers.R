#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

stdz_npi_output <- function(flat_npi_df, npi_type = "Individual"){
  if(npi_type=="Individual"){
    df <- dplyr::transmute(flat_npi_df, 
                     NPI = npi, 
                     Name = glue::glue("{basic_first_name} {basic_last_name}"),
                     `NPI Type` = "Individual",
                     # `NPI Type` = 1,
                     `Primary Practice Address` = glue::glue("{addresses_address_1} {addresses_address_2} 
                                                 {addresses_city},{addresses_state} {stdz_zips(addresses_postal_code)}"),
                     Phone = addresses_telephone_number,
                     `Primary Taxonomy` = taxonomies_desc,
    )
    unique(df)
  } else if(npi_type=="Organization"){
    df <- dplyr::transmute(flat_npi_df, 
                     NPI = npi, 
                     Name = glue::glue("{basic_organization_name}"),
                     `NPI Type` = "Organization",
                     # `NPI Type` = 2,
                     `Primary Practice Address` = glue::glue("{addresses_address_1} {addresses_address_2} 
                                                 {addresses_city},{addresses_state} {stdz_zips(addresses_postal_code)}"),
                     Phone = addresses_telephone_number,
                     `Primary Taxonomy` = taxonomies_desc
    )
    unique(df)
  }

}

npi_flat_search <- function( number = NULL,
                             enumeration_type = NULL,
                             taxonomy_description = NULL,
                             first_name = NULL,
                             last_name = NULL,
                             use_first_name_alias = NULL,
                             organization_name = NULL,
                             address_purpose = NULL,
                             city = NULL,
                             state = NULL,
                             postal_code = NULL,
                             country_code = NULL,
                             limit = 10L){
  npi_results_obj <- npi::npi_search(number=number, enumeration_type=enumeration_type, taxonomy_description=taxonomy_description,
                                     first_name=first_name, last_name=last_name, use_first_name_alias=use_first_name_alias,
                                     organization_name=organization_name, address_purpose=address_purpose, city=city,
                                     state=state, postal_code=postal_code, country_code=country_code, limit=limit)
  npi::npi_flatten(npi_results_obj)
}