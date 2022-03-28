#' search_records UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_search_records_ui <- function(id, country_choices = countries, state_choices = states){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        textInput(inputId = ns("npi_number"), 
                  label = "NPI Number"),
        textInput(inputId = ns("first_name"), 
                  label = "First Name"),
        textInput(inputId = ns("city"), 
                  label = "City"),
        textInput(inputId = "postal_code",
                  label = "Postal Code"),
        br(),
        actionButton(inputId = "clear_button", label = "Clear")
             ),
      column(3,
        selectInput(inputId = ns("npi_type"), 
                    label = "NPI Type", 
                    choices = c("Any" = "any",
                                "Individual" = "individual",
                                "Organization" = "organization")),
        textInput(inputId = ns("last_name"), 
                  label = "Last Name"),
        selectInput(inputId = ns("state"), 
                    label = "State", 
                    choices = state_choices),
        selectInput(inputId = ns("address_type"), 
                    label = "Address Type", 
                    choices = c("Any" = "any",
                                "Primary Location" = "primary_location",
                                "Secondary Location" = "secondary_location")),
        actionButton(inputId = "search_button", label = "Search")
             ),
      column(4,
             textInput(inputId = ns("taxonomy_desc"), 
                       label = "Taxonomy Description"),
             textInput(inputId = ns("organization_name"), 
                       label = "Organization Name (LBN, DBA, Former LBN or Other Name)"),
             selectInput(inputId = ns("country"), 
                         label = "Country", 
                         choices = country_choices)
             )
    )
    
  )
}
    
#' search_records Server Functions
#'
#' @noRd 
mod_search_records_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_search_records_ui("search_records_1")
    
## To be copied in the server
# mod_search_records_server("search_records_1")
