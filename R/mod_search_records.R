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
    fluidRow(column(6, h3("Search NPI Records"))),
    fluidRow(
      column(3,
        textInput(inputId = ns("npi_number"), 
                  label = "NPI Number"),
        textInput(inputId = ns("first_name"), 
                  label = "First Name"),
        textInput(inputId = ns("city"), 
                  label = "City"),
        textInput(inputId = ns("postal_code"),
                  label = "Postal Code"),
        br(),
        actionButton(inputId = ns("clear_button"), label = "Clear")
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
        actionButton(inputId = ns("search_button"), label = "Search",
                     style="color: #fff; background-color: #428bca; border-color: #357ebd;")
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
    ),
    fluidRow(
      column(10,
             h3("Results:"),
             reactable::reactableOutput(outputId = ns("search_table"))
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
    
    search_df_react <- reactiveVal({
      data.frame(None = "")
    })
    
    observeEvent(input$clear_button,{
      shinyjs::reset("npi_number")
      shinyjs::reset("first_name")
      shinyjs::reset("city")
      shinyjs::reset("postal_code")
      shinyjs::reset("npi_type")
      shinyjs::reset("last_name")
      shinyjs::reset("state")
      shinyjs::reset("address_type")
      shinyjs::reset("taxonomy_desc")
      shinyjs::reset("taxonomy_desc")
      shinyjs::reset("organization_name")
      shinyjs::reset("country")
    })
    
    observeEvent(input$search_button, {
      req(  isTruthy(input$npi_number) |
            isTruthy(input$taxonomy_desc) |
            isTruthy(input$first_name) |
            isTruthy(input$last_name) | 
            isTruthy(input$organization_name) | 
            isTruthy(input$city) |
            isTruthy(input$state) |
            isTruthy(input$country) |
            isTruthy(input$postal_code)
            )
      search_df_react(
        npi::npi_flatten(
          try(
            npi::npi_search(number = input$npi_number,
                            taxonomy_description = input$taxonomy_desc,
                            first_name = input$first_name,
                            last_name = input$last_name
                            )
            )
          )
        )
      
    })
    
    output$search_table <- reactable::renderReactable({
      reactable::reactable( 
        search_df_react()
        )
    })
    
  })
}
    
## To be copied in the UI
# mod_search_records_ui("search_records_1")
    
## To be copied in the server
# mod_search_records_server("search_records_1")
