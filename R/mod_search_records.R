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
    tags$div(HTML('<a href="https://github.com/parmsam/npiShiny" style="float:right;">Github Repo</a>')),
    HTML('<a name="top"></a>'),
    fluidRow(column(6, h3("Search NPI Records"))),
    hr(),
    fluidRow(
      column(3,
        textInput(inputId = ns("npi_number"), 
                  label = "NPI Number"),
        selectInput(inputId = ns("npi_type"), 
                    label = "NPI Type", 
                    choices = c("Any" = "any",
                                "Individual" = "individual",
                                "Organization" = "organization")),
        div(class="custom-label", tags$label("for individuals")),
        textInput(inputId = ns("first_name"), 
                  label = "First Name"),
        textInput(inputId = ns("last_name"), 
                  label = "Last Name")
             ),
      column(3,
        textInput(inputId = ns("city"), 
                  label = "City"),
        textInput(inputId = ns("postal_code"),
                  label = "Postal Code"),
        selectInput(inputId = ns("state"), 
                    label = "State", 
                    choices = state_choices),
        selectInput(inputId = ns("address_type"), 
                    label = "Address Type", 
                    choices = c("Any" = "any",
                                "Primary Location" = "primary_location",
                                "Secondary Location" = "secondary_location"))
             ),
      column(4,
             selectInput(inputId = ns("country"), 
                         label = "Country", 
                         choices = country_choices),
             textInput(inputId = ns("taxonomy_desc"), 
                       label = "Taxonomy Description"),
             div(class="custom-label", tags$label("for organizations")),
             textInput(inputId = ns("organization_name"), 
                       label = "Organization Name (LBN, DBA, Former LBN or Other Name)")
             )
    ),
    fluidRow(
      br(),
      column(3,
        actionButton(inputId = ns("clear_button"), label = "Clear"),
             ),
      column(4,
        actionButton(inputId = ns("search_button"), label = "Search",
                   style="color: #fff; background-color: #428bca; border-color: #357ebd;")
             ),
      br(),br(),
      hr()
    ),
    fluidRow(
      column(12,
             h3("Results"),
             downloadButton(outputId = ns("download_results"), "Download Search Results"),
             reactable::reactableOutput(outputId = ns("search_table")),
             tags$div(HTML('<a href="#top" style="float:right;">go to top</a>'))
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
    
    #initialize reference search result dataframe
    reference_df_react <- reactiveVal({
      data.frame(None = "")
    })
    
    #initialize output search results dataframe 
    search_df_react <- reactiveVal({
      data.frame(None = "")
    })
    
    #logic to get inputs and reset them on clear button press 
    initial_inputs <- isolate(reactiveValuesToList(input))
    observeEvent(input$clear_button,{
      for(id in names(initial_inputs)){
        shinyjs::reset(id)
      }
    })
    
    #define npi record types
    npi_type_react <- reactive({
      if( (isTruthy(input$first_name) | isTruthy(input$last_name)) & !isTruthy(input$organization_name) ){
        "Individual"
      }else if( !isTruthy(input$first_name) & !isTruthy(input$last_name) & isTruthy(input$organization_name) ){
        "Organization"
      } else{
        "Individual"
      }
    })
    
    #try search on search button press
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
          tryCatch({
            temp_df <- npi_flat_search(
              number = stringr::str_trim(input$npi_number),
              taxonomy_description = stringr::str_trim(input$taxonomy_desc),
              first_name = stringr::str_trim(input$first_name),
              last_name = stringr::str_trim(input$last_name),
              organization_name = stringr::str_trim(input$organization_name),
              city = stringr::str_trim(input$city),
              state = stringr::str_trim(input$state),
              postal_code = stringr::str_trim(input$postal_code),
              country_code = stringr::str_trim(input$country)
              )
            temp_df2 <- dplyr::filter(temp_df, addresses_address_purpose == 'LOCATION')
            temp_df2 <- dplyr::mutate(temp_df2, basic_status = dplyr::recode(basic_status, "A"="Active"))
            temp_df2 <- dplyr::mutate(temp_df2, 
            `Primary Practice Address` = glue::glue("{addresses_address_1} {addresses_address_2}
                                                    {addresses_city},{addresses_state} {stdz_zips(addresses_postal_code)}"))
            reference_df <- dplyr::select(temp_df2,
                                          NPI = npi,
                                          `Enumeration Date` = basic_enumeration_date,
                                          `Status` = basic_status,
                                          `Last Updated` = basic_last_updated,
                                          `Telephone Number` = addresses_telephone_number,
                                          `Primary Practice Address`,
                                          `Address Type` = addresses_address_type,
                                          `Identifier Type` = identifiers_desc,
                                          `Other Identifier` = identifiers_identifier
                                          )
            reference_df <- unique(reference_df)
            reference_df_react( reference_df  )
            
            stdz_npi_output( temp_df2, npi_type_react() )
            },
            error = function(cond){
              return( data.frame(Error = "") )
            }
          )
        )
      
    })
    
    #update search table output
    output$search_table <- reactable::renderReactable({
      reactable::reactable( 
        search_df_react(),
        showPageInfo = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20, 100),
        defaultPageSize = 5,
        paginationType = "jump",
        searchable = TRUE,
        defaultColDef = reactable::colDef(align = "left"),
        onClick = 'expand',
        details = function(index) {
            tryCatch({
              extended_data <- reference_df_react()[reference_df_react()$NPI == search_df_react()$NPI[index], ]
            htmltools::div(style = "padding: 16px",
                           reactable::reactable(extended_data, outlined = TRUE)
                           )
            }, error = function(e){
            htmltools::div(style = "padding: 16px",
                             reactable::reactable( data.frame(Empty=""), outlined = TRUE)
              )
            })
        }
        
        # columns = list(
        #   `NPI Type` = reactable::colDef(cell = reactablefmtr::icon_sets(data, icons = c("arrow-down","minus", "plus")
        #                                                                  )
        #                                  )
        #   )
      )
    })
    
    #define joined dataframe for csv download
    joined_df <- reactive({
      req( reference_df_react() )
      req( search_df_react() )
      dplyr::left_join( search_df_react(), reference_df_react(), by = c("NPI"))
    })
    
    #define download logic
    output$download_results <- downloadHandler(
      filename = function() {
        paste('npiShiny-results_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(joined_df(), con, row.names = FALSE, na = "")
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_search_records_ui("search_records_1")
    
## To be copied in the server
# mod_search_records_server("search_records_1")
