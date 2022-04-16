#' jump_top_site UI Function
#'
#' @description A shiny Module. Scroll up button based on tutorial from w3schools here: https://www.w3schools.com/howto/howto_js_scroll_to_top.asp
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_jump_top_site_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(HTML('<a id="top-of-site" name="top"></a>')),
    tags$div(HTML('<button onclick="topFunction()" id="myTopBtn" title="Go to top">go to top</button>')),
    tags$style(HTML(
'#myTopBtn {
  display: none;
  position: fixed;
  bottom: 20px;
  right: 30px;
  z-index: 99;
  border: none;
  outline: none;
  background-color: #5597d0;
  color: white;
  cursor: pointer;
  padding: 7px;
  border-radius: 4px;
}
#myTopBtn:hover {
  background-color: #555;
}')),
    tags$script(HTML(
'//Get the button
var mybutton = document.getElementById("myTopBtn");

// When the user scrolls down 20px from the top of the document, show the button
window.onscroll = function() {scrollFunction()};

function scrollFunction() {
  if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
    mybutton.style.display = "block";
  } else {
    mybutton.style.display = "none";
  }
}

// When the user clicks on the button, scroll to the top of the document
function topFunction() {
  document.body.scrollTop = 0;
  document.documentElement.scrollTop = 0;
}'))

  )
}
    
#' jump_top_site Server Functions
#'
#' @noRd 
# mod_jump_top_site_server <- function(id){
#   moduleServer( id, function(input, output, session){
#     ns <- session$ns
#  
#   })
# }
    
## To be copied in the UI
# mod_jump_top_site_ui("jump_top_site_1")
    
## To be copied in the server
# mod_jump_top_site_server("jump_top_site_1")
