#' ceAPs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ceAPs_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ceAPs Server Functions
#'
#' @noRd 
mod_ceAPs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ceAPs_ui("ceAPs_ui_1")
    
## To be copied in the server
# mod_ceAPs_server("ceAPs_ui_1")
