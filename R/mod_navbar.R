#' navbar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_navbar_ui <- function(id){
  ns <- NS(id)
    bs4Dash::bs4DashNavbar(
      shiny::tags$h2("R Minor"),
      status = "white",
      border = TRUE,
      sidebarIcon = shiny::icon("bars"),
      fixed = FALSE
    )
}
    
#' navbar Server Functions
#'
#' @noRd 
mod_navbar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_navbar_ui("navbar_1")
    
## To be copied in the server
# mod_navbar_server("navbar_1")
