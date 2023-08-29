#' coming_soon UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_coming_soon_ui <- function(id){
  ns <- NS(id)
  tagList(
   tags$div(class = "text-center",
            tags$h1("Coming Soon", icon("tools")),
            tags$h4("Currently working tabs have a ",tags$span(class = "text-success", shiny::icon("star", class = "text-success"), "green icon", shiny::icon("star", class = "text-success")))
            )
  )
}
    
#' coming_soon Server Functions
#'
#' @noRd 
mod_coming_soon_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_coming_soon_ui("coming_soon_1")
    
## To be copied in the server
# mod_coming_soon_server("coming_soon_1")
