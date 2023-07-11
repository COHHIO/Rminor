#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shiny NS tagList 

mod_body_ui <- function(id){
  ns <- NS(id)
  bs4Dash::dashboardBody(
    uiOutput(ns("bodyui"))
  )
}

#' body Server Functions
#'
#' @noRd 
mod_body_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
    
    e <- environment()
      
    output$bodyui <- renderUI({
      req(active$ui)
      message("Tab: ", active$tab)

      if (exists(active$server))
        rlang::exec(active$server, id = paste0("body_", active$tab), .env = e)
      # Render the body UIs here
      if (exists(active$ui)) {
        rlang::exec(active$ui, id = paste0(ns("body_"), active$tab))
      } else {
        rlang::exec("mod_coming_soon_ui", id = "coming_soon")
      }
        
    })
  })
}
    
## To be copied in the UI
# mod_body_ui("body_1")
    
## To be copied in the server
# mod_body_server("body_1")
