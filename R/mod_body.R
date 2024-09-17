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
mod_body_server <- function(id, is_youth) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    e <- environment()
    
    # Render the appropriate UI based on the active tab
    output$bodyui <- renderUI({
      req(active$ui)
      message("Tab: ", active$tab)
      
      # Render Program Details tab
      if (active$tab == "program_details") {
        mod_body_program_details_ui(ns("program_details"))
      } 
      else if (active$tab == "youth_program_details") {
        mod_body_program_details_ui(ns("youth_program_details"), is_youth = TRUE)
      }
      # Handle other tabs
      else {
        if (exists(active$server)) {
          rlang::exec(active$server, id = paste0("body_", active$tab), .env = e)
        }
        if (exists(active$ui)) {
          rlang::exec(active$ui, id = paste0(ns("body_"), active$tab))
        } else {
          rlang::exec("mod_coming_soon_ui", id = "coming_soon")
        }
      }
    })
    
    # Server logic for the Program Details tab, separated from UI rendering
    observeEvent(active$tab == "program_details", {
      if (active$tab == "program_details") {
        mod_body_program_details_server("program_details")
      }
    })
    
    # Server logic for the Program Details tab, separated from UI rendering
    observeEvent(active$tab == "youth_program_details", {
      if (active$tab == "youth_program_details") {
        mod_body_program_details_server("youth_program_details", is_youth = TRUE)
      }
    })
  })
}


    
## To be copied in the UI
# mod_body_ui("body_1")
    
## To be copied in the server
# mod_body_server("body_1")
