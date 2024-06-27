#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # Inputs needed in submodules
  active <<- reactiveValues()
  observe({
    active$ui <- paste0("mod_body_", input$active_tab, "_ui")
    active$server <-  paste0("mod_body_", input$active_tab, "_server")
    active$tab <- input$active_tab
    active$dark_mode <- input$dark_mode
    active$is_youth <- grepl("youth", input$active_tab, ignore.case = TRUE)
    message("Active tab: ", active$tab, " | Is Youth: ", active$is_youth)
  })

  # Top-level Modules
  mod_navbar_server("navbar")
  mod_sidebar_server("sidebar")
  mod_body_server("body", active$is_youth)
  mod_theme_server("color_theme")
  
}
