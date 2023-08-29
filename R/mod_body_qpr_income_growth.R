#' body_qpr_income_growth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_income_growth_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_qpr_income_growth Server Functions
#'
#' @noRd 
mod_body_qpr_income_growth_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Income Growth"))
}
    
## To be copied in the UI
# mod_body_qpr_income_growth_ui("body_qpr_income_growth_1")
    
## To be copied in the server
# mod_body_qpr_income_growth_server("body_qpr_income_growth_1")
