#' body_qpr_health_insurance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_health_insurance_youth_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_qpr_health_insurance Server Functions
#'
#' @noRd 
mod_body_qpr_health_insurance_youth_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Youth Health Insurance at Exit"))
}
    
## To be copied in the UI
# mod_body_qpr_health_insurance_ui("body_qpr_health_insurance_1")
    
## To be copied in the server
# mod_body_qpr_health_insurance_server("body_qpr_health_insurance_1")
