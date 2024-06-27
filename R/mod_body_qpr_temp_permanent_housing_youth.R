#' body_qpr_temp_permanent_housing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_temp_permanent_housing_youth_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_qpr_temp_permanent_housing Server Functions
#'
#' @noRd 
mod_body_qpr_temp_permanent_housing_youth_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Youth Exits to Temporary or Permanent Housing"))
}
    
## To be copied in the UI
# mod_body_qpr_temp_permanent_housing_ui("body_qpr_temp_permanent_housing_1")
    
## To be copied in the server
# mod_body_qpr_temp_permanent_housing_server("body_qpr_temp_permanent_housing_1")
