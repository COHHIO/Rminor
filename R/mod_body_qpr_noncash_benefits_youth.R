#' body_qpr_noncash_benefits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_noncash_benefits_youth_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_qpr_noncash_benefits Server Functions
#'
#' @noRd 
mod_body_qpr_noncash_benefits_youth_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Youth Non-Cash Benefits at Exit"))
}
    
## To be copied in the UI
# mod_body_qpr_noncash_benefits_ui("body_qpr_noncash_benefits_1")
    
## To be copied in the server
# mod_body_qpr_noncash_benefits_server("body_qpr_noncash_benefits_1")
