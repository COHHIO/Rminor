#' body_qpr_reentries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_reentries_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_qpr_rrh_placement Server Functions
#'
#' @noRd 
mod_body_qpr_reentries_server <- function(id){
  moduleServer( id, mod_qpr_server(id, header = "Entries into the Homeless System"))
}
    
## To be copied in the UI
# mod_body_qpr_reentries_ui("body_qpr_rrh_placement_1")
    
## To be copied in the server
# mod_body_qpr_reentries_server("body_qpr_rrh_placement_1")
