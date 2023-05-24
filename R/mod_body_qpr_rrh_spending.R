#' body_qpr_rrh_spending UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_rrh_spending_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_qpr_rrh_spending Server Functions
#'
#' @noRd 
mod_body_qpr_rrh_spending_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "RRH Spending"))  
}
    
## To be copied in the UI
# mod_body_qpr_rrh_spending_ui("body_qpr_rrh_spending_1")
    
## To be copied in the server
# mod_body_qpr_rrh_spending_server("body_qpr_rrh_spending_1")
