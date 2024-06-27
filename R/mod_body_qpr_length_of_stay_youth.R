#' body_length_of_stay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_length_of_stay_youth_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}

#' body_length_of_stay Server Functions
#'
#' @noRd 
mod_body_qpr_length_of_stay_youth_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Youth Length of Stay"))
}

## To be copied in the UI
# mod_body_length_of_stay_ui("body_length_of_stay_1")

## To be copied in the server
# mod_body_length_of_stay_server("body_length_of_stay_1")
