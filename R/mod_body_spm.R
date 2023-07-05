#' body_spm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_spm_ui <- function(id){
  ns <- NS(id)
  # Toggle BoS or Mahoning
  
#        title = "Metric 1b: Length of Time Homeless",

#      title = "Metrics 2a1 & 2b1: Clients Returning to Homelessness After 

      # title = "Metric 3.1: January 2019 and January 2020 PIT Counts",

      # title = "Metrics 7b1 & 7b2: Exits to or Retention of Permanent Housing",

}
    
#' body_spm Server Functions
#'
#' @noRd 
mod_body_spm_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_body_spm_ui("body_spm_1")
    
## To be copied in the server
# mod_body_spm_server("body_spm_1")
