#' body_performance_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_performance_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(
        DT::dataTableOutput(ns("ps_1")),
        title = "Measure 1: Length of Stay"
    )
  )


}
    
#' body_spm Server Functions
#'
#' @noRd 
mod_body_performance_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI({
      server_header(
        "Performance Summary"
      )
    })
      
      
      output$ps_1 <- DT::renderDT(server = FALSE, {
        qpr_leavers() |> 
          HMIS::exited_between(as.Date("2023-01-01"), as.Date("2023-12-31")) |> 
          dplyr::filter(((
            !is.na(MoveInDateAdjust) & ProjectType == 13
          ) |
            (
              !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
            ))) |> dplyr::group_by(ProjectName) |>
          dplyr::summarise(Average = round(mean(DaysinProject), 1),
                           Median = median(DaysinProject), clients = dplyr::n(), .groups = "drop_last") |> 
          datatable_default()
      })
      
      
  })
}
    
## To be copied in the UI
# mod_body_performance_summary_ui("body_spm_1")
    
## To be copied in the server
# mod_body_performance_server("body_spm_1")
