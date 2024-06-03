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
  bs4Dash::tabBox(width = 12,
             tabPanel("Length of Stay",
                      h3("Length of Stay Summary"),
                      tagList(
                        ui_header_row(),
                        plotly::plotlyOutput(ns("ps_plot_1")),
                        ui_row(
                          DT::dataTableOutput(ns("ps_table_1")),
                          title = "Measure 1: Length of Stay"
                        )
                      )
             ),
             tabPanel("Tab 2",
                      h3("Content for Tab 2"),
                      p("This is where the content for Tab 2 goes.")
             ),
             tabPanel("Tab 3",
                      h3("Content for Tab 3"),
                      p("This is where the content for Tab 3 goes.")
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
      
    output$ps_plot_1 <-
      plotly::renderPlotly({
        length_of_stay <- qpr_leavers() |> 
          HMIS::exited_between(as.Date("2023-01-01"), as.Date("2023-12-31")) |> 
          dplyr::filter(((
            !is.na(MoveInDateAdjust) & ProjectType == 13
          ) |
            (
              !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
            ))) |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(Average = round(mean(DaysinProject), 1),
                           Median = median(DaysinProject), clients = dplyr::n(), .groups = "drop_last")
        qpr_plotly(length_of_stay, "Length of Stay", hover = hover)
  })
    
    
      output$ps_table_1 <- DT::renderDT(server = FALSE, {
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
      
      output$ps_plot_1 <-
        plotly::renderPlotly({
          length_of_stay <- qpr_leavers() |> 
            HMIS::exited_between(as.Date("2023-01-01"), as.Date("2023-12-31")) |> 
            dplyr::filter(((
              !is.na(MoveInDateAdjust) & ProjectType == 13
            ) |
              (
                !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
              ))) |> 
            dplyr::group_by(ProjectName) |>
            dplyr::summarise(Average = round(mean(DaysinProject), 1),
                             Median = median(DaysinProject), clients = dplyr::n(), .groups = "drop_last")
          qpr_plotly(length_of_stay, "Length of Stay", hover = hover)
        })
      
      
      output$ps_table_1 <- DT::renderDT(server = FALSE, {
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
