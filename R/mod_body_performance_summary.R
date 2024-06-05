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
             tabPanel("Exits to Permanent Housing",
                      h3("Exits Summary"),
                      tagList(
                        ui_header_row(),
                        plotly::plotlyOutput(ns("ps_plot_2")),
                        ui_row(
                          DT::dataTableOutput(ns("ps_table_2")),
                          title = "Measure 2: Exits"
                        )
                      )
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
    
    # Length of Time
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
      
      # Exits to Permanent Housing
      qpr_leavers <- qpr_leavers()
      .exited <- qpr_leavers |> 
        HMIS::exited_between(as.Date("2023-01-01"), as.Date("2023-12-31"), lgl = TRUE)
      .served <- qpr_leavers |> 
        HMIS::served_between(as.Date("2023-01-01"), as.Date("2023-12-31"), lgl = TRUE)
      
      .psh_hp <- qpr_leavers$ProjectType %in% c(3, 9, 12)
      .es_th_sh_out_rrh <- qpr_leavers$ProjectType %in% c(0, 1, 2, 4, 8, 13)
      
      SuccessfullyPlaced <- dplyr::filter(qpr_leavers,
                                          ((ProjectType %in% c(3, 9, 13) &
                                              !is.na(MoveInDateAdjust)) |
                                             ProjectType %in% c(0, 1, 2, 4, 8, 12)
                                          ) &
                                            # excluding non-mover-inners
                                            (((DestinationGroup == "Permanent" |
                                                 #exited to ph or still in PSH/HP
                                                 is.na(ExitDate)) &
                                                .psh_hp & # PSH & HP
                                                .served
                                            ) |
                                              (
                                                DestinationGroup == "Permanent" & # exited to ph
                                                  .es_th_sh_out_rrh &
                                                  .exited
                                              )
                                            ))
      
      # calculating the total households to compare successful placements to
      TotalHHsSuccessfulPlacement <- 
        dplyr::filter(qpr_leavers,
                      (.served & .psh_hp) # PSH & HP
                      |
                        (.exited & .es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
        )
      
      TotalCount <- TotalHHsSuccessfulPlacement |> 
        dplyr::count(ProjectName) |> 
        dplyr::rename(clients = n)
      
      SuccessCount <- SuccessfullyPlaced |>
        dplyr::count(ProjectName) |>
        dplyr::rename(success_clients = n)
      
      exits <- TotalCount |> 
        dplyr::left_join(SuccessCount, by = "ProjectName") |>
        {\(.) {replace(.,is.na(.),0)}}() |>
        dplyr::mutate(Average = success_clients/clients)
      
      output$ps_plot_2 <-
        plotly::renderPlotly({
            qpr_plotly(exits, "Exits to Permanent Housing", hover = hover)
        })
      
      
      output$ps_table_2 <- DT::renderDT(server = FALSE, {
        exits |> 
          datatable_default()
      })
      
      
  })
}
    
## To be copied in the UI
# mod_body_performance_summary_ui("body_spm_1")
    
## To be copied in the server
# mod_body_performance_server("body_spm_1")
