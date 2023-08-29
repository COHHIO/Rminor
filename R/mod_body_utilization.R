#' body_utilization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_utilization_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(ui_picker_program(choices = sort(utilization_bed()$ProjectName),
                             multiple = FALSE),
           ui_date_range(start = lubridate::floor_date(Sys.Date(), unit = "month") -
                           lubridate::years(1))),
    plotly::plotlyOutput(ns("bed_plot")),
    htmltools::br(),
    ui_row(
      title = "Info",
      rlang::exec(
        bs4Dash::bs4Accordion,
        id = "utilization_info",
        !!!purrr::map2(
          utilization_notes,
          c(
            "What is Bed Utilization?",
            "What is Unit Utilization?",
            "Methodology"
          ),
          ~ bs4Dash::bs4AccordionItem(title = .y, HTML(.x))
        )
      )
    )
 
  )
}
    
#' body_utilization Server Functions
#'
#' @noRd 
mod_body_utilization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$header <- shiny::renderUI(server_header("Bed and Unit Utilization"))
    
    output$bed_plot <-
      plotly::renderPlotly({
        req(input$date_range, input$program)
        ReportingPeriod <- lubridate::interval(input$date_range[1], input$date_range[2])
        
        bed_plot <- utilization_bed() |> 
          tidyr::gather("Month",
                        "Utilization",
                        -ProjectID,
                        -ProjectName,
                        -ProjectType,
                        -accuracy) |>
          dplyr::mutate(Month = lubridate::parse_date_time(orders = "bY", Month)) |>
          dplyr::filter(ProjectName == input$program,
                        Month %within% ReportingPeriod) |>
          dplyr::mutate(
            Month = lubridate::floor_date(Month, unit = "month"),
            Bed = Utilization,
            Utilization = NULL
          )
        
        unit_plot <- utilization_unit() |> 
          tidyr::gather("Month",
                        "Utilization",
                        -ProjectID,
                        -ProjectName,
                        -ProjectType,
                        -accuracy) |>
          dplyr::mutate(Month = lubridate::parse_date_time(orders = "bY", Month)) |>
          dplyr::filter(ProjectName == input$program,
                        Month %within% ReportingPeriod) |>
          dplyr::mutate(
            Month = lubridate::floor_date(Month, unit = "month"),
            Unit = Utilization,
            Utilization = NULL
          )
        
        unit_plot |>
          dplyr::full_join(bed_plot,
                           by = c("ProjectID", "ProjectName", "ProjectType", "Month")) |>
          plotly::plot_ly(x = ~Month) |>
          plotly::add_trace(y = ~ Unit,
                            name = "Unit Utilization",
                            type = "scatter",
                            mode = "lines+markers",
                            hoverinfo = 'y') |>
          plotly::add_trace(y = ~Bed,
                            name = "Bed Utilization",
                            type = "scatter",
                            mode = "lines+markers",
                            hoverinfo = 'y') |>
          plotly::layout(yaxis = list(
            title = "Utilization",
            tickformat = "%",
            range = c(0, 2)
          ),
          margin = list(
            t = 100
          ),
          title = paste("Bed and Unit Utilization",
                        "\n", 
                        input$program,
                        "\n", 
                        input$date_range[1], 
                        "to", 
                        input$date_range[2]))
        
      })
 
  })
}
    
## To be copied in the UI
# mod_body_utilization_ui("body_utilization_1")
    
## To be copied in the server
# mod_body_utilization_server("body_utilization_1")
