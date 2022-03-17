#' utilization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# mod_UI
mod_utilization_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    ui_header_row(),
    ui_row(ui_picker_program(choices = sort(utilization_bed()$ProjectName)),
           ui_date_range(start = lubridate::floor_date(Sys.Date(), unit = "month") -
                           lubridate::years(1) +
                           lubridate::months(1))),
    plotly::plotlyOutput(ns("bed_plot")),
    htmltools::br(),
    ui_row(
      title = "Summary",
        bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
          "infobox_bed"
        ), width = '100%')),
        bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
          "infobox_unit"
        ), width = '100%')),
        bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
          "infobox_utilization"
        ), width = '100%')),
    )
  )
}

mod_utilization_server <- function(input, output, session){
  ns <- session$ns
  
  output$header <- shiny::renderUI(server_header("Bed and Unit Utilization", 
                                                 shiny::tags$p(shiny::icon("exclamation-triangle", 
                                                                           style = "display:inline-block; 
                                                                           color: #ffc107;"), 
                                                               "During this time, congregate facilities should 
                                                               be aiming to deconcentrate. If this causes 
                                                               fluctuations in Utilization, that is okay. 
                                                               Please continue to keep your clients safe.", 
                                                               status = "warning")))
  
  output$bed_plot <-
    plotly::renderPlotly({
      ReportingPeriod <- lubridate::interval(input$date_range[1], input$date_range[2])
      
      bed_plot <- utilization_bed() |> 
        tidyr::gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) |>
        dplyr::filter(ProjectName == input$program,
               
               Month %within% ReportingPeriod) |>
        dplyr::mutate(
          Month = lubridate::floor_date(Month, unit = "month"),
          Bed = Utilization,
          Utilization = NULL
        )
      
      unitPlot <- utilization_unit |> 
        tidyr::gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) %>%
        dplyr::filter(ProjectName == input$program,
               Month %within% ReportingPeriod) |>
        dplyr::mutate(
          Month = lubridate::floor_date(Month, unit = "month"),
          Unit = Utilization,
          Utilization = NULL
        )
      
      unitPlot |>
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
  
  output$current_bed_utilization <-
    if (nrow(utilization() %>%
             dplyr::filter(
               ProjectName == input$providerList &
               ProjectType %in% c(1:3, 8, 9)
             )) > 0) {
      shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          title = "Current Bed Utilization",
          subtitle = paste(
            utilization() %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(Clients),
            "Clients in",
            utilization() %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(BedCount),
            "Beds"
          ),
          color = "purple",
          icon = shiny::icon("bed"),
          utilization() %>%
            dplyr::filter(ProjectName == input$providerList) %>%
            dplyr::select(BedUtilization)
        )
      })
    }
}
    
## To be copied in the UI
# mod_utilization_ui("utilization_1")
    
## To be copied in the server
# mod_utilization_server("utilization_1")
