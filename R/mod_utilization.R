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
    ui_row(box(
      pickerInput(
        inputId = "providerListUtilization",
        choices = c(sort(utilization_bed$ProjectName)),
        options = pickerOptions(dropupAuto = FALSE,
                                liveSearch = TRUE),   
        selected = sample_n(utilization_bed |> select(ProjectName), 1),
        width = "100%"
      ),
      airDatepickerInput(
        inputId = "utilizationDate",
        label = "Report End Month",
        maxDate = ymd(floor_date(update_date, unit = "month") - 1),
        minDate = ymd(floor_date(update_date - 335, unit = "month")),
        dateFormat = "MM yyyy",
        view = "month",
        value = ymd(floor_date(update_date, unit = "month") - 1),
        minView = "months",
        addon = "none",
        autoClose = TRUE,
        width = '25%'
      ),
      width = 12
    )), 
    plotlyOutput("bedPlot"),
    br(),
    ui_row(
      title = "Summary",
      fluidRow(
        bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
          "infobox_bed"
        ), width = '100%')),
        bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
          "infobox_unit"
        ), width = '100%')),
        bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
          "infobox_utilization"
        ), width = '100%'))
      ),
      width = 12
    )
  )
}

mod_utilization_server <- function(input, output, session){
  ns <- session$ns
  
  output$header <- shiny::renderUI(server_header("Bed and Unit Utilization", 
                                                 format(input$date_range, "%B %Y"), 
                                                 shiny::tags$p(shiny::icon("exclamation-triangle", 
                                                                           style = "display:inline-block; 
                                                                           color: #ffc107;"), 
                                                               "During this time, congregate facilities should 
                                                               be aiming to deconcentrate. If this causes 
                                                               fluctuations in Utilization, that is okay. 
                                                               Please continue to keep your clients safe.", 
                                                               status = "warning")))
  
  output$bedPlot <-
    renderPlotly({
      ReportEnd <- ymd(input$utilizationDate) 
      ReportStart <- floor_date(ReportEnd, unit = "month") -
        years(1) +
        months(1)
      ReportingPeriod <- interval(ReportStart, ReportEnd)
      
      Provider <- input$providerListUtilization
      
      bedPlot <- utilization_bed |> 
        gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) |>
        filter(ProjectName == Provider,
               
               mdy(Month) %within% ReportingPeriod) |>
        mutate(
          Month = floor_date(mdy(Month), unit = "month"),
          Bed = Utilization,
          Utilization = NULL
        )
      
      unitPlot <- utilization_unit |> 
        gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) %>%
        filter(ProjectName == Provider,
               mdy(Month) %within% ReportingPeriod) |>
        mutate(
          Month = floor_date(mdy(Month), unit = "month"),
          Unit = Utilization,
          Utilization = NULL
        )
      
      utilizationPlot <- unitPlot |>
        full_join(bedPlot,
                  by = c("ProjectID", "ProjectName", "ProjectType", "Month")) 
      
      plot_ly(utilizationPlot, 
              x = ~Month) |>
        add_trace(y = ~ Unit,
                  name = "Unit Utilization",
                  type = "scatter",
                  mode = "lines+markers",
                  hoverinfo = 'y') |>
        add_trace(y = ~Bed,
                  name = "Bed Utilization",
                  type = "scatter",
                  mode = "lines+markers",
                  hoverinfo = 'y') |>
        layout(yaxis = list(
          title = "Utilization",
          tickformat = "%",
          range = c(0, 2)
        ),
        margin = list(
          t = 100
        ),
        title = paste("Bed and Unit Utilization",
                      "\n", 
                      Provider,
                      "\n", 
                      format(ymd(ReportStart), "%B %Y"), 
                      "to", 
                      format(ymd(ReportEnd), "%B %Y")))
      
    })
}
    
## To be copied in the UI
# mod_utilization_ui("utilization_1")
    
## To be copied in the server
# mod_utilization_server("utilization_1")
