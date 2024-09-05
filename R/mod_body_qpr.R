# Generic UI Function
mod_body_qpr_ui <- function(id, measure_name, measure_label, choices) {
  ns <- NS(id)
  
  bs4Dash::tabBox(width = 12,
                  tabPanel(measure_label,
                           h3(paste(measure_label, "Summary")),
                           tagList(
                             selectInput(
                               inputId = ns(paste0("project_type_", measure_name)),
                               label = "Select your Project Type",
                               choices = choices,
                               selected = choices[1],
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns(paste0("date_range_", measure_name)),
                                            label = "Date Range",
                                            start = Sys.Date() - 30,
                                            end = Sys.Date()
                             ),
                             plotly::plotlyOutput(ns(paste0("plot_", measure_name))),
                             br(),
                             DT::dataTableOutput(ns(paste0("table_", measure_name))),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns(paste0("details_", measure_name)))
                           )
                  )
  )
}

# Generic Server Function
mod_body_qpr_server <- function(id, measure_name, calculate_expr, infobox_expr, details_expr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Placeholder for calculation expressions
    data <- reactive({
      eval(calculate_expr)
    })
    
    # Infobox generation
    output[[paste0("infobox_", measure_name)]] <- renderUI({
      eval(infobox_expr)
    })
    
    # Data table output
    output[[paste0("details_", measure_name)]] <- DT::renderDataTable({
      eval(details_expr)
    })
    
    # Plot outputs (example, adjust as necessary)
    output[[paste0("plot_", measure_name)]] <- plotly::renderPlotly({
      plotly::plot_ly(data(), x = ~var1, y = ~var2)  # Adjust plot as necessary
    })
  })
}
