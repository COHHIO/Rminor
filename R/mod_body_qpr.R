mod_body_qpr_ui <- function(id, choices = NULL, date_choices = NULL, ns = rlang::caller_env()$ns) {
  ns <- NS(id)
  
  .id <- strip_id(id)  # Ensures that the ID is correctly set up
  .defaults <- purrr::compact(list(
    Dates = list(
      inputId = ns("date_range"),
      start = lubridate::floor_date(Sys.Date() - lubridate::dmonths(4), "month"),
      end = Sys.Date()
    ),
    Regions = list(
      inputId = ns("region"),
      choices = qpr_tab_choices[[.id]]$choices,  # Default to the first choice
      selected = qpr_tab_choices[[.id]]$choices[1],
      multiple = FALSE
    )
  ))
  
  shiny::tagList(
    ui_header_row(ns("header")),
    ui_row(
      title = "Report Details",
      bs4Dash::bs4Accordion(
        id = "about",
        bs4Dash::bs4AccordionItem(
          title = "Ohio BoS Performance Management Plan",
          tags$p(a("Ohio BoS 2024 Performance Management Plan", href = "https://cohhio.org/wp-content/uploads/2024/04/Ohio-BoSCoC-2024-PMP_Final.pdf")),
          collapsed = TRUE
        ),
        bs4Dash::bs4AccordionItem(
          title = "Performance Goals / How Measures Calculated",
          DT::DTOutput(ns("details_output")),  # Use ns() to namespace the output
          collapsed = TRUE
        )
      )
    ),
    ui_row(
      if (shiny::isTruthy(.defaults$Dates))
        do.call(ui_date_range, .defaults$Dates),
      if (shiny::isTruthy(.defaults$Regions))
        do.call(ui_picker_program, .defaults$Regions)
    ),
    ui_row(
      bs4Dash::infoBoxOutput(ns("infobox_output"), width = 12)  # Correctly namespaced infobox output
    )
  )
}


mod_body_qpr_server <- function(id, header, calculate_expr, infobox_expr, details_expr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure that data_env is only evaluated when inputs are available
    observeEvent({
      input$date_range
      input$region
    }, {
      message("Inputs available: date_range = ", input$date_range, ", region = ", input$region)
      req(input$date_range, input$region)  # Ensure inputs are available
      
      # Header
      output$header <- shiny::renderUI({
        req(input$date_range)
        server_header(header, date_range = input$date_range)
      })
      
      # Reactive data processing with logging
      data_env <- reactive({
        message("Evaluating data for measure: ", id)
        
        # Evaluate the calculation expression
        result <- tryCatch({
          eval(calculate_expr)
        }, error = function(e) {
          message("Error in data calculation: ", e$message)
          NULL
        })
        
        if (is.null(result)) {
          message("No data calculated for: ", id)
        } else {
          message("Data calculated for: ", id, " with ", nrow(result), " rows.")
        }
        
        return(result)
      })
      
      # Infobox generation
      output$infobox_output <- bs4Dash::renderbs4InfoBox({
        req(data_env())  # Ensure data is available
        message("Rendering infobox for measure: ", id)
        
        tryCatch({
          eval(infobox_expr)
        }, error = function(e) {
          message("Error rendering infobox: ", e$message)
          NULL
        })
      })
      
      # Data table generation
      output$details_output <- DT::renderDataTable({
        req(data_env())  # Ensure data is available
        message("Rendering details for measure: ", id)
        
        tryCatch({
          eval(details_expr)
        }, error = function(e) {
          message("Error rendering details: ", e$message)
          NULL
        })
      })
    })
  })
}

