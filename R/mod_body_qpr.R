mod_body_qpr_ui <- function(id, choices = NULL, date_choices = NULL, ns = rlang::caller_env()$ns,
                            is_youth = FALSE) {
  ns <- NS(id)

  .id <- strip_id(id, is_youth = is_youth)  # Ensures that the ID is correctly set up
  
  
  # Adjust choices based on is_youth flag
  region_choices <- if (is_youth) {
    qpr_tab_choices[[paste0(.id, "_youth")]]$choices  # Youth-specific choices
  } else {
    qpr_tab_choices[[.id]]$choices  # Standard choices
  }
  
  
  .defaults <- purrr::compact(list(
    Dates = list(
      inputId = ns("date_range"),
      start = lubridate::floor_date(Sys.Date() - lubridate::dmonths(4), "month"),
      end = Sys.Date()
    ),
    Regions = list(
      inputId = ns("region"),
      choices = region_choices,  # Default to the first choice
      selected = region_choices[1],
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
          tags$p(a("Ohio BoS 2025 Performance Management Plan", href = "https://cohhioorg.sharepoint.com/:b:/r/sites/HRDocuments/Shared%20Documents/HMIS%20%26%20BoSCoC/HMIS%20%26%20BoSCoC%20Planning/Performance%20and%20Outcomes/2025%20PMP/Ohio%20BoSCoC%202025%20PMP.pdf?csf=1&web=1&e=8v7708")),
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


mod_body_qpr_server <- function(id, header, calculate_expr, infobox_expr, details_expr, is_youth = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure that data_env is only evaluated when inputs are available
    observeEvent({
      input$date_range
      input$region
    }, {
      req(input$date_range, input$region)  # Ensure inputs are available
      
      # Header
      output$header <- shiny::renderUI({
        req(input$date_range)
        server_header(header, date_range = input$date_range)
      })
      
      # Reactive data processing with logging
      data_env <- reactive({
        
        # Select youth or standard calculation expression
        selected_expr <- if (is_youth) qpr_expr[[paste0(id, "_youth")]]$expr else calculate_expr
        
        # Evaluate the calculation expression
        result <- tryCatch({
          eval(selected_expr)
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
        
        # Select youth or standard infobox expression
        selected_infobox <- if (is_youth) qpr_expr[[paste0(id, "_youth")]]$infobox else infobox_expr
        
        tryCatch({
          eval(selected_infobox)
        }, error = function(e) {
          message("Error rendering infobox: ", e$message)
          NULL
        })
      })
      
      # Data table generation
      output$details_output <- DT::renderDataTable({
        req(data_env())  # Ensure data is available
        message("Rendering details for measure: ", id)
        
        # Select youth or standard details expression
        selected_details <- if (is_youth) qpr_expr[[paste0(id, "_youth")]]$details else details_expr
        
        tryCatch({
          eval(selected_details)
        }, error = function(e) {
          message("Error rendering details: ", e$message)
          NULL
        })
      })
    })
  })
}

