.qbegin <- lubridate::floor_date(Sys.Date(), "quarter")
strip_id <- function(id) {
  stringr::str_remove(id, "^body\\-program_details\\-")
}

#' @family QPR
#' @title QPR_tabItem UI Function
#' @description A shiny Module to generate the QPR tabitems.
#' @param id Internal parameters for {shiny}.
#' @param choices \code{(named list)} of choices of program types for
#'   the radio picker. Default does not show the UI item - choices must be
#'   provided for the picker to show.
#' @param date_choices \code{(named list)} of choices of date ranges for
#'   the date picker.
#' @param region_choices \code{(named list)} of choices of regions for the
#'   region drop-down selector.
#' @param radio_mean \code{(logical)} whether to show the Mean/Median based
#'   average radio UI.

mod_qpr_ui <- function(id, choices = NULL, date_choices = NULL, ns = rlang::caller_env()$ns) {
  force(ns)
  .id <- strip_id(id)
  .defaults <- purrr::compact(list(
    Dates = if (!isFALSE(date_choices)) list(
      inputId = ns("date_range"),
      start = lubridate::floor_date(lubridate::as_date(.qbegin - lubridate::dmonths(4)), "quarter"),
      end = .qbegin
    ),
    Regions = if (!isFALSE(choices))
      list(
        inputId = ns("region"),
        choices = qpr_tab_choices[[.id]]$choices,
        multiple = FALSE
      )
  ))
  .user <- purrr::compact(list(
    Dates = date_choices,
    Regions = choices
  ))
  if (UU::is_legit(.user)) {
    .defaults[names(.user)] <- purrr::map2(.defaults[names(.user)], .user, ~{
      purrr::list_modify(.x, !!!.y)
    })
  }
  
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
          iterate(qpr_expr[[.id]]$details, DT::DTOutput, "dt_measures"),
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
      iterate(qpr_expr[[.id]]$infobox, bs4Dash::infoBoxOutput, "ib_summary", width = 12)
    )
  )
}



#' @family QPR
#' @title QPR Server Functions
#' @description A shiny server Module to generate the header, slider, pickers 
#' and plot for each tabitem.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param header \code{(character)} The header text passed to the initial 
#' \link[shiny]{h2} tag in the header.
#' @param ... Additional \code{(list/shiny.tag.list/shiny.tag)}s  to be appended 
#' to the header after the \link[shiny]{h2} tag with `header`. Defaults to
#'  \code{list(h4(input$region), h4(paste(ReportStart, "to", ReportEnd)))} if 
#'  unspecified. 

mod_qpr_server <- function(id, header, calculate_expr, infobox_expr, details_expr, is_youth = FALSE, ...) {
  
  .id <- strip_id(id)

  if (missing(header))
    rlang::abort("Must provide header for mod_QPR_server(",id,")")
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message("mod_qpr_server called for id: ", .id)
    
    # Choose the correct expression list based on whether it's a youth measure
    expr_list <- if (is_youth) qpr_expr_youth else qpr_expr
    
    # Header
    output$header <- shiny::renderUI({
      req(input$date_range)
      server_header(header, date_range = input$date_range, ...)
    })
    
    # Process Data
    data_env <- shiny::reactive({
      eval(calculate_expr)
    }, quoted = TRUE)
    
    # Infobox generation
    if (UU::is_legit(qpr_expr[[.id]]$infobox)) {
      if (rlang::is_list(qpr_expr[[.id]]$infobox)) {
        x <- qpr_expr[[.id]]$infobox
      } else {
        x <- list(qpr_expr[[.id]]$infobox)
      }
      for (i in seq_along(x)) {
        # Evaluate the expression and print debugging information
        # Try to evaluate the infobox expression directly
        tryCatch({
          result <- eval(x[[i]])
          message("Infobox content: ", result)  # Debugging: Print the result of the evaluation
          output[[paste0("ib_summary", i)]] <- bs4Dash::renderbs4InfoBox(result, quoted = TRUE)
        }, error = function(e) {
          message("Error in evaluating infobox for id: ", .id, " - ", e$message)
        })
      } 
    } else {
      message("No infobox found for id: ", .id)
    }
    
    # Details section
    if (rlang::is_list(qpr_expr[[.id]]$details)) {
      for (i in seq_along(qpr_expr[[.id]]$details)) {
        output[[paste0("dt_measures", i)]] <- DT::renderDT(server = FALSE, qpr_expr[[.id]]$details[[i]], quoted = TRUE)
      }
    } else {
      output$dt_measures1 <- DT::renderDT(server = FALSE, qpr_expr[[.id]]$details, quoted = TRUE)
    }
  })
}
