.qbegin <- lubridate::floor_date(Sys.Date(), "quarter")
strip_id <- function(id) {
  stringr::str_remove(id, "^(?:body\\-)?body\\_qpr\\_")
}
#' @family QPR
#' @title QPR_tabItem UI Function
#' @description A shiny Module to generate the QPR tabitems.
#' @param id Internal parameters for {shiny}.
#' @param project_choices \code{(named list)} of choices of program types for
#'   the radio picker. Default does not show the UI item - choices must be
#'   provided for the picker to show.
#' @param region_choices \code{(named list)} of choices of regions for the
#'   region drop-down selector. Default does not show the UI item - choices must
#'   be provided for the picker to show options
#QUESTION Should there be defaults for options?
#' @param radio_mean \code{(logical)} whether to show the Mean/Median based
#'   average radio UI


mod_qpr_ui <- function(id, choices = NULL, date_choices = NULL,
                       ns = rlang::caller_env()$ns) {
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
    # if there are 
    .defaults[names(.user)] <- purrr::map2(.defaults[names(.user)], .user, ~{
      # replace default params with those supplied by user on a param by param 
      # basis, retaining defaults.
      purrr::list_modify(.x, !!!.y)
    })
  }

 do.call(ui_date_range, .defaults$Dates)
  # tabItem Output ----
  shiny::tagList(
    ui_header_row(ns("header")),
    ui_row(
      title = "Report Details",
      bs4Dash::bs4Accordion(
        id = "about",
        bs4Dash::bs4AccordionItem(
          title = "Ohio BoS Performance Management Plan",
          tags$p(a("Ohio BoS 2024 Performance Managment Plan", href = "https://cohhio.org/wp-content/uploads/2024/04/Ohio-BoSCoC-2024-PMP_Final.pdf")),
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
        do.call(ui_date_range, .defaults$Dates)
      ,
      if (shiny::isTruthy(.defaults$Regions))
        do.call(ui_picker_program, .defaults$Regions)
    )
    ,
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



mod_qpr_server <- function(id, header, ...) {
  .id <- strip_id(id)
  if (missing(header))
    rlang::abort("Must provide header for mod_QPR_server(",id,")")
  function(input, output, session){
    ns <- session$ns
    
    # Header
    output$header <- shiny::renderUI({
      req(input$date_range)
      server_header(header, date_range = input$date_range, ...)
    })

    # Process Data
    data_env <- shiny::reactive(qpr_expr[[.id]]$expr, quoted = TRUE)
    if (UU::is_legit(qpr_expr[[.id]]$infobox)) {
      if (rlang::is_list(qpr_expr[[.id]]$infobox))
        x <- qpr_expr[[.id]]$infobox
      else
        x <- list(qpr_expr[[.id]]$infobox)
      for (i in seq_along(x)) {
        output[[paste0("ib_summary",i)]] <- bs4Dash::renderbs4InfoBox(x[[i]], quoted = TRUE)
      }  
    }
    
    

    
    if (rlang::is_list(qpr_expr[[.id]]$details)) {
      for (i in seq_along(qpr_expr[[.id]]$details)) {
        output[[paste0("dt_measures",i)]] <- DT::renderDT(server = FALSE, qpr_expr[[.id]]$details[[i]], quoted = TRUE)
      }
    } else {
      output$dt_measures1 <- DT::renderDT(server = FALSE, qpr_expr[[.id]]$details, quoted = TRUE)
    }
    
  }
    
    


}
