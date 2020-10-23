#' @family QPR
#' @title QPR_tabItem UI Function
#' @description A shiny Module to generate the QPR tabitems.
#' @param id Internal parameters for {shiny}.
#' @param project_choices \code{(named list)} of choices of program types for the radio picker. Default does not show the UI item - choices must be provided for the picker to show.
#' @param region_choices \code{(named list)} of choices of regions for the region drop-down selector. Default does not show the UI item - choices must be provided for the picker to show options
#QUESTION Should there be defaults for options?
#' @param radio_mean \code{(logical)} whether to show the Mean/Median based average radio UI
#' @importFrom shiny NS tagList 
#' @importFrom rlang parse_expr


mod_QPR_tabItem_ui <- function(id, project_choices, region_choices, radio_mean = FALSE){
  ns <- NS(id)
  # Create labeled Quarter List
  # .quarter_labels <- rev(unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4))
  # slider_choices <- rev(purrr::map(.qs, ~lubridate::yq(.x) - lubridate::days(1)))
  # names(slider_choices) <- .quarter_labels
    
    shinydashboard::tabItem(
      tabName = ns("Tab"),
      shiny::fluidRow(shinydashboard::box(shiny::htmlOutput(ns("header")), width = 12)),
      shinyWidgets::chooseSliderSkin("Round"),
      shinyWidgets::setSliderColor("#56B4E9", 1),
      shinyWidgets::sliderTextInput(ns("slider"),
                                    "",
                                     unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4)
                                    ,
                                    selected = zoo::Sys.yearqtr() - 1 / 4),
      if (!missing(project_choices)) {
        shinyWidgets::prettyRadioButtons(
        inputId = ns("ProjectType"),
        label = "Program Type",
        thick = TRUE,
        animation = "pulse",
        status = "info",
        choices = project_choices,
        selected = NULL
      )},
      if (!missing(region_choices)) {
        shinyWidgets::pickerInput(
        inputId = ns("Region"),
        "Select Region(s)",
        choices = region_choices,
        options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                              actionsBox = TRUE),
        multiple = TRUE,
        selected = NULL
      )},
      if (radio_mean) {
      shinyWidgets::prettyRadioButtons(
        inputId = ns("radio_mean"),
        label = "",
        thick = TRUE,
        animation = "pulse",
        status = "info",
        choices = c("Average Days", "Median Days"),
        selected = "Average Days"
      )},
      # verbatimTextOutput("res"),
      plotly::plotlyOutput(ns("plot"))
    ) 
}

#' @family QPR
#' @title QPR Server Functions
#' @description A shiny server Module to generate the header, slider, pickers and plot for each tabitem.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param header \code{(character)} The human legible name for the tabitem header.
#' @importFrom shiny NS tagList 
#' @importFrom rlang parse_expr eval_bare
#' @importFrom purrr keep


mod_QPR_server <- function(id, header, input, output, session){
  if (missing(header)) {
    rlang::abort("Must provide header for mod_QPR_server(",id,")")
  }
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Process Slider Inputs
    Report <- eventReactive(input$slider, {
      list(
      Start = qstart_date(input$slider),
      End = qend_date(input$slider)
      )
    })
    ProjectType <- eventReactive(req(input$ProjectType), {
      # The RadioPicker input returns a character regardless of the type of objects passed to the UI element. If the object returned is meant to be a list or vector, ie  Permanent Supportive Housing = c(3,9), it returns a character "list(`3` = 3, `9` = 9)". This must then be parsed into an actual list.
      .pt <- unlist(eval(rlang::parse_expr(input$ProjectType)))
      # message(paste("ProjectType-class:",class(.pt)))
      # message(paste("ProjectType:",.pt))
      #browser(expr = is.na(.pt))
      # Get the selected project type
      ProjectType <- purrr::keep(choices_project_type, ~{any(unlist(.x) %in% .pt)})
      message(paste0(names(ProjectType),":", paste0(ProjectType, collapse = ",")))
      ProjectType
    })
    # Header
    output$header <- shiny::renderUI({
      list(shiny::h2("Quarterly Performance Report")
           , shiny::h3(paste(input$radio_mean, header))
           , shiny::h4(format.Date(Report()$Start, "%m-%d-%Y"), "-", format.Date(Report()$End, "%m-%d-%Y"))
           )
    })
    # Gather Objects
    
    # Process Data
    data_env <- reactive(qpr_expr[[id]]$expr, quoted = TRUE)
    
    output$plot <- plotly::renderPlotly({
      rlang::eval_bare(qpr_expr[[id]]$plot)
    })
  })
}

