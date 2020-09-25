#' QPR_tabItem UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#'
tab_choices <- list(LoS = c("Emergency Shelters", "Transitional Housing","Safe Haven", "Rapid Rehousing"),
     PH = c("Emergency Shelters", "Transitional Housing", "Safe Haven", "Prevention", "Rapid Rehousing", "Permanent Supportive Housing", "Street Outreach"),
     NCB = c("Emergency Shelters", "Transitional Housing", "Safe Haven", "Prevention", "Rapid Rehousing", "Permanent Supportive Housing", "Street Outreach"),
     HI = c("Emergency Shelters", "Transitional Housing", "Safe Haven", "Prevention", "Rapid Rehousing","Permanent Supportive Housing", "Street Outreach")
)
mod_QPR_tabItem_ui <- function(id, program_choices, region_choices, radio_mean = FALSE){
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
                                    c(
                                      unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4)
                                    ),
                                    selected = zoo::Sys.yearqtr() - 1 / 4),
      if (!missing(program_choices)) {
        shinyWidgets::prettyRadioButtons(
        inputId = ns("program_radio"),
        label = "Program Type",
        thick = TRUE,
        animation = "pulse",
        status = "info",
        choices = program_choices,
        selected = NULL
      )},
      if (!missing(region_choices)) {
        shinyWidgets::pickerInput(
        inputId = ns("region_select"),
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
    
#' QPR_tabItem Server Functions
#'
#' @noRd 
mod_QPR_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   
  })
}
    