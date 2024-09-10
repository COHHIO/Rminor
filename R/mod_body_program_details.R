#' Program Performance UI Function
#'
#' @description A shiny Module for the Program Performance tab.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
mod_body_program_details_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::bs4TabItem(
    tabName = "program_details",
    bs4Dash::tabsetPanel(
      id = ns("program_details_tabs"),
      type = "tabs",
      tabPanel(
        "Length of Stay",
        mod_body_performance_summary_ui(ns("length_of_stay"))
      ),
      tabPanel(
        "Exits to Permanent Housing",
        mod_body_performance_summary_ui(ns("exits_permanent_housing"))
      ),
      tabPanel(
        "Exits to Temporary or Permanent Housing",
        mod_body_performance_summary_ui(ns("exits_temp_permanent_housing"))
      ),
      tabPanel(
        "Non-Cash Benefits at Exit",
        mod_body_performance_summary_ui(ns("non_cash_benefits"))
      ),
      tabPanel(
        "Health Insurance at Exit",
        mod_body_performance_summary_ui(ns("health_insurance"))
      ),
      tabPanel(
        "Income Growth",
        mod_body_performance_summary_ui(ns("income_growth"))
      ),
      tabPanel(
        "Rapid Placement for RRH",
        mod_body_performance_summary_ui(ns("rapid_placement_rrh"))
      )
    )
  )
}

#' Program Performance Server Function
#'
#' @noRd 
mod_body_program_details_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call the server module for each measure
    mod_body_performance_summary_server("length_of_stay")
    mod_body_performance_summary_server("exits_permanent_housing")
    mod_body_performance_summary_server("exits_temp_permanent_housing")
    mod_body_performance_summary_server("non_cash_benefits")
    mod_body_performance_summary_server("health_insurance")
    mod_body_performance_summary_server("income_growth")
    mod_body_performance_summary_server("rapid_placement_rrh")
  })
}
