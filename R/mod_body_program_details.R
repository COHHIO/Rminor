#' Program Performance UI Function
#'
#' @description A shiny Module for the Program Performance tab.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
mod_body_program_details_ui <- function(id) {
  ns <- NS(id)
  
  # Directly use tabsetPanel without wrapping it inside bs4TabItem
  bs4Dash::tabBox(width = 12,
                  id = ns("program_details_tabs"),
                  type = "tabs",
                  
                  # Length of Stay
                  tabPanel(
                    "Length of Stay",
                    mod_qpr_ui(ns("length_of_stay"), "length_of_stay", "Length of Stay", 
                                    choices = c("Emergency Shelter – Entry Exit", "PH – Rapid Re-Housing", "Transitional Housing"))
                  ),
                  
                  # Exits to Permanent Housing
                  tabPanel(
                    "Exits to Permanent Housing",
                    mod_qpr_ui(ns("exits_permanent_housing"), "exits_permanent_housing", "Exits to Permanent Housing", 
                                    choices = c("Emergency Shelter – Entry Exit", "PH – Rapid Re-Housing", "Transitional Housing", "Street Outreach"))
                  ),
                  
                  # Exits to Temporary or Permanent Housing
                  tabPanel(
                    "Exits to Temporary or Permanent Housing",
                    mod_qpr_ui(ns("exits_temp_permanent_housing"), "exits_temp_permanent_housing", "Exits to Temporary or Permanent Housing", 
                                    choices = "Street Outreach")
                  ),
                  
                  # Non-Cash Benefits at Exit
                  tabPanel(
                    "Non-Cash Benefits at Exit",
                    mod_qpr_ui(ns("non_cash_benefits"), "non_cash_benefits", "Non-Cash Benefits at Exit", 
                                    choices = c("Emergency Shelter – Entry Exit", "PH – Rapid Re-Housing", "Transitional Housing", "PH – Permanent Supportive Housing"))
                  ),
                  
                  # Health Insurance at Exit
                  tabPanel(
                    "Health Insurance at Exit",
                    mod_qpr_ui(ns("health_insurance"), "health_insurance", "Health Insurance at Exit", 
                                    choices = c("Emergency Shelter – Entry Exit", "PH – Rapid Re-Housing", "Transitional Housing", "PH – Permanent Supportive Housing"))
                  ),
                  
                  # Income Growth
                  tabPanel(
                    "Income Growth",
                    mod_qpr_ui(ns("income_growth"), "income_growth", "Income Growth", 
                                    choices = c("Emergency Shelter – Entry Exit", "PH – Rapid Re-Housing", "Transitional Housing", "PH – Permanent Supportive Housing"))
                  ),
                  
                  # Rapid Placement for RRH
                  tabPanel(
                    "Rapid Placement for RRH",
                    mod_qpr_ui(ns("rapid_placement_rrh"), "rapid_placement_rrh", "Rapid Placement for RRH", 
                                    choices = "PH – Rapid Re-Housing")
                  )
  )
}




#' Program Performance Server Function
#'
#' @noRd 
mod_body_program_details_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Length of Stay
    mod_qpr_server(
      id = "length_of_stay", 
      measure_name = "length_of_stay",
      calculate_expr = qpr_expr$length_of_stay$expr, 
      infobox_expr = qpr_expr$length_of_stay$infobox, 
      details_expr = qpr_expr$length_of_stay$details
    )
    
    # Exits to Permanent Housing
    mod_qpr_server(
      id = "exits_permanent_housing", 
      measure_name = "exits_permanent_housing",
      calculate_expr = qpr_expr$exits_permanent_housing$expr, 
      infobox_expr = qpr_expr$exits_permanent_housing$infobox, 
      details_expr = qpr_expr$exits_permanent_housing$details
    )
    
    # Exits to Temporary or Permanent Housing
    mod_qpr_server(
      id = "exits_temp_permanent_housing", 
      measure_name = "exits_temp_permanent_housing",
      calculate_expr = qpr_expr$exits_temp_permanent_housing$expr, 
      infobox_expr = qpr_expr$exits_temp_permanent_housing$infobox, 
      details_expr = qpr_expr$exits_temp_permanent_housing$details
    )
    
    # Non-Cash Benefits at Exit
    mod_qpr_server(
      id = "non_cash_benefits", 
      measure_name = "non_cash_benefits",
      calculate_expr = qpr_expr$non_cash_benefits$expr, 
      infobox_expr = qpr_expr$non_cash_benefits$infobox, 
      details_expr = qpr_expr$non_cash_benefits$details
    )
    
    # Health Insurance at Exit
    mod_qpr_server(
      id = "health_insurance", 
      measure_name = "health_insurance",
      calculate_expr = qpr_expr$health_insurance$expr, 
      infobox_expr = qpr_expr$health_insurance$infobox, 
      details_expr = qpr_expr$health_insurance$details
    )
    
    # Income Growth
    mod_qpr_server(
      id = "income_growth", 
      measure_name = "income_growth",
      calculate_expr = qpr_expr$income_growth$expr, 
      infobox_expr = qpr_expr$income_growth$infobox, 
      details_expr = qpr_expr$income_growth$details
    )
    
    # Rapid Placement for RRH
    mod_qpr_server(
      id = "rapid_placement_rrh", 
      measure_name = "rapid_placement_rrh",
      calculate_expr = qpr_expr$rapid_placement_rrh$expr, 
      infobox_expr = qpr_expr$rapid_placement_rrh$infobox, 
      details_expr = qpr_expr$rapid_placement_rrh$details
    )
  })
}


