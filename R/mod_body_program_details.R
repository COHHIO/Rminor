#' Program Performance UI Function
#'
#' @description A shiny Module for the Program Performance tab.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
mod_body_program_details_ui <- function(id) {
  ns <- NS(id)  # Ensure this is correctly used
  
  bs4Dash::tabBox(width = 12,
                  id = ns("program_details_tabs"),
                  
                  tabPanel(
                    "Length of Stay",
                    mod_body_qpr_ui(
                      id = ns("length_of_stay")
                    )
                  ),
                  tabPanel(
                    "Exits to Permanent Housing",
                    mod_body_qpr_ui(
                      id = ns("permanent_housing")
                    )
                  ),
                  tabPanel(
                    "Exits to Temporary or Permanent Housing",
                    mod_body_qpr_ui(
                      id = ns("temp_permanent_housing")
                    )
                  ),
                  tabPanel(
                    "Non-cash Benefits at Exit",
                    mod_body_qpr_ui(
                      id = ns("noncash_benefits")
                    )
                  ),
                  tabPanel(
                    "Health Insurance",
                    mod_body_qpr_ui(
                      id = ns("health_insurance")
                    )
                  ),
                  tabPanel(
                    "Income Growth",
                    mod_body_qpr_ui(
                      id = ns("income_growth")
                    )
                  ),
                  tabPanel(
                    "Rapid Placement for RRH",
                    mod_body_qpr_ui(
                      id = ns("rrh_placement")
                    )
                  )
  )
}





#' Program Performance Server Function
#'
#' @noRd 
mod_body_program_details_server <- function(id, is_youth = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Maybe just make a separate qpr_expr for youth entirely
    qpr_expr <- if (is_youth) qpr_expr[[paste0(id, "_youth")]]$expr else qpr_expr
    
    # Length of Stay
    mod_body_qpr_server(
      id = "length_of_stay", 
      header = "Length of Stay",
      calculate_expr = qpr_expr$length_of_stay$expr, 
      infobox_expr = qpr_expr$length_of_stay$infobox, 
      details_expr = qpr_expr$length_of_stay$details
    )
    
    # Exits to Permanent Housing
    mod_body_qpr_server(
      id = "permanent_housing", 
      header = "Exits to Permanent Housing",
      calculate_expr = qpr_expr$permanent_housing$expr, 
      infobox_expr = qpr_expr$permanent_housing$infobox, 
      details_expr = qpr_expr$permanent_housing$details
    )
    
    # Exits to Temp or Permanent Housing
    mod_body_qpr_server(
      id = "temp_permanent_housing", 
      header = "Exits to Temporary or Permanent Housing",
      calculate_expr = qpr_expr$temp_permanent_housing$expr, 
      infobox_expr = qpr_expr$temp_permanent_housing$infobox, 
      details_expr = qpr_expr$temp_permanent_housing$details
    )
    
    # Non-cash benefits at Exit
    mod_body_qpr_server(
      id = "noncash_benefits", 
      header = "Non-cash Benefits at Exit",
      calculate_expr = qpr_expr$noncash_benefits$expr, 
      infobox_expr = qpr_expr$noncash_benefits$infobox, 
      details_expr = qpr_expr$noncash_benefits$details
    )
    
    # Health Insurance at Exit
    mod_body_qpr_server(
      id = "health_insurance", 
      header = "Health Insurance",
      calculate_expr = qpr_expr$health_insurance$expr, 
      infobox_expr = qpr_expr$health_insurance$infobox, 
      details_expr = qpr_expr$health_insurance$details
    )
    
    # Income Growth
    mod_body_qpr_server(
      id = "income_growth", 
      header = "Income Growth",
      calculate_expr = qpr_expr$income_growth$expr, 
      infobox_expr = qpr_expr$income_growth$infobox, 
      details_expr = qpr_expr$income_growth$details
    )
    
    # Rapid Placement
    mod_body_qpr_server(
      id = "rrh_placement", 
      header = "Rapid Placement for RRH",
      calculate_expr = qpr_expr$rrh_placement$expr, 
      infobox_expr = qpr_expr$rrh_placement$infobox, 
      details_expr = qpr_expr$rrh_placement$details
    )

  })
}



