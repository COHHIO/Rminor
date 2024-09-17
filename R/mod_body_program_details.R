#' Program Performance UI Function
#'
#' @description A shiny Module for the Program Performance tab.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
mod_body_program_details_ui <- function(id, is_youth = FALSE) {
  ns <- NS(id)  # Ensure this is correctly used
  
  # Base panels (that are common for both youth and non-youth)
  panels <- list(
    tabPanel(
      "Length of Stay",
      mod_body_qpr_ui(
        id = ns("length_of_stay"),
        is_youth = is_youth
      )
    ),
    tabPanel(
      "Exits to Permanent Housing",
      mod_body_qpr_ui(
        id = ns("permanent_housing"),
        is_youth = is_youth
      )
    ),
    tabPanel(
      "Exits to Temporary or Permanent Housing",
      mod_body_qpr_ui(
        id = ns("temp_permanent_housing"),
        is_youth = is_youth
      )
    ),
    tabPanel(
      "Non-cash Benefits at Exit",
      mod_body_qpr_ui(
        id = ns("noncash_benefits"),
        is_youth = is_youth
      )
    ),
    tabPanel(
      "Health Insurance",
      mod_body_qpr_ui(
        id = ns("health_insurance"),
        is_youth = is_youth
      )
    ),
    tabPanel(
      "Income Growth",
      mod_body_qpr_ui(
        id = ns("income_growth"),
        is_youth = is_youth
      )
    ),
    tabPanel(
      "Rapid Placement for RRH",
      mod_body_qpr_ui(
        id = ns("rrh_placement"),
        is_youth = is_youth
      )
    )
  )
  
  # Add reentries tab if is_youth is FALSE (non-youth)
  if (!is_youth) {
    panels <- c(panels, list(
      tabPanel(
        "Entries into the Homeless System",
        mod_body_qpr_ui(
          id = ns("reentries"),
          is_youth = is_youth
        )
      )
    ))
  }
  
  # Use do.call to build the tabBox dynamically with the correct panels
  do.call(bs4Dash::tabBox, c(list(id = ns("program_details_tabs"), width = 12), panels))
}



# Youth Program Performance UI Function
mod_body_program_details_ui_youth <- function(id) {
  mod_body_program_details_ui(id, is_youth = TRUE)
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
      details_expr = qpr_expr$length_of_stay$details,
      is_youth = is_youth
    )
    
    # Exits to Permanent Housing
    mod_body_qpr_server(
      id = "permanent_housing", 
      header = "Exits to Permanent Housing",
      calculate_expr = qpr_expr$permanent_housing$expr, 
      infobox_expr = qpr_expr$permanent_housing$infobox, 
      details_expr = qpr_expr$permanent_housing$details,
      is_youth = is_youth
    )
    
    # Exits to Temp or Permanent Housing
    mod_body_qpr_server(
      id = "temp_permanent_housing", 
      header = "Exits to Temporary or Permanent Housing",
      calculate_expr = qpr_expr$temp_permanent_housing$expr, 
      infobox_expr = qpr_expr$temp_permanent_housing$infobox, 
      details_expr = qpr_expr$temp_permanent_housing$details,
      is_youth = is_youth
    )
    
    # Non-cash benefits at Exit
    mod_body_qpr_server(
      id = "noncash_benefits", 
      header = "Non-cash Benefits at Exit",
      calculate_expr = qpr_expr$noncash_benefits$expr, 
      infobox_expr = qpr_expr$noncash_benefits$infobox, 
      details_expr = qpr_expr$noncash_benefits$details,
      is_youth = is_youth
    )
    
    # Health Insurance at Exit
    mod_body_qpr_server(
      id = "health_insurance", 
      header = "Health Insurance",
      calculate_expr = qpr_expr$health_insurance$expr, 
      infobox_expr = qpr_expr$health_insurance$infobox, 
      details_expr = qpr_expr$health_insurance$details,
      is_youth = is_youth
    )
    
    # Income Growth
    mod_body_qpr_server(
      id = "income_growth", 
      header = "Income Growth",
      calculate_expr = qpr_expr$income_growth$expr, 
      infobox_expr = qpr_expr$income_growth$infobox, 
      details_expr = qpr_expr$income_growth$details,
      is_youth = is_youth
    )
    
    # Rapid Placement
    mod_body_qpr_server(
      id = "rrh_placement", 
      header = "Rapid Placement for RRH",
      calculate_expr = qpr_expr$rrh_placement$expr, 
      infobox_expr = qpr_expr$rrh_placement$infobox, 
      details_expr = qpr_expr$rrh_placement$details,
      is_youth = is_youth
    )
    
    # Re-entries
    mod_body_qpr_server(
      id = "reentries", 
      header = "Entries into the Homeless System",
      calculate_expr = qpr_expr$reentries$expr, 
      infobox_expr = qpr_expr$reentries$infobox, 
      details_expr = qpr_expr$reentries$details,
      is_youth = is_youth
    )

  })
}



# Youth Program Performance Server Function
mod_body_program_details_server_youth <- function(id) {
  mod_body_program_details_server(id, is_youth = TRUE)
}

