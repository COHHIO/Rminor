#' ceAPs UI Function
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' 
#' @importFrom shiny NS tagList

mod_body_ceaps_ui <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    ui_header_row(),
    ui_row(
        title = "Coordinated Entry Access Points",
        htmltools::HTML("Coordinated Entry Access Points (CE APs) serve as the entry point to the Ohio BoSCoC homeless response system. If a CE AP provider notices that the posted information is not correct, please contact the CoC team at <a href='mailto:ohioboscoc@cohhio.org' target='_blank'>ohioboscoc@cohhio.org</a> to notify them of needed corrections."),
        DT::dataTableOutput(ns("AP_list")),
        width = 12
    ),
    ui_row(
      title = "Ohio Balance of State CoC Homeless Planning Regions",
      htmltools::HTML("To facilitate funding distribution and support system coordination efforts, the Ohio BoSCoC is divided into 17 Homeless Planning Regions. Throughout R minor, you will notice references to Homeless Planning Regions. Please consult this map if you are unsure what Region your county is in."
                      )
    ),
    ui_row(
      htmltools::img(src = "www/BoS-Region-Map-2023.png",
                     width = '50%')
    )
  )
}

#' ceAPs Server Functions
#'
#' 
#' @description Server module for ceAPs UI
#' 
#' @noRd
mod_body_ceaps_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$header <- shiny::renderUI({
      server_header(
        "Ohio Balance of State CoC Coordinated Entry Access Points",
        paste0("Updated: ", rm_dates()$meta_HUDCSV$Export_End)
      )
    })

    output$AP_list <- DT::renderDT(server = FALSE, {
      req(APs())
      APs() |>
        dplyr::select(ProjectID,
                      ProjectName,
                      TargetPop,
                      ProjectCountyServed,
                      Phone,
                      CoCCode,
                      Addresses,
                      City) |> 
        unique() |> 
        datatable_default(escape = FALSE)
    })
  })
}

## To be copied in the UI
# mod_ceAPs_ui("ceAPs_ui_1")

## To be copied in the server
# mod_ceAPs_server("ceAPs_ui_1")
