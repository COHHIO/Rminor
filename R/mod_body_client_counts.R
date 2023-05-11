#' body_client_counts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_client_counts_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    ui_header_row(),
    ui_row(
      tags$p(tags$em("Note:"), " if a program is absent from this list, it did not serve any clients since ", rm_dates()$calc$data_goes_back_to),
      ui_picker_program(),
      ui_date_range(start = prev_quarter()),
      headerBorder = FALSE
    ),
    ui_row(
      title = "Summary",
      DT::dataTableOutput(ns("summary")),
    ),
    ui_row(
      title = "Client Details",
      DT::dataTableOutput(ns("dt_output")),
      width = 12
    )
  )
}

#' body_client_counts Server Functions
#'
#' @noRd 
mod_body_client_counts_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$header <- shiny::renderUI({
      server_header(
        title = "Bed & Unit Utilization"
      )
    })
    program <- reactive(input$program) |> debounce(millis = 1500)
    validation_range <- eventReactive(c(input$date_range, input$program), {
      req(input$program, program(), input$date_range)
      dq_filter_between(validation(), date_range = input$date_range, program = program())
    })
    output$dt_output <- DT::renderDT(server = FALSE, {
      req(input$date_range, input$program, validation_range())
      validation_range()  |> 
        dplyr::mutate(
          RelationshipToHoH = HMIS::hud_translations$`3.15.1 RelationshipToHoH`(RelationshipToHoH),
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ paste0("Currently Awaiting Housing (", 
                                       lubridate::today() - EntryDate,
                                       " days)"),
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ paste0("Currently Moved In (",
                                       lubridate::today() - MoveInDateAdjust,
                                       " days)"),
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ paste0("Currently in program (",
                                       lubridate::today() - EntryDate, 
                                       " days)"),
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited program",
          ),
          sort = lubridate::today() - EntryDate
        ) |> 
        dplyr::arrange(dplyr::desc(sort), HouseholdID) |>
        dplyr::select(
          "County" = CountyServed,
          "Relationship to Head of Household" = RelationshipToHoH,
          "Entry Date" = EntryDate,
          "Move In Date (RRH/PSH Only)" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          Status
        ) |> 
        datatable_default(escape = FALSE)
    })
    
    output$summary <- DT::renderDT(server = FALSE, {
      
      v <- validation_range() |>
        dplyr::mutate(
          # Entered = if_else(between(EntryDate, input$date_range[1], input$date_range[2]),
          #                   "Entered in date range", "Entered outside date range"),
          # Leaver = if_else(!is.na(ExitDate), "Leaver", "Stayer"),
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Awaiting Housing",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Moved In",
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ "Currently in program",
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited program",
          )
        )
      dplyr::full_join(
        dplyr::distinct(v, 
                      HouseholdID,
                      ProjectType,
                      EntryDate,
                      MoveInDateAdjust,
                      ExitDate,
                      Status) |>
          unique() |>
          dplyr::group_by(Status) |>
          dplyr::summarise(Households = dplyr::n()),
        dplyr::distinct(v,
                      ProjectType,
                      EntryDate,
                      MoveInDateAdjust,
                      ExitDate,
                      Status) |>
          unique() |>
          dplyr::group_by(Status) |>
          dplyr::summarise(Clients = dplyr::n())
      ) |>  
        datatable_default(escape = FALSE)
    })
  })
}

## To be copied in the UI
# mod_body_client_counts_ui("body_client_counts_1")

## To be copied in the server
# mod_body_client_counts_server("body_client_counts_1")