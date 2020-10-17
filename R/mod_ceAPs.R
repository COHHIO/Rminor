#' ceAPs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_ceAPs_ui <- function(id) {
  ns <- NS(id)
  shinydashboard::tabItem(
    tabName = ns("Tab"),
    shiny::fluidRow(shinydashboard::box(
      shinyWidgets::prettyRadioButtons(
        inputId = ns("radio_search"),
        label = "Search by:",
        choices = c(
          "County" = "ProjectCountyServed",
          "Service Area" = "ProjectAreaServed",
          "Organization" = "ProjectAKA"
        ),
        selected = "ProjectCountyServed"
      )
    )),
    shiny::fluidRow(shinydashboard::box(
      shinyWidgets::pickerInput(
        inputId = ns("AP"),
        label = "Select County/-ies",
        options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                              actionsBox = TRUE),
        choices = bos_counties,
        multiple = TRUE
      )
    )),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Coordinated Entry Access Points",
        width = 12,
        DT::dataTableOutput(ns("AP_list"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Ohio Balance of State CoC Homeless Planning Regions",
        htmltools::HTML(
          "The solid-colored counties are all part of the Ohio
                       Balance of State CoC. The Ohio Development Services
                       Agency (ODSA) further divided the counties in the Balance
                       of State into 17 Homeless Planning Regions to make
                       implementation of state-funded programs in the Balance of
                       State more localized."
        ),
        img(src =
              "www/Homeless-Region-Map-for-COHHIO-2017.png",
            style = 'max-width: 100%; height: auto'),
        width = 12
      )
    )
  )
}

#' ceAPs Server Functions
#'
#' @noRd
mod_ceAPs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$radio_search, {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "AP",
        label = list(
          "ProjectCountyServed" = "Select County/-ies",
          "ProjectAreaServed" = "Select Service Area",
          "ProjectAKA" = "Select Organization"
        )[[input$radio_search]], 
        choices = list(
          "ProjectCountyServed" = bos_counties,
          "ProjectAreaServed" = choices_service_areas,
          "ProjectAKA" = APs %>%
            arrange(ProjectAKA) %>%
            pull(ProjectAKA) %>%
            unique()
        )[[input$radio_search]]
      )
    })
    
    output$AP_list <- renderDataTable({
      SearchColumn <- rlang::sym(input$radio_search)
      
      AP_list <- APs %>%
        filter(!!SearchColumn %in% c(input$AP)) %>%
        select(ProjectID) %>% unique()
      
      message(paste0("AP_list", nrow(AP_list)))
      
      AP_final <- APs %>%
        right_join(AP_list, by = "ProjectID") %>%
        mutate(Address = if_else(
          !is.na(CoCCode),
          paste(Addresses, City, sep = '<br>'),
          "Please call- address not available."
        )) %>%
        group_by(OrgLink,
                 TargetPop,
                 Address,
                 ProjectHours,
                 ProjectTelNo) %>%
        summarise(
          Regions = paste(unique(ProjectAreaServed), collapse = ",<br>"),
          Counties = paste(unique(ProjectCountyServed), collapse = ", ")
        ) %>%
        ungroup() %>%
        unique() %>%
        select(
          "Organization" = OrgLink,
          "Target Population" = TargetPop,
          "County/-ies" = Counties,
          Address,
          "Hours" = ProjectHours,
          "Phone" = ProjectTelNo
        )
      
      datatable(
        AP_final,
        rownames = FALSE,
        options = list(dom = 'ltpi'),
        escape = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_ceAPs_ui("ceAPs_ui_1")

## To be copied in the server
# mod_ceAPs_server("ceAPs_ui_1")
