#' QPR_tabItem UI Function
#' @title 
#' @description A shiny Module to generate the QPR tabitems.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param program_choices \code{(named list)} of choices of program types for the radio picker. Default does not show the UI item - choices must be provided for the picker to show.
#' @param region_choices \code{(named list)} of choices of regions for the region drop-down selector. Default does not show the UI item - choices must be provided for the picker to show options
#QUESTION Should there be defaults for options?
#' @param radio_mean \code{(logical)} whether to show the Mean/Median based average radio UI
#' @importFrom shiny NS tagList 
#TODO sinew::makeImport

tab_choices <-
  list(
    LoS = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Rapid Rehousing"
    ),
    PH = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    ),
    NCB = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    ),
    HI = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    )
  )

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

#' QPR Server Functions

mod_QPR_server <- function(id, header){
  if (missing(header)) {rlang::abort("Must provide header for mod_QPR_server(",id,")")}
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Process Slider Inputs
    Report <- eventReactive(input$slider, {
      list(
      Start = qstart_date(input$slider),
      End = qend_date(input$slider)
      )
    })
    # Header
    output$header <- shiny::renderUI({
      message(paste0(as.character(Report()$Start), " \ ", as.character(Report()$End)))
      list(shiny::h2("Quarterly Performance Report"),
           shiny::h3(paste(input$radio_mean, header)),
           shiny::h4(Report()$Start, "-", Report()$End))
    })
    # Gather Objects
    
    # Process Data
    # data_env <- reactive({
    #  rlang::eval_bare(expressions[[id]]) 
    # })
    output$plot <- plotly::renderPlotly({
      ReportStart <- Report()$Start
      ReportEnd <- Report()$End
      
      LoSGoals <- goals %>%
        dplyr::select(-Measure) %>%
        dplyr::filter(SummaryMeasure == "Length of Stay" &
                        ProjectType %in% c(input$ProjectType)) %>%
        unique()
      
      LoSDetail <- qpr_leavers %>%
        dplyr::filter(((
          !is.na(MoveInDateAdjust) &
            ProjectType == 13
        ) |
          (
            ProjectType %in% c(1, 2, 8) &
              !is.na(ExitDate)
          )) &
          exited_between(., ReportStart, ReportEnd)) %>%
        dplyr::filter(
          ProjectRegion %in% c(input$Region) &
            ProjectType %in% c(input$ProjectType)
        ) # this filter needs
      # to be here so the selection text matches the mutated data
      TotalLeavers <- LoSDetail %>%
        dplyr::group_by(FriendlyProjectName) %>%
        dplyr::summarise(Leavers = dplyr::n())
      
      title <-
        paste0(
          "Length of Stay (",
          input$radio_mean,
          ")\n",
          names(choices_project_type)[choices_project_type %in% input$ProjectType],
          "\n",
          ReportStart,
          " to ",
          ReportEnd
        )
      
      LoSSummary <- LoSDetail %>%
        dplyr::group_by(FriendlyProjectName,
                        ProjectRegion,
                        ProjectCounty,
                        ProjectType) %>%
        dplyr::summarise(
          Days = dplyr::case_when(
            input$radio_mean == "Average Days" ~
              as.numeric(mean(DaysinProject)),
            input$radio_mean == "Median Days" ~
              as.numeric(stats::median(DaysinProject))
          )
        ) %>%
        dplyr::left_join(LoSGoals, by = "ProjectType") %>%
        dplyr::left_join(TotalLeavers, by = ("FriendlyProjectName")) %>%
        dplyr::mutate(
          hover = paste0(
            FriendlyProjectName,
            "\nTotal Leavers: ",
            Leavers,
            "\nDays: ",
            Days,
            sep = "\n"
          )
        )
      message(paste0("LosSummary:", nrow(LoSSummary)))
      qpr_plotly(
        LoSDetail,
        y = ~ Days,
        title = title,
        xaxis = list(
          title = "Days",
          rangemode = "tozero",
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Days",
          rangemode = "tozero",
          showgrid = TRUE
        )
      )
    })
  })
}

