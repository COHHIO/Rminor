# Define the UI module
mod_body_performance_summary_ui <- function(id) {
  ns <- NS(id)
  
  # Calculate the start and end dates for the previous calendar year
  start_date <- lubridate::floor_date(Sys.Date(), "year") - lubridate::years(1)
  end_date <- lubridate::floor_date(Sys.Date(), "year") - lubridate::days(1)
  
  bs4Dash::tabBox(width = 12,
                  tabPanel("Length of Stay",
                           h3("Length of Stay Summary"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_1"),
                               label = "Select your Project Type",
                               choices = c("Emergency Shelter – Entry Exit",
                                           "Transitional Housing"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_1"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_1")),
                             DT::dataTableOutput(ns("ps_table_1"))
                           )
                  ),
                  tabPanel("Exits to Permanent Housing",
                           h3("Exits Summary"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_2"),
                               label = "Select your Project Type",
                               choices = c("Emergency Shelter – Entry Exit",
                                           "PH – Rapid Re-Housing",
                                           "Transitional Housing",
                                           "Street Outreach"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_2"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_2")),
                             ui_row(
                               DT::dataTableOutput(ns("ps_table_2")),
                               title = "Measure 2: Exits"
                             )
                           )
                  ),
                  tabPanel("Exits to Temporary or Permanent Housing",
                           h3("Exits Summary"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_3"),
                               label = "Select your Project Type",
                               choices = "Street Outreach",
                               selected = "Street Outreach",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_3"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_3")),
                             ui_row(
                               DT::dataTableOutput(ns("ps_table_3")),
                               title = "Measure 3: Exits to Temporary or Permanent Houusing"
                             )
                           )
                  )
  )
}

# Define the server module
mod_body_performance_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Debugging: Check if input$project_type is correctly bound
    observe({
      cat("Observer - project_type:", input$project_type, "\n")
    })
    
    #### Measure 1: Length of Stay
    length_of_stay <- eventReactive({
      list(input$project_type, input$date_range)
    }, {
      req(input$project_type_1, input$date_range_1)
      # Debugging: Print input to ensure it is available
      cat("Selected project type in eventReactive:", input$project_type_1, "\n")
      cat("Selected date range in eventReactive:", input$date_range_1, "\n")
      
      start_date <- as.Date(input$date_range_1[1])
      end_date <- as.Date(input$date_range_1[2])
      
      data <- qpr_leavers() |> 
        HMIS::exited_between(start_date, end_date) |> 
        dplyr::filter(((!is.na(MoveInDateAdjust) & ProjectType == 13) |
                         (!is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)))) |> 
        dplyr::group_by(ProjectName, ProjectType) |>
        dplyr::summarise(Average = round(mean(DaysinProject), 1),
                         Median = median(DaysinProject), clients = dplyr::n(), .groups = "drop_last") |>
        dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |> 
        dplyr::filter(ProjectType == input$project_type_1)
      
      # Debugging: Print the resulting data
      cat("Filtered data:\n")
      print(data)
      
      data
    })
    
    output$ps_plot_1 <- plotly::renderPlotly({
      measure_1 <- length_of_stay()
      # Debugging: Print data to ensure it's correct before plotting
      cat("Plot data:\n")
      print(measure_1)
      
      qpr_plotly(measure_1, title = "Length of Stay", xaxis_title = "Number of Clients",
                 yaxis_title = "Average Length of Stay")
    })
    
    output$ps_table_1 <- DT::renderDT(server = FALSE, {
      measure_1 <- length_of_stay()
      # Debugging: Print data to ensure it's correct before rendering table
      cat("Table data:\n")
      print(measure_1)
      
      measure_1 |> datatable_default()
    })
    
    #### Measure 2: Exits to Permanent Housing
    exits <- eventReactive({
      list(input$project_type_2, input$date_range_2)
    },{
      start_date <- as.Date(input$date_range_2[1])
      end_date <- as.Date(input$date_range_2[2])
      
      qpr_leavers <- qpr_leavers()
      .exited <- qpr_leavers |> 
        HMIS::exited_between(start_date, end_date, lgl = TRUE)
      .served <- qpr_leavers |> 
        HMIS::served_between(start_date, end_date, lgl = TRUE)
      
      .psh_hp <- qpr_leavers$ProjectType %in% c(3, 9, 12)
      .es_th_sh_out_rrh <- qpr_leavers$ProjectType %in% c(0, 1, 2, 4, 8, 13)
      
      SuccessfullyPlaced <- dplyr::filter(qpr_leavers,
                                          ((ProjectType %in% c(3, 9, 13) &
                                              !is.na(MoveInDateAdjust)) |
                                             ProjectType %in% c(0, 1, 2, 4, 8, 12)
                                          ) &
                                            # excluding non-mover-inners
                                            (((DestinationGroup == "Permanent" |
                                                 #exited to ph or still in PSH/HP
                                                 is.na(ExitDate)) &
                                                .psh_hp & # PSH & HP
                                                .served
                                            ) |
                                              (
                                                DestinationGroup == "Permanent" & # exited to ph
                                                  .es_th_sh_out_rrh &
                                                  .exited
                                              )
                                            ))
      
      # calculating the total households to compare successful placements to
      TotalHHsSuccessfulPlacement <- 
        dplyr::filter(qpr_leavers,
                      (.served & .psh_hp) # PSH & HP
                      |
                        (.exited & .es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
        )
      
      TotalCount <- TotalHHsSuccessfulPlacement |> 
        dplyr::count(ProjectName, ProjectType) |> 
        dplyr::rename(clients = n)
      
      SuccessCount <- SuccessfullyPlaced |>
        dplyr::count(ProjectName, ProjectType) |>
        dplyr::rename(success_clients = n)
      
      data <- TotalCount |> 
        dplyr::left_join(SuccessCount, by = c("ProjectName","ProjectType")) |>
        {\(.) {replace(.,is.na(.),0)}}() |>
        dplyr::mutate(Average = success_clients/clients) |> 
        dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |>
        dplyr::filter(ProjectType == input$project_type_2)
      
      # Debugging: Print the resulting data
      cat("Filtered data:\n")
      print(data)
      
      data
    })
    
    output$ps_plot_2 <-
      plotly::renderPlotly({
        measure_2 <- exits()
        qpr_plotly(measure_2, title = "Exits to Permanent Housing", 
                   xaxis_title = "Number of Clients", yaxis_title = "Percent to Permanent Housing",
                   y_label = "Percent Exiting to Permanent Housing")
      })
    
    
    output$ps_table_2 <- DT::renderDT(server = FALSE, {
      measure_2 <- exits()
      measure_2 |> 
        datatable_default()
    })
    
    #### Exits to Temp or Permanent Housing
    exits_temp <- eventReactive(input$project_type_3, {
      qpr_leavers <- qpr_leavers()
      
      .so <- qpr_leavers$ProjectType %in% c(4)
      .exited <- qpr_leavers |> 
        HMIS::exited_between(as.Date("2023-01-01"), as.Date("2023-12-31"), lgl = TRUE)
      
      SuccessfullyPlacedTemp <- dplyr::filter(qpr_leavers,
                                              ((DestinationGroup == "Permanent" | DestinationGroup == "Temporary") &
                                                 .so & .exited))
      
      # calculating the total households to compare successful placements to
      TotalHHsSuccessfulPlacementTemp <- 
        dplyr::filter(qpr_leavers,
                      (.exited & .so)
        )
      
      TotalCountTemp <- TotalHHsSuccessfulPlacementTemp |> 
        dplyr::count(ProjectName, ProjectType) |> 
        dplyr::rename(clients = n)
      
      SuccessCountTemp <- SuccessfullyPlacedTemp |>
        dplyr::count(ProjectName, ProjectType) |>
        dplyr::rename(success_clients = n)
      
      data <- TotalCountTemp |> 
        dplyr::left_join(SuccessCountTemp, by = c("ProjectName","ProjectType")) |>
        {\(.) {replace(.,is.na(.),0)}}() |>
        dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |>
        dplyr::filter(ProjectType == input$project_type_3) |> 
        dplyr::mutate(Average = success_clients/clients)
      
      # Debugging: Print the resulting data
      cat("Filtered data:\n")
      print(data)
      
      data
    })
    
    
    output$ps_plot_3 <-
      plotly::renderPlotly({
        measure_3 <- exits_temp()
        qpr_plotly(measure_3, title = "Exits to Temporary or Permanent Housing", 
                   xaxis_title = "Number of Clients", yaxis_title = "Percent to Housing",
                   y_label = "Percent to Temp or Permanent Housing")
      })
    
    
    output$ps_table_3 <- DT::renderDT(server = FALSE, {
      measure_3 <- exits_temp()
      measure_3 |> 
        datatable_default()
    })
  })
}

