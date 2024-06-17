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
                  ),
                  tabPanel("Non-Cash Benefits at Exit",
                           h3("Benefits Summary"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_4"),
                               label = "Select your Project Type",
                               choices = c("Emergency Shelter – Entry Exit",
                                           "PH – Rapid Re-Housing",
                                           "Transitional Housing",
                                           "PH – Permanent Supportive Housing"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_4"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_4")),
                             ui_row(
                               DT::dataTableOutput(ns("ps_table_4")),
                               title = "Measure 4: Non-Cash Benefits at Exit"
                             )
                           )
                  ),
                  tabPanel("Health Insurance at Exit",
                           h3("Benefits Summary"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_5"),
                               label = "Select your Project Type",
                               choices = c("Emergency Shelter – Entry Exit",
                                           "PH – Rapid Re-Housing",
                                           "Transitional Housing",
                                           "PH – Permanent Supportive Housing"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_5"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_5")),
                             ui_row(
                               DT::dataTableOutput(ns("ps_table_5")),
                               title = "Measure 5: Health Insurance at Exit"
                             )
                           )
                  ),
                  tabPanel("Income Growth",
                           h3("Households Increasing Their Income"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_6"),
                               label = "Select your Project Type",
                               choices = c("Emergency Shelter – Entry Exit",
                                           "PH – Rapid Re-Housing",
                                           "Transitional Housing",
                                           "PH – Permanent Supportive Housing"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_6"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_6")),
                             ui_row(
                               DT::dataTableOutput(ns("ps_table_6")),
                               title = "Measure 6: Households Increasing Their Income"
                             )
                           )
                  ),
                  tabPanel("Rapid Replacement for RRH",
                           h3("Rapid Replacement Summary"),
                           tagList(
                             selectInput(
                               inputId = ns("project_type_6"),
                               label = "Select your Project Type",
                               choices = "PH – Rapid Re-Housing",
                               selected = "PH – Rapid Re-Housing",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_7"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_7")),
                             ui_row(
                               DT::dataTableOutput(ns("ps_table_7")),
                               title = "Measure 7: Rapid Replacement for RRH"
                             )
                           )
                  )
  )
}

# Define the server module
mod_body_performance_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #### Measure 1: Length of Stay
    length_of_stay <- eventReactive({
      list(input$project_type_1, input$date_range_1)
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
      
      data
    })
    
    output$ps_plot_1 <- plotly::renderPlotly({
      measure_1 <- length_of_stay()
      
      # Define goals for different project types
      goals <- list(
        "Emergency Shelter – Entry Exit" = 40,
        "Transitional Housing" = 240
        # Add other project types and their respective goals here
      )
      
      qpr_plotly(measure_1, title = "Length of Stay", xaxis_title = "Number of Clients",
                 yaxis_title = "Average Length of Stay", project_type = input$project_type_1,
                 goals = goals)
    })
    
    output$ps_table_1 <- DT::renderDT(server = FALSE, {
      measure_1 <- length_of_stay()
      
      measure_1 |>
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Number of Clients" = clients) |> 
        datatable_default()
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
      
      data
    })
    
    output$ps_plot_2 <-
      plotly::renderPlotly({
        measure_2 <- exits()
        
        # Define goals for different project types
        goals <- list(
            "Emergency Shelter – Entry Exit" = 0.40,
            "PH – Rapid Re-Housing" = 0.83,
            "Transitional Housing" = 0.83,
            "Street Outreach" = 0.30
        )
        
        qpr_plotly(measure_2, title = "Exits to Permanent Housing", 
                   xaxis_title = "Number of Clients", yaxis_title = "Percent to Permanent Housing",
                   project_type = input$project_type_2,
                   goals = goals, rect_above_line = FALSE)
      })
    
    
    output$ps_table_2 <- DT::renderDT(server = FALSE, {
      measure_2 <- exits()
      measure_2 |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Clients Exiting to PH" = success_clients,
                      "Total Clients" = clients,
                      "Percent to PH" = Average) |> 
        datatable_default()
    })
    
    #### Measure 3: Exits to Temp or Permanent Housing
    exits_temp <- eventReactive({
      list(input$project_type_3, input$date_range_3)
    },{
      start_date <- as.Date(input$date_range_3[1])
      end_date <- as.Date(input$date_range_3[2])
      
      qpr_leavers <- qpr_leavers()
      
      .so <- qpr_leavers$ProjectType %in% c(4)
      .exited <- qpr_leavers |> 
        HMIS::exited_between(start_date, end_date, lgl = TRUE)
      
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
      
      data
    })
    
    
    output$ps_plot_3 <-
      plotly::renderPlotly({
        measure_3 <- exits_temp()
        
        # Define goals for different project types
        goals <- list(
          "Street Outreach" = 0.6
        )
        
        qpr_plotly(measure_3, title = "Exits to Temporary or Permanent Housing", 
                   xaxis_title = "Number of Clients", yaxis_title = "Percent to Housing",
                   project_type = input$project_type_3,
                   goals = goals, rect_above_line = FALSE)
      })
    
    
    output$ps_table_3 <- DT::renderDT(server = FALSE, {
      measure_3 <- exits_temp()
      measure_3 |>
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Clients to Housing" = success_clients,
                      "Total Clients" = clients,
                      "Percent to Housing" = Average) |> 
        datatable_default()
    })
    
    #### Measure 4: Non-cash Benefits at Exit
    benefits_at_exit <- eventReactive({
      list(input$project_type_4, input$date_range_4)
    }, {
      start_date <- as.Date(input$date_range_4[1])
      end_date <- as.Date(input$date_range_4[2])
      
      qpr_benefits <- qpr_benefits() |>
        HMIS::exited_between(start_date, end_date)
      
      data <- dplyr::left_join(
        # all_hhs
        qpr_benefits |> 
          dplyr::group_by(ProjectName, ProjectType) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        qpr_benefits |> 
          dplyr::filter(BenefitsFromAnySource == 1) |> 
          dplyr::group_by(ProjectName, ProjectType) |>
          dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName","ProjectType")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |>
        dplyr::filter(ProjectType == input$project_type_4) |> 
        dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
      
      data
    })
    
    
    
    output$ps_plot_4 <-
      plotly::renderPlotly({
        measure_4 <- benefits_at_exit()
        
        # Define goals for different project types
        goals <- list(
          "Emergency Shelter – Entry Exit" = 0.18,
          "PH – Rapid Re-Housing" = 0.18,
          "Transitional Housing" = 0.28,
          "PH – Permanent Supportive Housing" = 0.30
        )
        
        qpr_plotly(measure_4, title = "Non-Cash Benefits at Exit",
                   x_col = "TotalHHs", y_col = "Percent",
                   xaxis_title = "Number of Clients Exiting", yaxis_title = "Percent of Clients with Benefits",
                   project_type = input$project_type_4,
                   goals = goals, rect_above_line = FALSE)
      })
    
    
    output$ps_table_4 <- DT::renderDT(server = FALSE, {
      measure_4 <- benefits_at_exit()
      measure_4 |>
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Clients with Benefits" = BenefitsAtExit,
          "Total Clients" = TotalHHs,
          "Percent with Benefits" = Percent
        ) |> 
        datatable_default()
    })
    
    #### Measure 5: Health Insurance at Exit
    health_at_exit <- eventReactive({
      list(input$project_type_5, input$date_range_5)
      }, {
      start_date <- as.Date(input$date_range_5[1])
      end_date <- as.Date(input$date_range_5[2])
      
      qpr_benefits <- qpr_benefits() |>
        HMIS::exited_between(start_date, end_date)
      
      data <- dplyr::left_join(
        # all_hhs
        qpr_benefits |> 
          dplyr::group_by(ProjectName, ProjectType) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        qpr_benefits |> 
          dplyr::filter(InsuranceFromAnySource == 1) |> 
          dplyr::group_by(ProjectName, ProjectType) |>
          dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName", "ProjectType")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |>
        dplyr::filter(ProjectType == input$project_type_5) |> 
        dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
      
      data
    })
    
    output$ps_plot_5 <-
      plotly::renderPlotly({
        measure_5 <- health_at_exit()
        
        # Define goals for different project types
        goals <- list(
          "Emergency Shelter – Entry Exit" = 0.75,
          "PH – Rapid Re-Housing" = 0.85,
          "Transitional Housing" = 0.85,
          "PH – Permanent Supportive Housing" = 0.85
        )
        
        qpr_plotly(measure_5, title = "Health Insurance at Exit",
                   x_col = "TotalHHs", y_col = "Percent",
                   xaxis_title = "Number of Clients Exiting", yaxis_title = "Percent of Clients with Benefits",
                   project_type = input$project_type_5,
                   goals = goals, rect_above_line = FALSE)
      })
    
    
    output$ps_table_5 <- DT::renderDT(server = FALSE, {
      measure_5 <- health_at_exit()
      
      measure_5 |> 
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Clients with Insurance" = InsuranceAtExit,
          "Total Clients" = TotalHHs,
          "Percent with Insurance" = Percent
        ) |>
        datatable_default()
    })
    
    #### Measure 6: Increase in Income
    increase_income <- eventReactive({
      list(input$project_type_6, input$date_range_6)
    }, {
      start_date <- as.Date(input$date_range_6[1])
      end_date <- as.Date(input$date_range_6[2])
      
      qpr_income <- qpr_income() |>
        HMIS::exited_between(start_date, end_date)
      
      data <- dplyr::left_join(
        # all_hhs
        qpr_income |>
          dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        qpr_income |>
          dplyr::filter(Difference > 0) |> 
          dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
          dplyr::summarise(Increased = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName", "ProjectType", "ProjectCounty", "ProjectRegion")
      ) |>
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |>
        dplyr::filter(ProjectType == input$project_type_6) |>
        dplyr::mutate(Percent = Increased / TotalHHs)
      
      data
    })
    
    
    
    output$ps_plot_6 <-
      plotly::renderPlotly({
        measure_6 <- increase_income()
        
        # Define goals for different project types
        goals <- list(
          "Emergency Shelter – Entry Exit" = 0.18,
          "PH – Rapid Re-Housing" = 0.18,
          "Transitional Housing" = 0.28,
          "PH – Permanent Supportive Housing" = 0.30
        )
        
        qpr_plotly(measure_6, title = "Households Increasing Their Income",
                   x_col = "TotalHHs", y_col = "Percent",
                   xaxis_title = "Number of Clients Exiting", yaxis_title = "Percent of Clients with Increased Income",
                   project_type = input$project_type_6,
                   goals = goals, rect_above_line = FALSE)
      })
    
    
    output$ps_table_6 <- DT::renderDT(server = FALSE, {
      measure_6 <- increase_income()
      
      measure_6 |> 
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Project County" = ProjectCounty,
          "Project Region" = ProjectRegion,
          "Total Clients" = TotalHHs,
          "Total that Increased Income" = Increased,
          "Percent that Increased Income" = Percent
        ) |>
        datatable_default()
    })
    
    #### Measure 7: Rapid Placement RRH
    rrh_enterers <- eventReactive({
      list(input$date_range_7)
    }, {
      start_date <- as.Date(input$date_range_7[1])
      end_date <- as.Date(input$date_range_7[2])
      
      qpr_rrh_enterers <- qpr_rrh_enterers() |>
        HMIS::entered_between(start_date, end_date) |> 
        dplyr::filter(!is.na(MoveInDateAdjust))
      
        data <- qpr_rrh_enterers |>
          dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
          dplyr::mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) |>
          dplyr::summarise(AvgDaysToHouse = round(mean(DaysToHouse), 0), .groups = "drop_last",
                           clients = dplyr::n()) |>
          dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType))
      
      data
    })
      
      
    output$ps_plot_7 <-
      plotly::renderPlotly({
        measure_7 <- rrh_enterers()
        
        goals <- list("PH – Rapid Re-Housing" = 21)
        
        qpr_plotly(measure_7, title = "RRH Placement",
                   x_col = "clients", y_col = "AvgDaysToHouse",
                   xaxis_title = "Number of Clients", yaxis_title = "Average Days to House",
                   project_type = "PH – Rapid Re-Housing",
                   goals = goals, rect_above_line = TRUE)
      })
    
    
    output$ps_table_7 <- DT::renderDT(server = FALSE, {
      measure_7 <- rrh_enterers()
      
      measure_7 |> 
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Project County" = ProjectCounty,
          "Project Region" = ProjectRegion,
          "Average Days to House" = AvgDaysToHouse,
          "Total Clients" = clients
        ) |>
        datatable_default()
    })
  })
}

