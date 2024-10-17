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
                                           "PH – Rapid Re-Housing",
                                           "Transitional Housing"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_1"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_1a")),
                             br(),
                             plotly::plotlyOutput(ns("ps_plot_1b")),
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_1")),
                             br(),
                             br(),
                             br(),
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
                                           "Street Outreach",
                                           "PH – Permanent Supportive Housing"),
                               selected = "Emergency Shelter – Entry Exit",
                               multiple = FALSE
                             ),
                             dateRangeInput(inputId = ns("date_range_2"),
                                            label = "Date Range",
                                            start = start_date,
                                            end = end_date
                             ),
                             plotly::plotlyOutput(ns("ps_plot_2")),
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_2")),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns("ps_table_2"))
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
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_3")),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns("ps_table_3"))
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
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_4")),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns("ps_table_4"))
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
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_5")),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns("ps_table_5"))
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
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_6")),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns("ps_table_6"))
                           )
                  ),
                  tabPanel("Rapid Placement for RRH",
                           h3("Rapid Placement Summary"),
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
                             br(),
                             DT::dataTableOutput(ns("ps_bos_table_7")),
                             br(),
                             br(),
                             br(),
                             DT::dataTableOutput(ns("ps_table_7"))
                           )
                  )
  )
}

# Define the server module
mod_body_performance_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #### Measure 1: Length of Stay
    
    # Define a helper function
    calculate_measure_1 <- function(data, project_type, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      filtered_data <- data |>
        dplyr::filter(ProgramCoC == "OH-507") |>
        HMIS::exited_between(start_date, end_date) |> 
        dplyr::filter(((!is.na(MoveInDateAdjust) & ProjectType == 13) |
                         (!is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8))))
      
      if (group_by_project_name) {
        grouped_data <- filtered_data |>
          dplyr::group_by(ProjectName, ProjectType) |>
          dplyr::summarise(Average = round(mean(DaysinProject), 1),
                           Median = median(DaysinProject), clients = dplyr::n(), .groups = "drop_last")
      } else {
        grouped_data <- filtered_data |>
          dplyr::group_by(ProjectType) |>
          dplyr::summarise(Average = round(mean(DaysinProject), 1),
                           Median = median(DaysinProject), clients = dplyr::n(), .groups = "drop_last") |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      final_data <- grouped_data |>
        dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |> 
        dplyr::filter(ProjectType == project_type)
      
      return(final_data)
    }
    
    length_of_stay <- eventReactive({
      list(input$project_type_1, input$date_range_1)
    }, {
      req(input$project_type_1, input$date_range_1)
      
      data <- qpr_leavers()
      calculate_measure_1(data, input$project_type_1, input$date_range_1, group_by_project_name = TRUE)
    })
    
    length_of_stay_bos <- eventReactive({
      list(input$project_type_1, input$date_range_1)
    }, {
      req(input$project_type_1, input$date_range_1)
      
      data <- qpr_leavers()
      calculate_measure_1(data, input$project_type_1, input$date_range_1, group_by_project_name = FALSE)
    })
    
    output$ps_plot_1a <- plotly::renderPlotly({
      measure_1 <- length_of_stay()
      
      # Define goals for different project types
      goals <- list(
        "Emergency Shelter – Entry Exit" = 40,
        "PH – Rapid Re-Housing" = 150,
        "Transitional Housing" = 240
      )
      
      # Extract the goal for the selected project type
      selected_goal <- goals[[input$project_type_1]]
      
      # Calculate the number of projects meeting the goal
      num_points_total <- nrow(measure_1)
      num_points_outside <- sum(measure_1$Average <= selected_goal)
      
      # Add annotation
      annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
      
      plot <- qpr_plotly(measure_1, title = "Average Length of Stay", xaxis_title = "Number of Clients",
                 yaxis_title = "Number of Days", project_type = input$project_type_1,
                 goals = goals)
      
      plot |> 
        plotly::layout(
          annotations = list(
            x = 1, 
            y = 1.1, 
            text = annotation_text, 
            showarrow = FALSE, 
            xref = 'paper', 
            yref = 'paper',
            xanchor = 'right',
            yanchor = 'top',
            font = list(size = 14, color = "black")
          ),
          margin = list(t = 60) # Adjust top margin if needed
        )
    })
    
    output$ps_plot_1b <- plotly::renderPlotly({
      measure_1 <- length_of_stay()
      
      # Define goals for different project types
      goals <- list(
        "Emergency Shelter – Entry Exit" = 40,
        "PH – Rapid Re-Housing" = 150,
        "Transitional Housing" = 240
      )
      
      # Extract the goal for the selected project type
      selected_goal <- goals[[input$project_type_1]]
      
      # Calculate the number of projects meeting the goal
      num_points_total <- nrow(measure_1)
      num_points_outside <- sum(measure_1$Median <= selected_goal)
      
      # Add annotation
      annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
      
      plot <- qpr_plotly(measure_1, title = "Median Length of Stay", xaxis_title = "Number of Clients",
                 y_col = "Median", yaxis_title = "Number of Days", 
                 project_type = input$project_type_1,
                 goals = goals)
      
      plot |> 
        plotly::layout(
          annotations = list(
            x = 1, 
            y = 1.1, 
            text = annotation_text, 
            showarrow = FALSE, 
            xref = 'paper', 
            yref = 'paper',
            xanchor = 'right',
            yanchor = 'top',
            font = list(size = 14, color = "black")
          ),
          margin = list(t = 60) # Adjust top margin if needed
        )
    })
    
    output$ps_table_1 <- DT::renderDT(server = FALSE, {
      measure_1 <- length_of_stay()
      
      measure_1 |>
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Number of Clients" = clients) |> 
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                             'Performance by Project'))
    })
    
    output$ps_bos_table_1 <- DT::renderDT(server = FALSE, {
      bos_measure_1 <- length_of_stay_bos()
      
      bos_measure_1 |>
        dplyr::select(ProjectName, ProjectType, Average, Median, clients) |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Number of Clients" = clients) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance'))
    })
    
    #### Measure 2: Exits to Permanent Housing
    calculate_measure_2 <- function(data, project_type, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])

      qpr_leavers <- data |> 
        dplyr::filter(ProgramCoC == "OH-507")
      
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
      
      if (group_by_project_name) {
        TotalCount <- TotalHHsSuccessfulPlacement |> 
          dplyr::count(ProjectName, ProjectType) |> 
          dplyr::rename(clients = n)
        
        SuccessCount <- SuccessfullyPlaced |>
          dplyr::count(ProjectName, ProjectType) |>
          dplyr::rename(success_clients = n)
        
        data <- TotalCount |> 
          dplyr::left_join(SuccessCount, c("ProjectName","ProjectType")) |>
          {\(.) {replace(.,is.na(.),0)}}() |>
          dplyr::mutate(Percent = success_clients / clients) |> 
          dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |> 
          dplyr::filter(ProjectType == project_type)
      } else {
        TotalCount <- TotalHHsSuccessfulPlacement |> 
          dplyr::count(ProjectType) |> 
          dplyr::rename(clients = n)
        
        SuccessCount <- SuccessfullyPlaced |>
          dplyr::count(ProjectType) |>
          dplyr::rename(success_clients = n)
        
        data <- TotalCount |> 
          dplyr::left_join(SuccessCount, by = "ProjectType") |>
          {\(.) {replace(.,is.na(.),0)}}() |>
          dplyr::mutate(Percent = success_clients / clients) |> 
          dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |> 
          dplyr::filter(ProjectType == project_type) |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      
      
      data
    }
    
    exits <- eventReactive({
      list(input$project_type_2, input$date_range_2)
    }, {
      req(input$project_type_2, input$date_range_2)
      data <- qpr_leavers()
      calculate_measure_2(data, input$project_type_2, input$date_range_2, group_by_project_name = TRUE)
    })
    
    exits_bos <- eventReactive({
      list(input$project_type_2, input$date_range_2)
    }, {
      req(input$project_type_2, input$date_range_2)
      data <- qpr_leavers()
      calculate_measure_2(data, input$project_type_2, input$date_range_2, group_by_project_name = FALSE)
    })
    

    output$ps_plot_2 <-
      plotly::renderPlotly({
        measure_2 <- exits()
        
        # Define goals for different project types
        goals <- list(
            "Emergency Shelter – Entry Exit" = 0.40,
            "PH – Rapid Re-Housing" = 0.83,
            "Transitional Housing" = 0.83,
            "Street Outreach" = 0.30,
            "PH – Permanent Supportive Housing" = 0.90
        )
        
        # Extract the goal for the selected project type
        selected_goal <- goals[[input$project_type_2]]
        
        # Calculate the number of projects meeting the goal
        num_points_total <- nrow(measure_2)
        num_points_outside <- sum(measure_2$Percent >= selected_goal)
        
        # Add annotation
        annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
        
        plot <- qpr_plotly(measure_2, title = "Exits to Permanent Housing", x_col = "clients",
                   xaxis_title = "Number of Clients", 
                   y_col = "Percent", yaxis_title = "Percent to Permanent Housing",
                   y_label = "Percent to PH",
                   project_type = input$project_type_2,
                   goals = goals, rect_above_line = FALSE, percent_format = TRUE)
        
        plot |> 
          plotly::layout(
            annotations = list(
              x = 1, 
              y = 1.1, 
              text = annotation_text, 
              showarrow = FALSE, 
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 14, color = "black")
            ),
            margin = list(t = 60) # Adjust top margin if needed
          )
      })
    
    
    output$ps_table_2 <- DT::renderDT(server = FALSE, {
      measure_2 <- exits()
      
      measure_2 |>
        dplyr::select(ProjectName, ProjectType, Percent, clients) |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Percent to PH" = Percent,
                      "Number of Clients" = clients
        ) |>
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Performance by Project')) |> 
        DT::formatPercentage('Percent to PH', 1)
    })
    
    output$ps_bos_table_2 <- DT::renderDT(server = FALSE, {
      bos_measure_2 <- exits_bos()
      
      bos_measure_2 |>
        dplyr::select(ProjectName, ProjectType, Percent, clients) |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Percent to PH" = Percent,
                      "Number of Clients" = clients
                      ) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance')) |> 
        DT::formatPercentage('Percent to PH', 1)
    })
    
    #### Measure 3: Exits to Temp or Permanent Housing
    calculate_measure_3 <- function(data, project_type, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      qpr_leavers <- data |>
        dplyr::filter(ProgramCoC == "OH-507")
      
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
      
      if (group_by_project_name) {
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
          dplyr::filter(ProjectType == project_type) |> 
          dplyr::mutate(Average = success_clients/clients)
      } else {
        TotalCountTemp <- TotalHHsSuccessfulPlacementTemp |> 
          dplyr::count(ProjectType) |> 
          dplyr::rename(clients = n)
        
        SuccessCountTemp <- SuccessfullyPlacedTemp |>
          dplyr::count(ProjectType) |>
          dplyr::rename(success_clients = n)
        
        data <- TotalCountTemp |> 
          dplyr::left_join(SuccessCountTemp, by = c("ProjectType")) |>
          {\(.) {replace(.,is.na(.),0)}}() |>
          dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |>
          dplyr::filter(ProjectType == project_type) |> 
          dplyr::mutate(Average = success_clients/clients) |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      data
      
    }
    
    exits_temp <- eventReactive({
      list(input$project_type_3, input$date_range_3)
    }, {
      req(input$project_type_3, input$date_range_3)
      data <- qpr_leavers()
      calculate_measure_3(data, input$project_type_3, input$date_range_3, group_by_project_name = TRUE)
    })
    
    exits_temp_bos <- eventReactive({
      list(input$project_type_3, input$date_range_3)
    }, {
      req(input$project_type_3, input$date_range_3)
      data <- qpr_leavers()
      calculate_measure_3(data, input$project_type_3, input$date_range_3, group_by_project_name = FALSE)
    })
    
    output$ps_plot_3 <-
      plotly::renderPlotly({
        measure_3 <- exits_temp()
        
        # Define goals for different project types
        goals <- list(
          "Street Outreach" = 0.6
        )
        
        # Extract the goal for the selected project type
        selected_goal <- goals[[input$project_type_3]]
        
        # Calculate the number of projects meeting the goal
        num_points_total <- nrow(measure_3)
        num_points_outside <- sum(measure_3$Average >= selected_goal)
        
        # Add annotation
        annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
        
        plot <- qpr_plotly(measure_3, title = "Exits to Temporary or Permanent Housing", 
                   xaxis_title = "Number of Clients", yaxis_title = "Percent to Housing",
                   project_type = input$project_type_3,
                   goals = goals, rect_above_line = FALSE, percent_format = TRUE)
        
        plot |> 
          plotly::layout(
            annotations = list(
              x = 1, 
              y = 1.1, 
              text = annotation_text, 
              showarrow = FALSE, 
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 14, color = "black")
            ),
            margin = list(t = 60) # Adjust top margin if needed
          ) 
      })
    
    
    output$ps_table_3 <- DT::renderDT(server = FALSE, {
      measure_3 <- exits_temp()
      measure_3 |>
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Clients to Housing" = success_clients,
                      "Total Clients" = clients,
                      "Percent to Housing" = Average) |> 
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Performance by Project')) |> 
        DT::formatPercentage('Percent to Housing', 1)
    })
    
    output$ps_bos_table_3 <- DT::renderDT(server = FALSE, {
      bos_measure_3 <- exits_temp_bos()
      
      bos_measure_3 |>
        dplyr::select(ProjectName, ProjectType, Percent, clients) |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Percent to PH" = Percent,
                      "Number of Clients" = clients
        ) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance')) |> 
        DT::formatPercentage('Percent to PH', 1)
    })
    
    #### Measure 4: Non-cash Benefits at Exit
    calculate_measure_4 <- function(data, project_type, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      qpr_benefits <- data |>
        dplyr::filter(ProgramCoC == "OH-507") |>
        HMIS::exited_between(start_date, end_date)
      
      if (group_by_project_name) {
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
          dplyr::filter(ProjectType == project_type) |> 
          dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
      } else {
        data <- dplyr::left_join(
          # all_hhs
          qpr_benefits |> 
            dplyr::group_by(ProjectType) |>
            dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
          # meeting_objective
          qpr_benefits |> 
            dplyr::filter(BenefitsFromAnySource == 1) |> 
            dplyr::group_by(ProjectType) |>
            dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
          by = c("ProjectType")
        ) |> 
          dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |>
          dplyr::filter(ProjectType == project_type) |> 
          dplyr::mutate(Percent = BenefitsAtExit / TotalHHs) |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      data
      
    }
    
    benefits_at_exit <- eventReactive({
      list(input$project_type_4, input$date_range_4)
    }, {
      req(input$project_type_4, input$date_range_4)
      data <- qpr_benefits()
      calculate_measure_4(data, input$project_type_4, input$date_range_4, group_by_project_name = TRUE)
    })
    
    benefits_at_exit_bos <- eventReactive({
      list(input$project_type_4, input$date_range_4)
    }, {
      req(input$project_type_4, input$date_range_4)
      data <- qpr_benefits()
      calculate_measure_4(data, input$project_type_4, input$date_range_4, group_by_project_name = FALSE)
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
        
        # Extract the goal for the selected project type
        selected_goal <- goals[[input$project_type_4]]
        
        # Calculate the number of projects meeting the goal
        num_points_total <- nrow(measure_4)
        num_points_outside <- sum(measure_4$Percent >= selected_goal)
        
        # Add annotation
        annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
        
        plot <- qpr_plotly(measure_4, title = "Non-Cash Benefits at Exit",
                   x_col = "TotalHHs", y_col = "Percent",
                   xaxis_title = "Number of Clients Exiting", yaxis_title = "Percent of Clients with Benefits",
                   project_type = input$project_type_4,
                   goals = goals, rect_above_line = FALSE, percent_format = TRUE)
        
        plot |> 
          plotly::layout(
            annotations = list(
              x = 1, 
              y = 1.1, 
              text = annotation_text, 
              showarrow = FALSE, 
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 14, color = "black")
            ),
            margin = list(t = 60) # Adjust top margin if needed
          )
      })
    
    
    output$ps_table_4 <- DT::renderDT(server = FALSE, {
      measure_4 <- benefits_at_exit()
      measure_4 |>
        dplyr::select(ProjectName, ProjectType, BenefitsAtExit, TotalHHs, Percent) |>
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Clients with Benefits" = BenefitsAtExit,
          "Total Clients" = TotalHHs,
          "Percent with Benefits" = Percent
        ) |> 
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                             'Performance by Project')) |> 
        DT::formatPercentage('Percent with Benefits', 1)
    })
    
    output$ps_bos_table_4 <- DT::renderDT(server = FALSE, {
      bos_measure_4 <- benefits_at_exit_bos()
      
      bos_measure_4 |>
        dplyr::select(ProjectName, ProjectType, BenefitsAtExit, TotalHHs, Percent) |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Clients with Benefits" = BenefitsAtExit,
                      "Total Clients" = TotalHHs,
                      "Percent with Benefits" = Percent
        ) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance')) |> 
        DT::formatPercentage('Percent with Benefits', 1)
    })
    
    #### Measure 5: Health Insurance at Exit
    calculate_measure_5 <- function(data, project_type, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      qpr_benefits <- data |>
        dplyr::filter(ProgramCoC == "OH-507") |>
        HMIS::exited_between(start_date, end_date)
      
      if (group_by_project_name) {
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
          dplyr::filter(ProjectType == project_type) |> 
          dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
      } else {
        data <- dplyr::left_join(
          # all_hhs
          qpr_benefits |> 
            dplyr::group_by(ProjectType) |>
            dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
          # meeting_objective
          qpr_benefits |> 
            dplyr::filter(InsuranceFromAnySource == 1) |> 
            dplyr::group_by(ProjectType) |>
            dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
          by = c("ProjectType")
        ) |> 
          dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |>
          dplyr::filter(ProjectType == project_type) |> 
          dplyr::mutate(Percent = InsuranceAtExit / TotalHHs) |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      data
      
    }
    
    health_at_exit <- eventReactive({
      list(input$project_type_5, input$date_range_5)
    }, {
      req(input$project_type_5, input$date_range_5)
      data <- qpr_benefits()
      calculate_measure_5(data, input$project_type_5, input$date_range_5, group_by_project_name = TRUE)
    })
    
    health_at_exit_bos <- eventReactive({
      list(input$project_type_5, input$date_range_5)
    }, {
      req(input$project_type_5, input$date_range_5)
      data <- qpr_benefits()
      calculate_measure_5(data, input$project_type_5, input$date_range_5, group_by_project_name = FALSE)
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
        
        # Extract the goal for the selected project type
        selected_goal <- goals[[input$project_type_5]]
        
        # Calculate the number of projects meeting the goal
        num_points_total <- nrow(measure_5)
        num_points_outside <- sum(measure_5$Percent >= selected_goal)
        
        # Add annotation
        annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
        
        plot <- qpr_plotly(measure_5, title = "Health Insurance at Exit",
                   x_col = "TotalHHs", y_col = "Percent",
                   xaxis_title = "Number of Clients Exiting", yaxis_title = "Percent of Clients with Benefits",
                   project_type = input$project_type_5,
                   goals = goals, rect_above_line = FALSE, percent_format = TRUE)
        
        plot |> 
          plotly::layout(
            annotations = list(
              x = 1, 
              y = 1.1, 
              text = annotation_text, 
              showarrow = FALSE, 
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 14, color = "black")
            ),
            margin = list(t = 60) # Adjust top margin if needed
          )
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
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                             'Overall Balance of State Performance')) |> 
        DT::formatPercentage('Percent with Insurance', 1)
    })
    
    output$ps_bos_table_5 <- DT::renderDT(server = FALSE, {
      bos_measure_5 <- health_at_exit_bos()
      
      bos_measure_5 |>
        dplyr::select(ProjectName, ProjectType, InsuranceAtExit, TotalHHs, Percent) |> 
        dplyr::rename("Project Name" = ProjectName,
                      "Project Type" = ProjectType,
                      "Clients with Insurance" = InsuranceAtExit,
                      "Total Clients" = TotalHHs,
                      "Percent with Insurance" = Percent
        ) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance')) |> 
        DT::formatPercentage('Percent with Insurance', 1)
    })
    
    #### Measure 6: Increase in Income
    calculate_measure_6 <- function(data, project_type, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      qpr_income <- data |>
        dplyr::filter(ProgramCoC == "OH-507") |>
        HMIS::exited_between(start_date, end_date)
      
      if (group_by_project_name) {
        # By Program
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
          dplyr::filter(ProjectType == project_type) |>
          dplyr::mutate(Percent = Increased / TotalHHs)
      } else {
        # Balance of State
        data <- dplyr::left_join(
          # all_hhs
          qpr_income |>
            dplyr::group_by(ProjectType) |>
            dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
          # meeting_objective
          qpr_income |>
            dplyr::filter(Difference > 0) |> 
            dplyr::group_by(ProjectType) |>
            dplyr::summarise(Increased = dplyr::n(), .groups = "drop_last"),
          by = c("ProjectType")
        ) |>
          dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |>
          dplyr::filter(ProjectType == project_type) |>
          dplyr::mutate(Percent = Increased / TotalHHs) |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      data
    }
    
    increase_income <- eventReactive({
      list(input$project_type_6, input$date_range_6)
    }, {
      req(input$project_type_6, input$date_range_6)
      data <- qpr_income()
      calculate_measure_6(data, input$project_type_6, input$date_range_6, group_by_project_name = TRUE)
    })
    
    increase_income_bos <- eventReactive({
      list(input$project_type_6, input$date_range_6)
    }, {
      req(input$project_type_6, input$date_range_6)
      data <- qpr_income()
      calculate_measure_6(data, input$project_type_6, input$date_range_6, group_by_project_name = FALSE)
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
        
        # Extract the goal for the selected project type
        selected_goal <- goals[[input$project_type_6]]
        
        # Calculate the number of projects meeting the goal
        num_points_total <- nrow(measure_6)
        num_points_outside <- sum(measure_6$Percent >= selected_goal)
        
        # Add annotation
        annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
        
        plot <- qpr_plotly(measure_6, title = "Households Increasing Their Income",
                   x_col = "TotalHHs", y_col = "Percent",
                   xaxis_title = "Number of Clients Exiting", yaxis_title = "Percent of Clients with Increased Income",
                   project_type = input$project_type_6,
                   goals = goals, rect_above_line = FALSE, percent_format = TRUE)
        
        plot |> 
          plotly::layout(
            annotations = list(
              x = 1, 
              y = 1.1, 
              text = annotation_text, 
              showarrow = FALSE, 
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 14, color = "black")
            ),
            margin = list(t = 60) # Adjust top margin if needed
          )
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
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                             'Performance by Project')) |> 
        DT::formatPercentage('Percent that Increased Income', 1)
    })
    
    output$ps_bos_table_6 <- DT::renderDT(server = FALSE, {
      bos_measure_6 <- increase_income_bos()
      
      bos_measure_6 |>
        dplyr::select(ProjectName, ProjectType, TotalHHs, Increased, Percent) |> 
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Total Clients" = TotalHHs,
          "Total that Increased Income" = Increased,
          "Percent that Increased Income" = Percent
        ) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance')) |> 
        DT::formatPercentage('Percent that Increased Income', 1)
    })
    
    #### Measure 7: Rapid Placement RRH
    calculate_measure_7 <- function(data, date_range, group_by_project_name = TRUE) {
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      if (group_by_project_name) {
        qpr_rrh_enterers <- data |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          HMIS::entered_between(start_date, end_date) |> 
          dplyr::filter(!is.na(MoveInDateAdjust))
        
        data <- qpr_rrh_enterers |>
          dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
          dplyr::mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) |>
          dplyr::summarise(AvgDaysToHouse = round(mean(DaysToHouse), 0), .groups = "drop_last",
                           clients = dplyr::n()) |>
          dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType))
      } else {
        qpr_rrh_enterers <- data |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          HMIS::entered_between(start_date, end_date) |> 
          dplyr::filter(!is.na(MoveInDateAdjust))
        
        data <- qpr_rrh_enterers |>
          dplyr::group_by(ProjectType) |>
          dplyr::mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) |>
          dplyr::summarise(AvgDaysToHouse = round(mean(DaysToHouse), 0), .groups = "drop_last",
                           clients = dplyr::n()) |>
          dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |> 
          tibble::add_column(ProjectName = "Balance of State (OH-507)")
      }
      
      data
    }
    
    rrh_enterers <- eventReactive({
      list(input$date_range_7)
    }, {
      req(input$date_range_7)
      data <- qpr_rrh_enterers()
      calculate_measure_7(data, input$date_range_7, group_by_project_name = TRUE)
    })
    
    rrh_enterers_bos <- eventReactive({
      list(input$date_range_7)
    }, {
      req(input$date_range_7)
      data <- qpr_rrh_enterers()
      calculate_measure_7(data, input$date_range_7, group_by_project_name = FALSE)
    })
      
    output$ps_plot_7 <-
      plotly::renderPlotly({
        measure_7 <- rrh_enterers()
        
        goals <- list(
          "PH – Rapid Re-Housing" = 21
          )
        
        # Extract the goal for the selected project type
        selected_goal <- goals[["PH – Rapid Re-Housing"]]
        
        # Calculate the number of projects meeting the goal
        num_points_total <- nrow(measure_7)
        num_points_outside <- sum(measure_7$AvgDaysToHouse <= selected_goal)
        
        # Add annotation
        annotation_text <- paste0("Number of Projects Meeting Goal: ", num_points_outside, "/", num_points_total)
        
        plot <- qpr_plotly(measure_7, title = "RRH Placement",
                   x_col = "clients", y_col = "AvgDaysToHouse",
                   xaxis_title = "Number of Clients", yaxis_title = "Average Days to House",
                   project_type = "PH – Rapid Re-Housing",
                   goals = goals, rect_above_line = TRUE)
        
        plot |> 
          plotly::layout(
            annotations = list(
              x = 1,
              y = 1.1,
              text = annotation_text,
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 14, color = "black")
            ),
            margin = list(t = 60) # Adjust top margin if needed
          )
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
        datatable_default(caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                             'Performance by Project'))
    })
    
    output$ps_bos_table_7 <- DT::renderDT(server = FALSE, {
      bos_measure_7 <- rrh_enterers_bos()
      
      bos_measure_7 |>
        dplyr::select(ProjectName, ProjectType, AvgDaysToHouse, clients) |> 
        dplyr::rename(
          "Project Name" = ProjectName,
          "Project Type" = ProjectType,
          "Average Days to House" = AvgDaysToHouse,
          "Total Clients" = clients
        ) |>
        DT::datatable(options = list(dom = 't'),
                      caption = htmltools::tags$caption( style = 'caption-side: 
                                                         top; text-align: center; 
                                                         color:black;  font-size:125% ;',
                                                         'Overall Balance of State Performance'))
    })
  })
}

