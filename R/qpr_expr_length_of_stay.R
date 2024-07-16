qpr_dependencies$length_of_stay <- c(
  "goals",
  "qpr_leavers"
)
qpr_expr$length_of_stay <- list()
qpr_expr$length_of_stay$expr <- rlang::expr({
  req(input$date_range, input$region)
  data <- qpr_leavers() |> 
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(((
      !is.na(MoveInDateAdjust) & ProjectType == 13
    ) |
      (
        !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
      )) &
      ProjectName == input$region
    )
  
  data
})

qpr_expr$length_of_stay$infobox <- rlang::expr({
  data_env() |> 
    dplyr::group_by(ProjectName) |>
    dplyr::summarise(Average = round(mean(DaysinProject), 1),
                     Median = median(DaysinProject), .groups = "drop_last") |> 
    qpr_infobox(icon = "clock",
                value = paste("Average", 
                              .data$Average, 
                              "/ Median", 
                              .data$Median,
                              "days"),
                title = "Average and Median Length of Stay",
                subtitle = "Length of Stay in Program's Housing"
    )
})


qpr_expr$length_of_stay$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Emergency Shelter", "Emergency Shelter", "Transitional Housing", "Transitional Housing"),
    Goal = c("Emergency Shelter (ES) projects will have a household average length of stay of no more than 40 days",
             "ES projects will have a household median length of stay of no more than 40 days",
             "Rapid Re-housing (RRH) projects will have a household average length of stay of no more than 150 days",
             "RRH projects will have a household median length of stay of no more than 150 days",
             "Transitional Housing (TH) projects will have a household average length of stay of no more than 240 days",
             "TH projects will have a household median length of stay of no more than 240 days"),
    HowCalculated = c("Average length of stay for households who have exited",
                      "Median length of stay for households who have exited",
                      "Average length of stay for households who have exited from Housing Move-In Date to Exit",
                      "Median length of stay for households who have exited from Housing Move-In Date to Exit",
                      "Average length of stay for households who have exited",
                      "Median length of stay for households who have exited")
  ) |> 
    DT::datatable(escape = FALSE)
})

#### Expressions for youth length of stay
qpr_dependencies$length_of_stay_youth <- c(
  "goals",
  "qpr_leavers"
)

qpr_expr$length_of_stay_youth <- list()
qpr_expr$length_of_stay_youth$expr <- rlang::expr({
  req(input$date_range, input$region)
  data <- qpr_leavers() |> 
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(((
      !is.na(MoveInDateAdjust) & ProjectType == 13
    ) |
      (
        !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
      )) &
      ProjectName == input$region
    ) |> 
    dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp"))
  
  message("length_of_stay_youth data processed: ", nrow(data), " rows")
  data
})

qpr_expr$length_of_stay_youth$infobox <- rlang::expr({
  data <- data_env()
  message("length_of_stay infobox data: ", nrow(data), " rows")
  
  data_env() |> 
    dplyr::group_by(ProjectName) |>
    dplyr::summarise(Average = round(mean(DaysinProject), 1),
                     Median = median(DaysinProject), .groups = "drop_last") |> 
    qpr_infobox(icon = "clock",
                value = paste("Average", 
                              .data$Average, 
                              "/ Median", 
                              .data$Median,
                              "days"),
                title = "Average and Median Length of Stay for Youth",
                subtitle = "Length of Stay in Program's Housing"
    )
})


qpr_expr$length_of_stay_youth$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Emergency Shelter", "Emergency Shelter", "Transitional Housing",
                    "Transitional Housing", "Rapid Re-housing", "Rapid Re-housing"),
    Goal = c("Emergency Shelter (ES) projects will have a household average length of stay of no more than 20 days",
             "ES projects will have a household median length of stay of no more than 20 days",
             "Transitional Housing (TH) projects will have a household average length of stay of no more than 240 days",
             "TH projects will have a household median length of stay of no more than 240 days",
             "Youth Rapid Re-housing (RRH) projects will have an average household length of stay of no more than 200 days",
             "Youth Rapid Re-housing (RRH) projects will have an median household length of stay of no more than 200 days"),
    HowCalculated = c("Average length of stay for households who have exited",
                      "Median length of stay for households who have exited",
                      "Average length of stay for households who have exited",
                      "Median length of stay for households who have exited")
  ) |> 
    DT::datatable(escape = FALSE)
})