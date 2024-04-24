qpr_dependencies$length_of_stay <- c(
  "goals",
  "qpr_leavers"
)
qpr_expr$length_of_stay <- list()
qpr_expr$length_of_stay$expr <- rlang::expr({
  req(input$date_range, input$region)
  qpr_leavers() |> 
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(((
      !is.na(MoveInDateAdjust) & ProjectType == 13
    ) |
      (
        !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
      )) &
      ProjectName == input$region
    ) 
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

qpr_expr$length_of_stay$datatable <- rlang::expr({
  data_env() |>
    dplyr::arrange(dplyr::desc(DaysinProject)) |>
    dplyr::select(
      UniqueID,
      "Bed Start" = EntryAdjust,
      ExitDate,
      "Days in Program" = DaysinProject
    ) |> 
    datatable_default(escape = FALSE)
})

qpr_expr$length_of_stay$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Emergency Shelter", "Emergency Shelter", "Transitional Housing", "Transitional Housing"),
    Goal = c("Emergency Shelter (ES) projects will have a household average length of stay of no more than 40 days",
             "ES projects will have a household median length of stay of no more than 40 days",
             "Transitional Housing (TH) projects will have a household average length of stay of no more than 240 days",
             "TH projects will have a household median length of stay of no more than 240 days"),
    HowCalculated = c("Average length of stay for households who have exited",
                      "Median length of stay for households who have exited",
                      "Average length of stay for households who have exited",
                      "Median length of stay for households who have exited")
  ) |> 
    DT::datatable(escape = FALSE)
})