qpr_expr$income_growth <- list()

qpr_expr$income_growth$expr <- rlang::expr({
  qpr_income() |>
    HMIS::stayed_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$income_growth$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |>
      dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |>
      dplyr::filter(Difference > 0) |> 
      dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
      dplyr::summarise(Increased = dplyr::n(), .groups = "drop_last"),
    by = c("ProjectName", "ProjectType", "ProjectCounty", "ProjectRegion")
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = Increased / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Households Increasing Their Income",
                  color = "success",
                  icon = "hand-holding-usd",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$Increased, "out of", .data$TotalHHs, "households served")
    )
  } else {
    .args <- list(title = HTML("No clients in date range. If you think this is inaccurate, please email <a href='mailto:hmis@cohhio.org' target='_blank'>hmis@cohhio.org</a>!"), .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})


qpr_expr$income_growth$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Emergency Shelter", "Transitional Housing", "Rapid Re-housing", "Permanent Supportive Housing"),
    Goal = c("At least 18% of households in ES projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 28% of households in TH projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 18% of households in RRH projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 30% of households in PSH projects will gain or increase employment or non-employment cash income during the reporting period or at exit"),
    HowCalculated = c("Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households served by the project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households served by the project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households who entered an project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households who entered a PSH project")
  ) |> 
    DT::datatable(escape = FALSE)
})

#### Expressions for youth
qpr_expr$income_growth_youth <- list()

qpr_expr$income_growth_youth$expr <- rlang::expr({
  qpr_income() |>
    HMIS::stayed_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$income_growth_youth$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |>
      dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |>
      dplyr::filter(Difference > 0) |> 
      dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
      dplyr::summarise(Increased = dplyr::n(), .groups = "drop_last"),
    by = c("ProjectName", "ProjectType", "ProjectCounty", "ProjectRegion")
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = Increased / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Households Increasing Their Income",
                  color = "success",
                  icon = "hand-holding-usd",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$Increased, "out of", .data$TotalHHs, "households served")
    )
  } else {
    .args <- list(title = HTML("No clients in date range. If you think this is inaccurate, please email <a href='mailto:hmis@cohhio.org' target='_blank'>hmis@cohhio.org</a>!"), .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})


qpr_expr$income_growth_youth$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Emergency Shelter", "Transitional Housing", "Rapid Re-housing", "Permanent Supportive Housing"),
    Goal = c("At least 10% of households in Youth ES projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 10% of households in Youth TH projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 18% of households in Youth RRH projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 14% of households in Youth PSH projects will gain or increase employment or non-employment cash income during the reporting period or at exit"),
    HowCalculated = c("Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households served by the project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households served by the project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households who entered an project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households who entered a PSH project")
  ) |> 
    DT::datatable(escape = FALSE)
})
