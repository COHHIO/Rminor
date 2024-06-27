qpr_dependencies$NCB <- c(
  "qpr_benefits"
)
qpr_expr$noncash_benefits <- list()
qpr_expr$noncash_benefits$expr <- rlang::expr({
  req(input$date_range, input$region)
  qpr_benefits() |>
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$noncash_benefits$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(BenefitsFromAnySource == 1) |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
    by = "ProjectName"
  ) |> 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  icon = "shopping-cart",
                  color = "fuchsia",
                  value = scales::percent(.data$Percent),
                  title = "Households Exiting With Non Cash Benefits",
                  subtitle = paste(.data$BenefitsAtExit, 
                                   "out of",
                                   .data$TotalHHs, 
                                   "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})


qpr_expr$noncash_benefits$details <- rlang::expr({
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


#### Expression for youth
qpr_dependencies$NCB_youth <- c(
  "qpr_benefits"
)
qpr_expr$noncash_benefits_youth <- list()
qpr_expr$noncash_benefits_youth$expr <- rlang::expr({
  req(input$date_range, input$region)
  qpr_benefits() |>
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$noncash_benefits_youth$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(BenefitsFromAnySource == 1) |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
    by = "ProjectName"
  ) |> 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  icon = "shopping-cart",
                  color = "fuchsia",
                  value = scales::percent(.data$Percent),
                  title = "Households Exiting With Non Cash Benefits",
                  subtitle = paste(.data$BenefitsAtExit, 
                                   "out of",
                                   .data$TotalHHs, 
                                   "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})


qpr_expr$noncash_benefits_youth$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Emergency Shelter", "Transitional Housing", "Rapid Re-housing", "Permanent Supportive Housing"),
    Goal = c("At least 10% of households in Youth ES projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 60% of households in Youth TH projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 70% of households in Youth RRH projects will gain or increase employment or non-employment cash income during the reporting period or at exit",
             "At least 75% of households in Youth PSH projects will gain or increase employment or non-employment cash income during the reporting period or at exit"),
    HowCalculated = c("Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households served by the project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households served by the project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households who entered an project",
                      "Number of households who either gained or increased earned income or who gained or increased non-employment cash income / number of households who entered a PSH project")
  ) |>
    DT::datatable(escape = FALSE)
})
