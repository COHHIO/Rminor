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

# qpr_expr$income_growth$expr <- rlang::expr({
#   .PT_nm <- names(ProjectType()) 
#   
#   ReportStart <- Report()$Start
#   ReportEnd <- Report()$End
#    
#   meeting_objective <- qpr_income() %>%
#     dplyr::filter(
#       ProjectRegion %in% c(input$Region) &
#         ProjectType == .PT_nm &
#         HMIS::stayed_between(., ReportStart, ReportEnd) &
#         Difference > 0
#     ) %>%
#     dplyr::group_by(FriendlyProjectName,
#                     ProjectType,
#                     ProjectCounty,
#                     ProjectRegion) %>%
#     dplyr::summarise(Increased = dplyr::n())
#   
#   # calculating the total households for comparison
#   all_hhs <- qpr_income() %>%
#     dplyr::filter(ProjectRegion %in% c(input$Region) &
#                     ProjectType == .PT_nm &
#                     HMIS::stayed_between(., ReportStart, ReportEnd)) %>%
#     dplyr::group_by(FriendlyProjectName,
#                     ProjectType,
#                     ProjectCounty,
#                     ProjectRegion) %>%
#     dplyr::summarise(TotalHHs = dplyr::n())
#   
#   IncreasedIncome <- all_hhs %>%
#     dplyr::left_join(
#       meeting_objective,
#       by = c("FriendlyProjectName",
#              "ProjectType",
#              "ProjectCounty",
#              "ProjectRegion")
#     )
#   
#   IncreasedIncome[is.na(IncreasedIncome)] <- 0
#   
#   IncreasedIncome <- IncreasedIncome %>%
#     dplyr::mutate(Percent = Increased / TotalHHs)
#   
#   IncomeGoal <-
#     goals() %>%
#     dplyr::filter(Measure == "Gain or Increase Income" & 
#                     ProjectType %in% unlist(ProjectType())) %>%
#     dplyr::distinct(Goal)
#   IncomeGoal[["ProjectType"]] <- .PT_nm
#   
#   title <- paste0("Increased Income\n",
#                   .PT_nm, "\n",
#                   Report()$Start, " to ", Report()$End)
#   
#   stagingIncome <- IncreasedIncome %>%
#     dplyr::left_join(IncomeGoal, by = "ProjectType") %>%
#     dplyr::filter(ProjectType == .PT_nm &
#                     ProjectRegion %in% c(input$Region)) %>%
#     dplyr::mutate(
#       hover = paste0(
#         FriendlyProjectName,
#         "\nIncreased Income: ", Increased,
#         "\nTotal Households: ", TotalHHs,
#         "\n", as.integer(Percent * 100), "%",
#         sep = "\n"
#       )
#     )
#   attr(stagingIncome, "title") <- title
#   stagingIncome
# })

qpr_expr$income_growth$datatable <- rlang::expr({
  data_env() |>
    dplyr::mutate(EntryIncome = scales::dollar(EntryIncome, accuracy = .01),
                  RecentIncome = scales::dollar(RecentIncome, accuracy = .01),
                  Difference = scales::dollar(Difference, accuracy = .01)) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      ExitDate,
      "Income at Entry" = EntryIncome,
      "Most Recent Income" = RecentIncome,
      "Income Difference" = Difference
    ) |> 
    datatable_default(escape = FALSE)
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
