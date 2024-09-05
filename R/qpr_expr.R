qpr_expr <- list(
  #### Health Insurance Measure
  health_insurance = list(
    expr = rlang::expr({
      qpr_benefits() |>
        HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectName == input$region)
  }),
    infobox = rlang::expr({
      .data <- dplyr::left_join(
        # all_hhs
        data_env() |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        data_env() |> 
          dplyr::filter(InsuranceFromAnySource == 1) |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
        dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
      
      if (nrow(.data) > 0) {
        .args <- list(.data = .data,
                      title = "Total Households Exiting With Health Insurance",
                      color = "gray-dark",
                      icon = "medkit",
                      value = scales::percent(.data$Percent),
                      subtitle = paste(.data$InsuranceAtExit, 
                                       "out of",
                                       .data$TotalHHs,
                                       "households")
        )
      } else {
        .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
      }
      
      do.call(qpr_infobox, .args)
    }),
    details = rlang::expr({
      tibble::tibble(
        ProjectType = c("Emergency Shelter", "Transitional Housing", "Rapid Re-housing", "Permanent Supportive Housing"),
        Goal = c("At least 75% of households in ES projects will receive at least one source of health insurance at program exit",
                 "At least 85% of households in TH projects will receive at least one source of health insurance at program exit",
                 "At least 85% of households in RRH projects will receive at least one source of health insurance at program exit",
                 "At least 85% of households in PSH projects will receive at least one source of health insurance at program exit"),
        HowCalculated = c("Number of households who exited with 1 or more sources of health insurance / number of households who exited the project",
                          "Number of households who exited with 1 or more sources of health insurance / number of households who exited the project",
                          "Number of households who exited with 1 or more sources of health insurance / number of households who exited RRH",
                          "Number of households who exited with 1 or more sources of health insurance / number of households that entered a PSH project who exited the project")
      ) |> 
        DT::datatable(escape = FALSE)
    })
  ),
  #### Youth Health Insurance Measure
  health_insurance_youth = list(
    expr = rlang::expr({
      qpr_benefits() |>
        HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectName == input$region)
    }),
    infobox = rlang::expr({
      .data <- dplyr::left_join(
        # all_hhs
        data_env() |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        data_env() |> 
          dplyr::filter(InsuranceFromAnySource == 1) |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
        dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
      
      if (nrow(.data) > 0) {
        .args <- list(.data = .data,
                      title = "Total Households Exiting With Health Insurance",
                      color = "gray-dark",
                      icon = "medkit",
                      value = scales::percent(.data$Percent),
                      subtitle = paste(.data$InsuranceAtExit, 
                                       "out of",
                                       .data$TotalHHs,
                                       "households")
        )
      } else {
        .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
      }
      
      do.call(qpr_infobox, .args)
    }),
    details = rlang::expr({
      tibble::tibble(
        ProjectType = c("Emergency Shelter", "Transitional Housing", "Rapid Re-housing", "Permanent Supportive Housing"),
        Goal = c("At least 75% of households in Youth ES projects will receive at least one source of health insurance at program exit",
                 "At least 85% of households in Youth TH projects will receive at least one source of health insurance at program exit",
                 "At least 85% of households in Youth RRH projects will receive at least one source of health insurance at program exit",
                 "At least 85% of households in Youth PSH projects will receive at least one source of health insurance at program exit"),
        HowCalculated = c("Number of households who exited with 1 or more sources of health insurance / number of households who exited the project",
                          "Number of households who exited with 1 or more sources of health insurance / number of households who exited the project",
                          "Number of households who exited with 1 or more sources of health insurance / number of households who exited RRH",
                          "Number of households who exited with 1 or more sources of health insurance / number of households that entered a PSH project who exited the project")
      ) |> 
        DT::datatable(escape = FALSE)
    })
  ),
  #### Length of Stay
  length_of_stay = list(
    expr = rlang::expr({
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
    }),
    infobox = rlang::expr({
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
    }),
    details = rlang::expr({
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
  ),
  #### Youth Length of Stay
  length_of_stay_youth = list(
    expr = rlang::expr({
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
    }),
    infobox = rlang::expr({
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
    }),
    details = rlang::expr({
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
  )
)