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
  ),
  #### Income Growth
  income_growth = list(
    expr = rlang::expr({
      qpr_income() |>
        HMIS::stayed_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectName == input$region)
    }),
    infobox = rlang::expr({
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
    }),
    details = rlang::expr({
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
  ),
  #### Income Growth Youth
  income_growth_youth = list(
    expr = rlang::expr({
      qpr_income() |>
        HMIS::stayed_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectName == input$region)
    }),
    infobox = rlang::expr({
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
    }),
    details = rlang::expr({
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
  ),
  #### Non-cash Benefits
  noncash_benefits = list(
    expr = rlang::expr({
      req(input$date_range, input$region)
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
    }),
    details = rlang::expr({
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
  ),
  #### Non-cash benefits Youth
  noncash_benefits_youth = list(
    expr = rlang::expr({
      req(input$date_range, input$region)
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
    }),
    details = rlang::expr({
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
  ),
  #### Permanent Housing
  permanent_housing = list(
    expr = rlang::expr({
      req(input$date_range, input$region)
      
      .by_region <- qpr_leavers() |>
        dplyr::filter(ProjectName == input$region)
      .exited <- .by_region |> 
        HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      .served <- .by_region |> 
        HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      
      
      .psh_hp <- .by_region$ProjectType %in% c(3, 9, 12)
      .es_th_sh_out_rrh <- .by_region$ProjectType %in% c(0, 1, 2, 4, 8, 13)
      
      SuccessfullyPlaced <- dplyr::filter(.by_region,
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
        dplyr::filter(.by_region,
                      (.served & .psh_hp) # PSH & HP
                      |
                        (.exited & .es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
        )
      list(SuccessfullyPlaced = SuccessfullyPlaced,
           TotalHHsSuccessfulPlacement = TotalHHsSuccessfulPlacement)
      
    }),
    infobox = rlang::expr({
      req(data_env())
      
      qpr_infobox(
        data_env(),
        title = "Successfully Placed",
        color = "info",
        value = scales::percent(nrow(.data$SuccessfullyPlaced) / nrow(.data$TotalHHsSuccessfulPlacement)),
        icon = shiny::icon("key"),
        subtitle = paste(
          nrow(.data$SuccessfullyPlaced),
          "/",
          nrow(.data$TotalHHsSuccessfulPlacement),
          "households"
        )
      )
    }),
    details = rlang::expr({
      tibble::tibble(
        ProjectType = c("Street Outreach", "Emergency Shelter", "Transitional Housing", "Rapid Re-housing"),
        Goal = c("At least 30% of households in Outreach projects will move into permanent housing at exit",
                 "At least 40% of households in ES projects will move into permanent housing at exit",
                 "At least 83% of households in TH projects will move into permanent housing at exit",
                 "At least 83% of households in RRH projects will move into permanent housing at exit"),
        HowCalculated = c("Number of households who moved to PH upon exit / number of participants who exited project",
                          "Number of households who moved to PH upon exit / number of participants who exited ES project",
                          "Number of households who moved to PH upon exit / number of participants who exited TH project",
                          "Number of households who moved to PH upon exit / number of participants who exited RRH project")
      ) |> 
        DT::datatable(escape = FALSE)
    })
  ),
  #### Youth Permanent Housing
  permanent_housing_youth = list(
    expr = rlang::expr({
      req(input$date_range, input$region)
      
      .by_region <- qpr_leavers() |>
        dplyr::filter(ProjectName == input$region)
      .exited <- .by_region |> 
        HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      .served <- .by_region |> 
        HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      
      
      .psh_hp <- .by_region$ProjectType %in% c(3, 9, 12)
      .es_th_sh_out_rrh <- .by_region$ProjectType %in% c(0, 1, 2, 4, 8, 13)
      
      SuccessfullyPlaced <- dplyr::filter(.by_region,
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
        dplyr::filter(.by_region,
                      (.served & .psh_hp) # PSH & HP
                      |
                        (.exited & .es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
        )
      list(SuccessfullyPlaced = SuccessfullyPlaced,
           TotalHHsSuccessfulPlacement = TotalHHsSuccessfulPlacement)
      
    }),
    infobox = rlang::expr({
      req(data_env())
      
      qpr_infobox(
        data_env(),
        title = "Successfully Placed",
        color = "info",
        value = scales::percent(nrow(.data$SuccessfullyPlaced) / nrow(.data$TotalHHsSuccessfulPlacement)),
        icon = shiny::icon("key"),
        subtitle = paste(
          nrow(.data$SuccessfullyPlaced),
          "/",
          nrow(.data$TotalHHsSuccessfulPlacement),
          "households"
        )
      )
    }),
    details = rlang::expr({
      tibble::tibble(
        ProjectType = c("Street Outreach", "Emergency Shelter", "Transitional Housing", "Rapid Re-housing"),
        Goal = c("At least 30% of households in Youth Outreach projects will move into permanent housing at exit",
                 "At least 25% of households in Youth ES projects will move into permanent housing at exit",
                 "At least 50% of households in Youth TH projects will move into permanent housing at exit",
                 "At least 83% of households in Youth RRH projects will move into permanent housing at exit"),
        HowCalculated = c("Number of households who moved to PH upon exit / number of participants who exited project",
                          "Number of households who moved to PH upon exit / number of participants who exited ES project",
                          "Number of households who moved to PH upon exit / number of participants who exited TH project",
                          "Number of households who moved to PH upon exit / number of participants who exited RRH project")
      ) |> 
        DT::datatable(escape = FALSE)
    })
  ),
  #### Temp Permanent Housing
  temp_permanent_housing = list(
    expr = rlang::expr({
      req(input$date_range, input$region)
      
      .by_region <- qpr_leavers() |> 
        dplyr::filter(ProjectName == input$region)
      .exited <- .by_region |> 
        HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      .served <- .by_region |> 
        HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      
      .so <- .by_region$ProjectType %in% c(4)
      
      SuccessfullyPlaced <- dplyr::filter(.by_region,
                                          ((DestinationGroup == "Permanent" | DestinationGroup == "Temporary") &
                                             .so & .exited))
      
      # calculating the total households to compare successful placements to
      TotalHHsSuccessfulPlacement <- 
        dplyr::filter(.by_region,
                      (.exited & .so)
        )
      list(SuccessfullyPlaced = SuccessfullyPlaced,
           TotalHHsSuccessfulPlacement = TotalHHsSuccessfulPlacement)
      
    }),
    infobox = rlang::expr({
      req(data_env())
      
      qpr_infobox(
        data_env(),
        title = "Successfully Placed",
        color = "info",
        value = scales::percent(nrow(.data$SuccessfullyPlaced) / nrow(.data$TotalHHsSuccessfulPlacement)),
        icon = shiny::icon("key"),
        subtitle = paste(
          nrow(.data$SuccessfullyPlaced),
          "/",
          nrow(.data$TotalHHsSuccessfulPlacement),
          "households"
        )
      )
    }),
    details = rlang::expr({
      tibble::tibble(
        ProjectType = c("Street Outreach"),
        Goal = c("At least 60% of households in Outreach projects will move from unsheltered locations to temporary or permanent housing at exit"),
        HowCalculated = c("Number of households who moved from unsheltered locations to temporary (ES or TH) or to PH upon exit / number of households who moved from unsheltered locations to any destination at exit")
      ) |>
        DT::datatable(escape = FALSE)
    })
  ),
  #### Temp or Permanent Housing Youth
  temp_permanent_housing_youth = list(
    expr = rlang::expr({
      req(input$date_range, input$region)
      
      .by_region <- qpr_leavers() |> 
        dplyr::filter(ProjectName == input$region)
      .exited <- .by_region |> 
        HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      .served <- .by_region |> 
        HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      
      .so <- .by_region$ProjectType %in% c(4)
      
      SuccessfullyPlaced <- dplyr::filter(.by_region,
                                          ((DestinationGroup == "Permanent" | DestinationGroup == "Temporary") &
                                             .so & .exited))
      
      # calculating the total households to compare successful placements to
      TotalHHsSuccessfulPlacement <- 
        dplyr::filter(.by_region,
                      (.exited & .so)
        )
      list(SuccessfullyPlaced = SuccessfullyPlaced,
           TotalHHsSuccessfulPlacement = TotalHHsSuccessfulPlacement)
      
    }),
    infobox = rlang::expr({
      req(data_env())
      
      qpr_infobox(
        data_env(),
        title = "Successfully Placed",
        color = "info",
        value = scales::percent(nrow(.data$SuccessfullyPlaced) / nrow(.data$TotalHHsSuccessfulPlacement)),
        icon = shiny::icon("key"),
        subtitle = paste(
          nrow(.data$SuccessfullyPlaced),
          "/",
          nrow(.data$TotalHHsSuccessfulPlacement),
          "households"
        )
      )
    }),
    rlang::expr({
      tibble::tibble(
        ProjectType = c("Street Outreach"),
        Goal = c("At least XX% of households in Youth Outreach projects will move from unsheltered locations to temporary or permanent housing at exit"),
        HowCalculated = c("Number of households who moved from unsheltered locations to temporary (ES or TH) or to PH upon exit / number of households who moved from unsheltered locations to any destination at exit")
      ) |>
        DT::datatable(escape = FALSE)
    })
  )
)