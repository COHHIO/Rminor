
qpr_expr$Income <- list()

qpr_expr$Income$expr <- rlang::expr({
  ReportStart <- Report()$Start
  ReportEnd <- Report()$End
   
  meeting_objective <- qpr_income %>%
    dplyr::filter(
      ProjectRegion %in% input$QPRIncomeRegionSelect &
        ProjectType == input$radioQPR_Income_PTC &
        HMIS::stayed_between(., ReportStart, ReportEnd) &
        Difference > 0
    ) %>%
    dplyr::group_by(FriendlyProjectName,
                    ProjectType,
                    ProjectCounty,
                    ProjectRegion) %>%
    dplyr::summarise(Increased = dplyr::n())
  
  # calculating the total households for comparison
  all_hhs <- qpr_income %>%
    dplyr::filter(ProjectRegion %in% input$QPRIncomeRegionSelect &
                    ProjectType == input$radioQPR_Income_PTC &
                    HMIS::stayed_between(., ReportStart, ReportEnd)) %>%
    dplyr::group_by(FriendlyProjectName,
                    ProjectType,
                    ProjectCounty,
                    ProjectRegion) %>%
    dplyr::summarise(TotalHHs = dplyr::n())
  
  IncreasedIncome <- all_hhs %>%
    dplyr::left_join(
      meeting_objective,
      by = c("FriendlyProjectName",
             "ProjectType",
             "ProjectCounty",
             "ProjectRegion")
    )
  
  IncreasedIncome[is.na(IncreasedIncome)] <- 0
  
  IncreasedIncome <- IncreasedIncome %>%
    dplyr::mutate(Percent = Increased / TotalHHs)
  
  IncomeGoal <-
    goals %>%
    dplyr::filter(Measure == "Gain or Increase Income") %>%
    dplyr::mutate(ProjectType = dplyr::case_when(
      ProjectType == 1 ~ "Emergency Shelters",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType == 3 ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 9 ~ "Permanent Supportive Housing",
      ProjectType == 12 ~ "Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    )) %>% unique()
  
  title <- paste0("Increased Income\n",
                  input$radioQPR_Income_PTC, "\n",
                  Report()$Start, " to ", Report()$End)
  
  region <- c(input$QPRIncomeRegionSelect)
  
  stagingIncome <- IncreasedIncome %>%
    dplyr::left_join(IncomeGoal, by = "ProjectType") %>%
    dplyr::filter(ProjectType == input$radioQPR_Income_PTC &
                    ProjectRegion %in% region) %>%
    dplyr::mutate(
      hover = paste0(
        FriendlyProjectName,
        "\nIncreased Income: ", Increased,
        "\nTotal Households: ", TotalHHs,
        "\n", as.integer(Percent * 100), "%",
        sep = "\n"
      )
    )
  attr(stagingIncome, "title") <- title
  stagingIncome
})

qpr_expr$Income$plot <- rlang::expr({
  qpr_plotly(
    data_env(),
    title = attr(data_env(), "title"),
    y = ~ Percent,
    text = ~ hover,
    hoverinfo = 'text',
    type = "bar"
  )
})
