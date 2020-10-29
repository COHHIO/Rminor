qpr_expr$HI <- list()
qpr_expr$HI$expr <- rlang::expr({
  ReportStart <- Report()$Start
  ReportEnd <- Report()$End
  
  meeting_objective <- qpr_benefits %>%
    dplyr::filter(
      ProjectRegion %in% input$QPRHIRegionSelect &
        ProjectType == input$radioQPR_HI_PTC &
        HMIS::exited_between(., ReportStart, ReportEnd) &
        InsuranceFromAnySource == 1
    ) %>% 
    dplyr::group_by(FriendlyProjectName,
                    ProjectType,
                    ProjectCounty,
                    ProjectRegion) %>%
    dplyr::summarise(InsuranceAtExit = dplyr::n())
  
  # calculating the total households for comparison
  all_hhs <- qpr_benefits %>%
    dplyr::filter(ProjectRegion %in% input$QPRHIRegionSelect &
                    ProjectType == input$radioQPR_HI_PTC &
                    HMIS::exited_between(., ReportStart, ReportEnd)) %>%
    dplyr::group_by(FriendlyProjectName,
                    ProjectType,
                    ProjectCounty,
                    ProjectRegion) %>%
    dplyr::summarise(TotalHHs = dplyr::n()) 
  
  HIAtExit <- all_hhs %>%
    dplyr::left_join(
      meeting_objective,
      by = c("FriendlyProjectName",
             "ProjectType",
             "ProjectCounty",
             "ProjectRegion")
    )
  
  HIAtExit[is.na(HIAtExit)] <- 0
  
  HIAtExit <- HIAtExit %>%
    dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
  
  HIGoal <-
    goals %>%
    dplyr::filter(Measure == "Health Insurance at Exit") %>%
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
  
  title <- paste0("Health Insurance at Exit\n", 
                  input$radioQPR_NCB_PTC, "\n",
                  Report()$Start, " to ", Report()$End)
  
  region <- c(input$QPRHIRegionSelect)
  
  stagingHI <- HIAtExit %>%
    dplyr::left_join(HIGoal, by = "ProjectType") %>%
    dplyr::filter(ProjectType == input$radioQPR_HI_PTC & 
                    ProjectRegion %in% region) %>%
    dplyr::mutate(
      hover = paste0(
        FriendlyProjectName, 
        "\nHealth Insurance at Exit: ", InsuranceAtExit, 
        "\nTotal Households: ", TotalHHs, 
        "\n", as.integer(Percent * 100), "%",
        sep = "\n"
      )
    )
  attr(stagingHI, "title") <- title
  stagingHI
})

qpr_expr$HI$plot <- rlang::expr({
  qpr_plotly(
    data_env(),
    title = attr(data_env(), "title"),
    y = ~ Percent,
    text = ~ hover,
    hoverinfo = 'text',
    type = "bar"
  )
})