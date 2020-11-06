
qpr_expr$HI <- list()

qpr_expr$HI$expr <- rlang::expr({
  ReportStart <- Report()$Start
  ReportEnd <- Report()$End
  .PT_nm <- names(ProjectType())
  meeting_objective <- qpr_benefits %>%
    dplyr::filter(
      ProjectRegion %in% input$Region &
        ProjectType == .PT_nm &
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
    dplyr::filter(ProjectRegion %in% c(input$Region) &
                    ProjectType == names(ProjectType()) &
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
    dplyr::filter(Measure == "Health Insurance at Exit" & 
                    ProjectType %in% unlist(ProjectType())) %>%
    dplyr::distinct(Goal)
  
  HIGoal[["ProjectType"]] <- .PT_nm
  
  title <- paste0("Health Insurance at Exit\n", 
                  .PT_nm, "\n",
                  Report()$Start, " to ", Report()$End)
  
  region <- c(input$Region)
  
  stagingHI <- HIAtExit %>%
    dplyr::left_join(HIGoal, by = "ProjectType") %>%
    dplyr::filter(ProjectType == .PT_nm & 
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
    title = attr(data_env(), "title")
  )
})
