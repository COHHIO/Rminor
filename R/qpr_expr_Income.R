qpr_dependencies$Income <- c(
  "qpr_income"
)
qpr_expr$Income <- list()

qpr_expr$Income$expr <- rlang::expr({
  .PT_nm <- names(ProjectType()) 
  
  ReportStart <- Report()$Start
  ReportEnd <- Report()$End
   
  meeting_objective <- qpr_income() %>%
    dplyr::filter(
      ProjectRegion %in% c(input$Region) &
        ProjectType == .PT_nm &
        HMIS::stayed_between(., ReportStart, ReportEnd) &
        Difference > 0
    ) %>%
    dplyr::group_by(FriendlyProjectName,
                    ProjectType,
                    ProjectCounty,
                    ProjectRegion) %>%
    dplyr::summarise(Increased = dplyr::n())
  
  # calculating the total households for comparison
  all_hhs <- qpr_income() %>%
    dplyr::filter(ProjectRegion %in% c(input$Region) &
                    ProjectType == .PT_nm &
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
    goals() %>%
    dplyr::filter(Measure == "Gain or Increase Income" & 
                    ProjectType %in% unlist(ProjectType())) %>%
    dplyr::distinct(Goal)
  IncomeGoal[["ProjectType"]] <- .PT_nm
  
  title <- paste0("Increased Income\n",
                  .PT_nm, "\n",
                  Report()$Start, " to ", Report()$End)
  
  stagingIncome <- IncreasedIncome %>%
    dplyr::left_join(IncomeGoal, by = "ProjectType") %>%
    dplyr::filter(ProjectType == .PT_nm &
                    ProjectRegion %in% c(input$Region)) %>%
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
    title = attr(data_env(), "title")
  )
})
