qpr_dependencies$NCB <- c(
  "qpr_benefits"
)
qpr_expr$NCB <- list()
qpr_expr$NCB$expr <- rlang::expr({
  .PT_nm <- names(ProjectType()) 
  meeting_objective <- qpr_benefits() %>%
    dplyr::filter(
      ProjectRegion %in% c(input$Region) &
        ProjectType %in% .PT_nm &
        HMIS::exited_between(., ReportStart, ReportEnd) &
        BenefitsFromAnySource == 1
    ) %>% 
    dplyr::group_by(FriendlyProjectName, 
                    ProjectType, 
                    ProjectCounty, 
                    ProjectRegion) %>%
    dplyr::summarise(BenefitsAtExit = dplyr::n())
  
  # calculating the total households for comparison
  all_hhs <- qpr_benefits() %>%
    dplyr::filter(ProjectRegion %in% c(input$Region) &
                    ProjectType == names(ProjectType()) &
                    HMIS::exited_between(., ReportStart, ReportEnd)) %>%
    dplyr::group_by(FriendlyProjectName, 
                    ProjectType, 
                    ProjectCounty, 
                    ProjectRegion) %>%
    dplyr::summarise(TotalHHs = dplyr::n()) 
  
  NCBsAtExit <- all_hhs %>%
    dplyr::left_join(
      meeting_objective,
      by = c("FriendlyProjectName", 
             "ProjectType", 
             "ProjectCounty", 
             "ProjectRegion")
    )
  
  NCBsAtExit[is.na(NCBsAtExit)] <- 0
  
  NCBsAtExit <- NCBsAtExit %>%
    dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
  
  NCBGoal <-
    goals() %>%
    dplyr::filter(Measure == "Non-cash Benefits" & 
                    ProjectType %in% unlist(ProjectType())) %>% 
    # TODO This won't be necessary once qpr_benefits also uses numeric
    dplyr::distinct(Goal)
  NCBGoal[["ProjectType"]] <- .PT_nm
  
  title <- paste0("Non-Cash Benefits at Exit\n", 
                  .PT_nm, "\n",
                  Report()$Start, " to ", Report()$End)
  
  stagingNCBs <- NCBsAtExit %>%
    dplyr::left_join(NCBGoal, by = "ProjectType") %>%
    dplyr::filter(ProjectType %in% .PT_nm & 
                    ProjectRegion %in% input$Region) %>%
    dplyr::mutate(
      hover = paste0(
        FriendlyProjectName, 
        "\nReceiving Non-Cash Benefits at Exit: ", BenefitsAtExit, 
        "\nTotal Households: ", TotalHHs, 
        "\n", as.integer(Percent * 100), "%",
        sep = "\n"
      )
    )
  attr(stagingNCBs, "title") <- title
  stagingNCBs
})

qpr_expr$NCB$plot <- rlang::expr({
  qpr_plotly(
    data_env(),
    title = attr(data_env(), "title")
  )
})
