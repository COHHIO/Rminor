qpr_dependencies$LoS <- c(
  "goals",
  "qpr_leavers"
)
qpr_expr$LoS <- list()
qpr_expr$LoS$expr <- rlang::expr({
  ReportStart <- Report()$Start
  ReportEnd <- Report()$End
  
  LoSGoals <- goals() %>%
    dplyr::select(-Measure) %>%
    dplyr::filter(SummaryMeasure == "Length of Stay" &
                    ProjectType %in% unlist(ProjectType())) %>%
    unique()
  
  LoSDetail <- qpr_leavers() %>%
    dplyr::filter(((
      !is.na(MoveInDateAdjust) &
        ProjectType == 13
    ) |
      (
        ProjectType %in% c(1, 2, 8) &
          !is.na(ExitDate)
      )) &
      HMIS::exited_between(., ReportStart, ReportEnd)) %>%
    dplyr::filter(
      ProjectRegion %in% c(input$Region) &
        ProjectType %in% unlist(ProjectType())
    ) # this filter needs
  # to be here so the selection text matches the mutated data
  TotalLeavers <- LoSDetail %>%
    dplyr::group_by(FriendlyProjectName) %>%
    dplyr::summarise(Leavers = dplyr::n())
  
  title <-
    paste0(
      "Length of Stay (",
      input$radio_mean,
      ")\n",
      names(ProjectType()),
      "\n",
      Report()$Start,
      " to ",
      Report()$End
    )
  
  LoSSummary <- LoSDetail %>%
    dplyr::group_by(FriendlyProjectName,
                    ProjectRegion,
                    ProjectCounty,
                    ProjectType) %>%
    dplyr::summarise(
      Days = dplyr::case_when(
        input$radio_mean == "Average Days" ~
          as.numeric(mean(DaysinProject)),
        input$radio_mean == "Median Days" ~
          as.numeric(stats::median(DaysinProject))
      )
    ) %>%
    dplyr::left_join(LoSGoals, by = "ProjectType") %>%
    dplyr::left_join(TotalLeavers, by = ("FriendlyProjectName")) %>%
    dplyr::mutate(
      hover = paste0(
        FriendlyProjectName,
        "\nTotal Leavers: ",
        Leavers,
        "\nDays: ",
        Days,
        sep = "\n"
      )
    )
  attr(LoSSummary, "title") <- title
  LoSSummary
})
qpr_expr$LoS$plot <- rlang::expr({
  qpr_plotly(
    data_env(),
    y = ~ Days,
    title = attr(data_env(), "title"),
    xaxis = list(
      title = "",
      rangemode = "tozero",
      showgrid = TRUE
    ),
    yaxis = list(
      title = "Days",
      rangemode = "tozero",
      showgrid = TRUE
    )
  )
})
