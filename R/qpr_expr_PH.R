qpr_dependencies$PH <- c(
  "qpr_leavers",
  "goals"
)

qpr_expr$PH <- list()
qpr_expr$PH$expr <- rlang::expr({
  ReportStart <- Report()$Start
  ReportEnd <- Report()$End
  # hhs that achieved the goal

  SuccessfullyPlaced <- qpr_leavers() %>%
  dplyr::filter(((
    ProjectType %in% c(3, 9, 13) &
      !is.na(MoveInDateAdjust)
  ) |
    ProjectType %in% c(1, 2, 4, 8, 12)) &
    # excluding non-mover-inners
    (((DestinationGroup == "Permanent" |
         #exited to ph or still in PSH/HP
         is.na(ExitDate)) &
        ProjectType %in% c(3, 9, 12) &
        HMIS::served_between(., ReportStart, ReportEnd)# PSH & HP
    ) |
      (
        DestinationGroup == "Permanent" & # exited to ph
          ProjectType %in% c(1, 2, 4, 8, 13) &
          HMIS::exited_between(., ReportStart, ReportEnd)
      )
    )) %>% # ES, TH, SH, RRH, OUT) %>%
  dplyr::group_by(FriendlyProjectName, 
                  ProjectType, 
                  ProjectCounty, 
                  ProjectRegion) %>%
  dplyr::summarise(SuccessfullyPlacedHHs = dplyr::n())

# calculating the total households to compare successful placements to
TotalHHsSuccessfulPlacement <- qpr_leavers() %>%
  dplyr::filter((
    HMIS::served_between(., ReportStart, ReportEnd) &
      ProjectType %in% c(3, 9, 12) # PSH & HP
  ) |
    (
      HMIS::exited_between(., ReportStart, ReportEnd) &
        ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
    )) %>%
  dplyr::group_by(FriendlyProjectName, 
                  ProjectType, 
                  ProjectCounty, 
                  ProjectRegion) %>%
  dplyr::summarise(TotalHHs = dplyr::n()) # For PSH & HP, it's total hhs served;
# otherwise, it's total hhs *exited* during the reporting period

SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
  dplyr::left_join(
    SuccessfullyPlaced,
    by = c("FriendlyProjectName", 
           "ProjectType", 
           "ProjectCounty", 
           "ProjectRegion")
  ) %>%
  dplyr::mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

PlacementGoal <-
  goals() %>%
  dplyr::filter(
    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
      Measure != "Exits to Temporary or Permanent Housing"
  )


stagingExitsToPH <- SuccessfulPlacement %>%
  dplyr::left_join(PlacementGoal, by = "ProjectType") %>%
  dplyr::filter(ProjectType %in% unlist(ProjectType()),
                ProjectRegion %in% input$Region) %>%
  dplyr::mutate(
    hover = paste0(
      FriendlyProjectName, 
      "\nExited to PH: ", SuccessfullyPlacedHHs, 
      "\nTotal Households: ", TotalHHs, 
      "\n", as.integer(Percent * 100), "%",
      sep = "\n"
    )
  )

  out <- list(stagingExitsToPH = stagingExitsToPH)

if (names(ProjectType()) == "Street Outreach") {
    
    totalServed <- qpr_leavers() %>%
      dplyr::filter(HMIS::exited_between(., ReportStart, ReportEnd) &
                      ProjectType == 4) %>%
      dplyr::group_by(FriendlyProjectName,
                      ProjectType,
                      ProjectCounty,
                      ProjectRegion) %>%
      dplyr::summarise(TotalHHs = dplyr::n())
    
    notUnsheltered <- qpr_leavers() %>%
      dplyr::filter(
        ProjectType == 4 &
          Destination != 16 &
          DestinationGroup %in% c("Temporary", "Permanent") &
          HMIS::exited_between(., ReportStart, ReportEnd)
      ) %>%
      dplyr::group_by(FriendlyProjectName,
                      ProjectType,
                      ProjectCounty,
                      ProjectRegion) %>%
      dplyr::summarise(NotUnsheltered = dplyr::n())
    
    goalOutreach <- goals() %>%
      dplyr::filter(Measure == "Exits to Temporary or Permanent Housing") %>%
      dplyr::select(Goal, ProjectType)
    
    notUnsheltered <- notUnsheltered %>%
      dplyr::left_join(goalOutreach, by = "ProjectType") %>%
      dplyr::left_join(
        totalServed,
        by = c(
          "FriendlyProjectName",
          "ProjectType",
          "ProjectCounty",
          "ProjectRegion"
        )
      )  %>%
      dplyr::filter(ProjectRegion %in% input$Region) %>% 
      mutate(
        Percent = NotUnsheltered / TotalHHs,
        hover = paste0(
          FriendlyProjectName,
          "\nExited to Temp or PH: ",
          NotUnsheltered,
          "\nTotal Households: ",
          TotalHHs,
          "\n",
          as.integer(Percent * 100),
          "%",
          sep = "\n"
        )
      )
    
    #IDEA What if we rendered a single graph with a dodged bar format for temporary vs permanent
    # out <- dplyr::bind_rows(list(Permanent = stagingExitsToPH, Temporary = notUnsheltered %>% dplyr::ungroup() %>%  dplyr::rename("SuccessfullyPlacedHHs" = NotUnsheltered), Total = notUnsheltered %>% dplyr::ungroup() %>%  dplyr::rename("SuccessfullyPlacedHHs" = NotUnsheltered)), .id = "type")
    # out[out$type %in% "Temporary", "SuccessfullyPlacedHHs"] <- out[out$type %in% "Temporary", "SuccessfullyPlacedHHs"] - out[out$type %in% "Permanent", "SuccessfullyPlacedHHs"]
    # out <- dplyr::mutate(out,
    #   Percent = SuccessfullyPlacedHHs / TotalHHs,
    #   hover = paste0(
    #     FriendlyProjectName,
    #     "\nExited to Temp or PH: ",
    #     SuccessfullyPlacedHHs,
    #     "\nTotal Households: ",
    #     TotalHHs,
    #     "\n",
    #     as.integer(Percent * 100),
    #     "%",
    #     sep = "\n"
    #   )
    # )
    # .g <- ggplot2::ggplot(dplyr::ungroup(out), ggplot2::aes(y = Percent, x = FriendlyProjectName, fill = type)) +
    #   #ggplot2::geom_bar(stat = "identity", position = "dodge")+
    #   ggplot2::geom_area(data = type_goals, inherit.aes = FALSE, ggplot2::aes(y = Goal, x = FriendlyProjectName), fill = "black", position = "stack") +
    #   ggplot2::scale_fill_manual(values = c(Total = "#1f77b4", Temporary = "#59a6db", Permanent = "#5a93bb"))+
    #   ggplot2::scale_y_continuous(breaks = c(0,.5,1), labels = rlang::as_function(~{paste0(.x * 100,"%")}), limits = c(0,1))+
    #   ggplot2::xlab("") +
    #   ggplot2::ylab("") +
    #   ggplot2::ggtitle("") +
    #   ggplot2::theme_minimal()+
    #   ggplot2::theme(
    #     plot.title = element_text(hjust = .5),
    #     plot.subtitle = element_text(hjust = .5),
    #     axis.text.x = element_text(angle = -30, hjust = 0),
    #     plot.background = element_rect(color = "#cbe5cb"))
    out$notUnsheltered <- notUnsheltered
  }
  out
})





qpr_expr$PH$plot <- rlang::expr({
  # title changes if you pick PSH since it's looking at Stayers as well
  .PT <- unlist(ProjectType())
  .PT_nm <- names(ProjectType())
  title <-
    paste0(
      dplyr::if_else(
        .PT_nm ==
          "Permanent Supportive Housing",
        "Remained in or Exited to ",
        "Exited to "
      ),
      dplyr::if_else(.PT_nm == "Street Outreach", "Temporary or Permanent Housing\nStreet Outreach\n", "Permanent Housing\n"),
      Report()$Start,
      " to ",
      Report()$End
    )
  yAxisTitle <- dplyr::if_else(
    .PT_nm ==
      "Permanent Supportive Housing",
    "Remained in or Exited to PH",
    paste0("Exited to ", dplyr::if_else(.PT_nm == "Street Outreach", "Temp or Permanent", "Permanent")," Housing")
  )
  # Default plotly layout options
  .p_opts <- list(
                  xaxis = list(title = ""),
                  yaxis = list(title = yAxisTitle,
                               tickformat = "%"),
                  title = list(
                    text = title,
                    font = list(
                      size = 15
                    )),
                  margin = list(
                    l = 50,
                    r = 50,
                    b = 100,
                    t = 100,
                    pad = 4
                  )
  )
  .p <- plotly::plot_ly(
    data_env()[[1]],
    x = ~ FriendlyProjectName,
    y = ~ Percent,
    text = ~ hover,
    hoverinfo = 'text',
    type = "bar",
    name = "Permanent",
    marker = list(color = "#1f77b4", line = list(color = "white", width = .5))
  ) 
  if (.PT_nm == "Street Outreach") {
    .p <- .p %>%
      plotly::add_trace(data = data_env()[[2]], name = "Temp or\n Permanent",
                        marker = list(color = "#59a6db", line = list(color = "white", width = .5)))
    shapes <- list(list(
      type = "rect",
      name = "CoC Goal",
      fillcolor = "#1f77b4",
      line = list(color = "white", width = .02),
      layer = "below",
      xref = "paper",
      yref = "y",
      x0 = 0,
      x1 = 1,
      y1 = dplyr::if_else(.PT == 4, .6, 1),
      y0 = data_env()$stagingExitsToPH$Goal[1],
      opacity = .6
    ),
    list(
      type = "rect",
      name = "CoC Goal",
      fillcolor = "#59a6db",
      line = list(color = "white", width = .02),
      layer = "below",
      xref = "paper",
      yref = "y",
      x0 = 0,
      x1 = 1,
      y1 = 1,
      y0 = data_env()$notUnsheltered$Goal[1],
      opacity = .6
    ))
    .p_opts$barmode <- 'group'  
  } else {
    shapes <- list(
      type = "rect",
      name = "CoC Goal",
      fillcolor = "#008000",
      line = list(color = "white", width = .01),
      layer = "below",
      xref = "paper",
      yref = "y",
      x0 = 0,
      x1 = 1,
      y0 = ~ Goal[1],
      y1 = 1,
      opacity = .2
    )
  }
  .p_opts$p <- .p
  .p_opts$shapes <- shapes
  do.call(plotly::layout, .p_opts)
})
