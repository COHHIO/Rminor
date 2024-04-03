qpr_dependencies$RRH <- c(
  "qpr_rrh_enterers",
  "goals"
)

qpr_expr$rrh_placement <- list()
qpr_expr$rrh_placement$expr <- rlang::expr({
  qpr_rrh_enterers() |>
    HMIS::entered_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(!is.na(MoveInDateAdjust) & ProjectName %in% input$region) 
})

qpr_expr$rrh_placement$infobox <- rlang::expr({
  data_env() |>
    dplyr::mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) |>
    dplyr::summarise(AvgDaysToHouse = round(mean(DaysToHouse), 0), .groups = "drop_last") |> 
    qpr_infobox(
      title = "Average Days to House",
      color = "indigo",
      icon = "hourglass-half",
      value = .data$AvgDaysToHouse,
    )
})

qpr_expr$rrh_placement$datatable <- rlang::expr({
  data_env() |>
    dplyr::arrange(dplyr::desc(DaysToHouse)) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      "Move In Date" = MoveInDate,
      "Days to House" = DaysToHouse
    ) |> 
    datatable_default(escape = FALSE)
})

# qpr_expr$RRH$expr <- rlang::expr({
#   ReportStart <- Report()$Start
#   ReportEnd <- Report()$End
#   daysToHouse <- qpr_rrh_enterers() %>%
#     dplyr::filter(
#       !is.na(MoveInDateAdjust) &
#         ProjectRegion %in% c(input$Region) &
#         HMIS::entered_between(., ReportStart, ReportEnd)
#     )
#   
#   RRHgoal <- goals() %>%
#     dplyr::filter(SummaryMeasure == "Rapid Placement") %>%
#     dplyr::select(ProjectType, Goal)
#   
#   summaryDays <- daysToHouse %>%
#     dplyr::group_by(FriendlyProjectName,
#                     ProjectCounty,
#                     ProjectRegion,
#                     ProjectType) %>%
#     dplyr::summarise(AvgDays = as.integer(mean(DaysToHouse)),
#                      TotalHHs = dplyr::n()) %>%
#     dplyr::left_join(RRHgoal, by = "ProjectType") %>%
#     dplyr::mutate(hover = paste0(
#       FriendlyProjectName,
#       "\nAverage Days to House: ",
#       AvgDays,
#       "\nTotal Households: ",
#       TotalHHs,
#       sep = "\n"
#     ))
#   
#   title <- paste0("Average Days to House\nRapid Rehousing\n",
#                   Report()$Start, " to ", Report()$End)
#   attr(summaryDays, "title") <- title
#   summaryDays
# })
# 
# qpr_expr$RRH$plot <- rlang::expr({
#   qpr_plotly(
#     data_env(),
#     title = attr(data_env(), "title"),
#     y = ~ AvgDays,
#     xaxis = list(title = ~ FriendlyProjectName),
#     yaxis = list(title = "Average Days to House"),
#     shapes = list(
#       y0 = ~ Goal[1],
#       y1 = 0
#     )  
#   )
#   
# })