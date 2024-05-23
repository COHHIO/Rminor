qpr_dependencies$reentries <- c(
  "qpr_reentries",
  "goals"
)

qpr_expr$reentries <- list()
qpr_expr$reentries$expr <- rlang::expr({
  ExitsFromHP <- qpr_reentries() |>
    dplyr::filter(LatestPermanentProject12 >= (input$date_range[1] - lubridate::years(1)) &
                    LatestPermanentProject12 <= input$date_range[2]) |> 
    dplyr::filter(ExitingHP %in% input$region) |> 
    dplyr::group_by(UniqueID) |>
    dplyr::mutate(min_entry_date = min(EntryDate, na.rm = TRUE)) |> 
    dplyr::filter(EntryDate == min_entry_date | is.na(EntryDate)) |> 
    dplyr::ungroup() |> 
    dplyr::select(-min_entry_date)
  Reentries <- qpr_reentries() |> 
    HMIS::entered_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ExitingHP %in% input$region) |> 
    dplyr::group_by(UniqueID) |> 
    dplyr::filter(EntryDate == min(EntryDate)) |> 
    dplyr::ungroup()
  
  list(ExitsFromHP = ExitsFromHP,
       Reentries = Reentries)
})

qpr_expr$reentries$infobox <- rlang::expr({
  req(data_env())
  qpr_infobox(
    data_env(),
    title = "Number of Re-entries",
    color = "info",
    value = scales::percent(nrow(.data$Reentries) / nrow(.data$ExitsFromHP)),
    icon = shiny::icon("key"),
    subtitle = paste(
      nrow(.data$Reentries),
      "/",
      nrow(.data$ExitsFromHP),
      "households"
    )
  )
})

# qpr_expr$reentries$datatable <- rlang::expr({
#   data_env()$Reentries |>
#     # dplyr::arrange(dplyr::desc(DaysToHouse)) |>
#     # dplyr::select(
#     #   UniqueID,
#     #   EntryDate,
#     #   "Move In Date" = MoveInDate,
#     #   "Days to House" = DaysToHouse
#     # ) |> 
#     datatable_default(escape = FALSE)
# })


qpr_expr$reentries$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Rapid Re-housing"),
    Goal = c("RRH projects will place households into permanent housing within 21 days of project entry"),
    HowCalculated = c("Average number of days between leavers' RRH entry date and Housing Move-in Date")
    ) |> 
    DT::datatable(escape = FALSE)
})
