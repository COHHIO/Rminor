qpr_dependencies$TPH <- c(
  "qpr_leavers",
  "goals"
)

qpr_expr$temp_permanent_housing <- list()

qpr_expr$temp_permanent_housing$expr <- rlang::expr({
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
  
})

qpr_expr$temp_permanent_housing$infobox <- rlang::expr({
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
  
})

qpr_expr$temp_permanent_housing$datatable <- 
  rlang::expr({
    req(data_env())
    dplyr::left_join(
      data_env()$TotalHHsSuccessfulPlacement,
      data_env()$SuccessfullyPlaced,
      by = c(
        "EnrollmentID",
        "ProjectType",
        "ProjectName",
        "PersonalID",
        "UniqueID",
        "EntryDate",
        "MoveInDate",
        "MoveInDateAdjust",
        "ExitDate",
        "DestinationGroup",
        "Destination",
        "HouseholdID"
      )
    ) |>
      dplyr::mutate(BedStart = dplyr::if_else(ProjectType %in% c(3, 9, 13),
                                              MoveInDate, EntryDate)) |>
      dplyr::arrange(DestinationGroup, PersonalID) |>
      dplyr::select(
        UniqueID,
        EntryDate,
        "Bed Start" = BedStart,
        ExitDate,
        "Destination Group" =  DestinationGroup
      ) |> 
      datatable_default(escape = FALSE)
  })

qpr_expr$temp_permanent_housing$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Street Outreach"),
    Goal = c("At least 60% of households in Outreach projects will move from unsheltered locations to temporary or permanent housing at exit"),
    HowCalculated = c("Number of households who moved from unsheltered locations to temporary (ES or TH) or to PH upon exit / number of households who moved from unsheltered locations to any destination at exit")
  ) |> 
    DT::datatable(escape = FALSE)
})
