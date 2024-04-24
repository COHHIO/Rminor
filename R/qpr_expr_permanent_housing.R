qpr_dependencies$PH <- c(
  "qpr_leavers",
  "goals"
)

qpr_expr$permanent_housing <- list()

qpr_expr$permanent_housing$expr <- rlang::expr({
  req(input$date_range, input$region)
  # input <- list(region = sample(qpr_leavers()$ProjectName, 1),
  #               date_range = c(start = lubridate::floor_date(lubridate::as_date(.qbegin - lubridate::dmonths(4)), "quarter"),
  #                               end = .qbegin))
  .by_region <- qpr_leavers() |>
    dplyr::filter(ProjectName == input$region)
  .exited <- .by_region |> 
    HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
  .served <- .by_region |> 
    HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)
  
  
  .psh_hp <- .by_region$ProjectType %in% c(3, 9, 12)
  .es_th_sh_out_rrh <- .by_region$ProjectType %in% c(0, 1, 2, 4, 8, 13)
  
  SuccessfullyPlaced <- dplyr::filter(.by_region,
                                      ((ProjectType %in% c(3, 9, 13) &
                                          !is.na(MoveInDateAdjust)) |
                                         ProjectType %in% c(0, 1, 2, 4, 8, 12)
                                      ) &
                                        # excluding non-mover-inners
                                        (((DestinationGroup == "Permanent" |
                                             #exited to ph or still in PSH/HP
                                             is.na(ExitDate)) &
                                            .psh_hp & # PSH & HP
                                            .served
                                        ) |
                                          (
                                            DestinationGroup == "Permanent" & # exited to ph
                                              .es_th_sh_out_rrh &
                                              .exited
                                          )
                                        ))
  
  # calculating the total households to compare successful placements to
  TotalHHsSuccessfulPlacement <- 
    dplyr::filter(.by_region,
                  (.served & .psh_hp) # PSH & HP
                  |
                    (.exited & .es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
    )
  list(SuccessfullyPlaced = SuccessfullyPlaced,
       TotalHHsSuccessfulPlacement = TotalHHsSuccessfulPlacement)
  
})

qpr_expr$permanent_housing$infobox <- rlang::expr({
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

qpr_expr$permanent_housing$datatable <- 
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

qpr_expr$permanent_housing$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c("Street Outreach", "Emergency Shelter", "Transitional Housing", "Rapid Re-housing"),
    Goal = c("At least 30% of households in Outreach projects will move into permanent housing at exit",
             "At least 40% of households in ES projects will move into permanent housing at exit",
             "At least 83% of households in TH projects will move into permanent housing at exit",
             "At least 83% of households in RRH projects will move into permanent housing at exit"),
    HowCalculated = c("Number of households who moved to PH upon exit / number of participants who exited project",
                      "Number of households who moved to PH upon exit / number of participants who exited ES project",
                      "Number of households who moved to PH upon exit / number of participants who exited TH project",
                      "Number of households who moved to PH upon exit / number of participants who exited RRH project")
  ) |> 
    DT::datatable(escape = FALSE)
})
