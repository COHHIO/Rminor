qpr_dependencies$RRHspending <- c(
  "qpr_spending",
  "goals"
)
qpr_expr$rrh_spending <- list()
spending_filter <- function(x, date_range, region, ProjectType) {
  x |> 
    HMIS::entered_between(date_range[1], date_range[2]) |> 
    dplyr::filter(
      OrganizationName == region &
        ProjectType == {{ProjectType}}
    )
}
qpr_expr$rrh_spending$expr <- rlang::expr({
  .data <- qpr_spending()
  list(
    rrh = spending_dt(spending_filter(.data, input$date_range, input$region, 13)),
    hp = spending_dt(spending_filter(.data, input$date_range, input$region, 12))
  )
})

qpr_expr$rrh_spending$infobox <- list(
  rlang::expr({
    qpr_infobox(
      data_env(),
      title = "RRH Spending",
      color = "success",
      value = scales::dollar(sum(.data$rrh$Amount)) 
    )
  }),
  rlang::expr({
    qpr_infobox(
      data_env(),
      title = "HP Spending",
      color = "success",
      value = scales::dollar(sum(.data$hp$Amount))
    )
  })
)

spending_dt <- function(x) {
  x |> 
    dplyr::mutate(ProjectName = as.factor(ProjectName)) |>
    dplyr::group_by(UniqueID, ProjectName, ServiceStartDate, ServiceItemName) |> 
    dplyr::summarize(Amount = sum(ServiceAmount)) |> 
    dplyr::select(
      UniqueID,
      "Project Name" = ProjectName,
      "Service Date" = ServiceStartDate,
      Description = ServiceItemName,
      Amount
    ) |> 
    dplyr::arrange(dplyr::desc(Amount))
  
}

qpr_expr$rrh_spending$datatable <- list(
  `Rapid Rehousing Spending` = rlang::expr({
    datatable_default(data_env()$rrh, escape = FALSE)
  }),
  `Homeless Prevention Spending` = rlang::expr({
    datatable_default(data_env()$hp, escape = FALSE)
  })
)
# qpr_expr$RRHspending$expr <- rlang::expr({
#   ReportStart <- Report()$Start
#   ReportEnd <- Report()$End
#   rrhSpending <- qpr_spending() %>%
#     dplyr::filter(
#       !is.na(OrganizationName) &
#         ProjectRegion %in% c(input$Region) &
#         HMIS::entered_between(., ReportStart, ReportEnd)
#     ) %>%
#     dplyr::mutate(ProjectType = dplyr::if_else(ProjectType == 12,
#                                                "HP",
#                                                "RRH"),
#                   ProjectType = factor(ProjectType, levels = c("HP", "RRH")))
#   
#   
#   rrhSpending <- qpr_spending() %>%
#     dplyr::filter(
#       !is.na(OrganizationName) &
#         ProjectRegion %in% c(input$Region) &
#         HMIS::entered_between(., ReportStart, ReportEnd)
#     ) %>%
#     dplyr::select(OrganizationName, ProjectRegion) %>%
#     unique() %>% 
#     dplyr::bind_rows(., .) %>% 
#     dplyr::arrange(OrganizationName) %>% 
#     dplyr::mutate(ProjectType = factor(rep_len(c("HP", "RRH"), length.out = nrow(.)))) %>% 
#     {dplyr::right_join(rrhSpending, ., by = c("OrganizationName",
#                                          "ProjectRegion",
#                                          "ProjectType"))}
#   
# 
#   
#   # rrhSpending  <- rrhSpending %>% dplyr::mutate(PersonalID = dplyr::if_else(is.na(PersonalID), 4216, PersonalID),
#   #                 EntryDate = dplyr::if_else(PersonalID == 4216,
#   #                                            ReportStart,
#   #                                            EntryDate),
#   #                 MoveInDateAdjust = dplyr::if_else(PersonalID == 4216,
#   #                                                   ReportStart,
#   #                                                   EntryDate),
#   #                 ExitDate = dplyr::if_else(PersonalID == 4216,
#   #                                           ReportEnd,
#   #                                           EntryDate))
#   
#   
#   #CHANGED  ymd/mdy conversions for all values can be expensive as the size of NA data grows. Using base r we can just store the existing Dates in the appropriate places without coercion saving computational resources.
#   # get the indices of missing PersonalIDs
#   .na_ind <- is.na(rrhSpending$PersonalID)
#   # Default to EntryDate for all MoveInDateAdjust & ExitDate
#   rrhSpending$MoveInDateAdjust <- rrhSpending$EntryDate
#   rrhSpending$ExitDate <- rrhSpending$EntryDate
#   # Use 4216 for all NA PersonalID
#   rrhSpending[.na_ind, c("PersonalID")] <- 4216
#   # For NA PersonalID use ReportStart for EntryDate and MoveInDateAdjust
#   rrhSpending[.na_ind, c("EntryDate")] <- ReportStart
#   rrhSpending[.na_ind, c("MoveInDateAdjust")] <- ReportStart
#   # For NA PersonalID use ReportEnd for ExitDate
#   rrhSpending[.na_ind, c("ExitDate")] <- ReportEnd
#   
#   rrhSpending <- rrhSpending  %>%
#     dplyr::group_by(OrganizationName, ProjectRegion, ProjectType) %>%
#     dplyr::summarise(Amount = sum(Amount),
#                      HHs = dplyr::n()) %>%
#     dplyr::ungroup() %>%
#     tidyr::spread(ProjectType, Amount)
#   
#   rrhSpending[is.na(rrhSpending)] <- 0
#   
#   rrhSpending <- rrhSpending %>%
#     dplyr::group_by(OrganizationName, ProjectRegion) %>%
#     dplyr::summarise(HHs = sum(HHs),
#                      RRH = sum(RRH),
#                      HP = sum(HP)) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(HP > 0 | RRH > 0) %>%
#     # NOTE Goal is hardcoded here whereas it was typically pulled from the goals object in other QPRs. Is this intended?
#     dplyr::mutate(Goal = 0.75,
#                   PercentRRH = RRH/(RRH + HP),
#                   hover = paste0(
#                     OrganizationName,
#                     "\nPercent Spent on RRH: ",
#                     scales::percent(PercentRRH),
#                     "\nTotal Spent on RRH: $",
#                     RRH,
#                     "\nTotal Spent on Prevention: $",
#                     HP,
#                     "\nTotal Households: ",
#                     HHs,
#                     sep = "\n"
#                   ))
#   
#   title <- paste0("Percent Spent on Rapid Rehousing\n",
#                   Report()$Start, " to ", Report()$End)
#   attr(rrhSpending, "title") <- title
#   rrhSpending
# })
# 
# qpr_expr$RRHspending$plot <- rlang::expr({
#   qpr_plotly(data_env(), 
#              title = attr(data_env(), "title"),
#              x = ~ OrganizationName,
#              y = ~ PercentRRH,
#              yaxis = list(title = "RRH Spending",
#                           tickformat = "%"))
# })