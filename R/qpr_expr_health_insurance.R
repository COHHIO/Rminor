qpr_dependencies$health_insurance <- c(
  "qpr_benefits"
)
qpr_expr$health_insurance <- list()

qpr_expr$health_insurance$expr <- rlang::expr({
  qpr_benefits() |>
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
  # input <- list(region = "Richland - Harmony House Homeless Services - HCRP RRH",
  #               date_range = c(lubridate::ymd("2020-01-01"), Sys.Date()))
})

qpr_expr$health_insurance$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(InsuranceFromAnySource == 1) |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
    by = c("ProjectName")
  ) |> 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
  
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Total Households Exiting With Health Insurance",
                  color = "gray-dark",
                  icon = "medkit",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$InsuranceAtExit, 
                                   "out of",
                                   .data$TotalHHs,
                                   "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$health_insurance$datatable <- rlang::expr({
  data_env() |>
    dplyr::mutate(
      InsuranceFromAnySource = HMIS::hud_translations$`1.8 NoYesReasons for Missing Data`(InsuranceFromAnySource)) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      ExitDate,
      "Health Insurance from Any Source (at Exit)" = InsuranceFromAnySource
    ) |> 
    datatable_default(escape = FALSE)
})

# qpr_expr$HI$expr <- rlang::expr({
#   ReportStart <- Report()$Start
#   ReportEnd <- Report()$End
#   .PT_nm <- names(ProjectType())
#   meeting_objective <- qpr_benefits() %>%
#     dplyr::filter(
#       ProjectRegion %in% input$Region &
#         ProjectType == .PT_nm &
#         HMIS::exited_between(., ReportStart, ReportEnd) &
#         InsuranceFromAnySource == 1
#     ) %>% 
#     dplyr::group_by(FriendlyProjectName,
#                     ProjectType,
#                     ProjectCounty,
#                     ProjectRegion) %>%
#     dplyr::summarise(InsuranceAtExit = dplyr::n())
#   
#   # calculating the total households for comparison
#   all_hhs <- qpr_benefits() %>%
#     dplyr::filter(ProjectRegion %in% c(input$Region) &
#                     ProjectType == names(ProjectType()) &
#                     HMIS::exited_between(., ReportStart, ReportEnd)) %>%
#     dplyr::group_by(FriendlyProjectName,
#                     ProjectType,
#                     ProjectCounty,
#                     ProjectRegion) %>%
#     dplyr::summarise(TotalHHs = dplyr::n()) 
#   
#   HIAtExit <- all_hhs %>%
#     dplyr::left_join(
#       meeting_objective,
#       by = c("FriendlyProjectName",
#              "ProjectType",
#              "ProjectCounty",
#              "ProjectRegion")
#     )
#   
#   HIAtExit[is.na(HIAtExit)] <- 0
#   
#   HIAtExit <- HIAtExit %>%
#     dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
#   
#   HIGoal <-
#     goals() %>%
#     dplyr::filter(Measure == "Health Insurance at Exit" & 
#                     ProjectType %in% unlist(ProjectType())) %>%
#     dplyr::distinct(Goal)
#   
#   HIGoal[["ProjectType"]] <- .PT_nm
#   
#   title <- paste0("Health Insurance at Exit\n", 
#                   .PT_nm, "\n",
#                   Report()$Start, " to ", Report()$End)
#   
#   region <- c(input$Region)
#   
#   stagingHI <- HIAtExit %>%
#     dplyr::left_join(HIGoal, by = "ProjectType") %>%
#     dplyr::filter(ProjectType == .PT_nm & 
#                     ProjectRegion %in% region) %>%
#     dplyr::mutate(
#       hover = paste0(
#         FriendlyProjectName, 
#         "\nHealth Insurance at Exit: ", InsuranceAtExit, 
#         "\nTotal Households: ", TotalHHs, 
#         "\n", as.integer(Percent * 100), "%",
#         sep = "\n"
#       )
#     )
#   attr(stagingHI, "title") <- title
#   stagingHI
# })
# 
# qpr_expr$HI$plot <- rlang::expr({
#   qpr_plotly(
#     data_env(),
#     title = attr(data_env(), "title")
#   )
# })
