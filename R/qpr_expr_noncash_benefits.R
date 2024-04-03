qpr_dependencies$NCB <- c(
  "qpr_benefits"
)
qpr_expr$noncash_benefits <- list()
qpr_expr$noncash_benefits$expr <- rlang::expr({
  req(input$date_range, input$region)
  qpr_benefits() |>
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$noncash_benefits$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(BenefitsFromAnySource == 1) |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
    by = "ProjectName"
  ) |> 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  icon = "shopping-cart",
                  color = "fuchsia",
                  value = scales::percent(.data$Percent),
                  title = "Households Exiting With Non Cash Benefits",
                  subtitle = paste(.data$BenefitsAtExit, 
                                   "out of",
                                   .data$TotalHHs, 
                                   "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$noncash_benefits$datatable <- rlang::expr({
  data_env() |>
    dplyr::mutate(
      BenefitsFromAnySource = HMIS::hud_translations$`1.8 NoYesReasons for Missing Data`(BenefitsFromAnySource)
    ) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      ExitDate,
      "Benefits from Any Source (at Exit)" = BenefitsFromAnySource
    ) |> 
    datatable_default(escape = FALSE)
})
# qpr_expr$noncash_benefits$expr <- rlang::expr({
#   .PT_nm <- names(ProjectType()) 
#   meeting_objective <- qpr_benefits() %>%
#     dplyr::filter(
#       ProjectRegion %in% c(input$Region) &
#         ProjectType %in% .PT_nm &
#         HMIS::exited_between(., ReportStart, ReportEnd) &
#         BenefitsFromAnySource == 1
#     ) %>% 
#     dplyr::group_by(FriendlyProjectName, 
#                     ProjectType, 
#                     ProjectCounty, 
#                     ProjectRegion) %>%
#     dplyr::summarise(BenefitsAtExit = dplyr::n())
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
#   NCBsAtExit <- all_hhs %>%
#     dplyr::left_join(
#       meeting_objective,
#       by = c("FriendlyProjectName", 
#              "ProjectType", 
#              "ProjectCounty", 
#              "ProjectRegion")
#     )
#   
#   NCBsAtExit[is.na(NCBsAtExit)] <- 0
#   
#   NCBsAtExit <- NCBsAtExit %>%
#     dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
#   
#   NCBGoal <-
#     goals() %>%
#     dplyr::filter(Measure == "Non-cash Benefits" & 
#                     ProjectType %in% unlist(ProjectType())) %>% 
#     # TODO This won't be necessary once qpr_benefits also uses numeric
#     dplyr::distinct(Goal)
#   NCBGoal[["ProjectType"]] <- .PT_nm
#   
#   title <- paste0("Non-Cash Benefits at Exit\n", 
#                   .PT_nm, "\n",
#                   Report()$Start, " to ", Report()$End)
#   
#   stagingNCBs <- NCBsAtExit %>%
#     dplyr::left_join(NCBGoal, by = "ProjectType") %>%
#     dplyr::filter(ProjectType %in% .PT_nm & 
#                     ProjectRegion %in% input$Region) %>%
#     dplyr::mutate(
#       hover = paste0(
#         FriendlyProjectName, 
#         "\nReceiving Non-Cash Benefits at Exit: ", BenefitsAtExit, 
#         "\nTotal Households: ", TotalHHs, 
#         "\n", as.integer(Percent * 100), "%",
#         sep = "\n"
#       )
#     )
#   attr(stagingNCBs, "title") <- title
#   stagingNCBs
# })

# qpr_expr$NCB$plot <- rlang::expr({
#   qpr_plotly(
#     data_env(),
#     title = attr(data_env(), "title")
#   )
# })
