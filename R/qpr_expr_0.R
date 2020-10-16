# ReportStart <- lubridate::ymd("2020-01-01")
# ReportEnd <- lubridate::today()
# input <- list(
#   ProjectType = choices_project_type["Rapid Rehousing"],
#   Region = c(
#     "Homeless Planning Region 14",
#     "Homeless Planning Region 4",
#     "Homeless Planning Region 16",
#     "Homeless Planning Region 14"
#   ),
#   radio_mean = "Average Days"
# )
# ProjectType <- rlang::as_function(~{input$ProjectType})
qpr_expr <- list()