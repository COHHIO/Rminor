# ReportStart <- lubridate::ymd("2020-01-01")
# ReportEnd <- lubridate::today()
# input <- list(
#   ProjectType = choices_project_type["Rapid Rehousing"],
#   Region = c(
# "Homeless Planning Region 1",
# "Homeless Planning Region 2",
# "Homeless Planning Region 3",
# "Homeless Planning Region 4",
# "Homeless Planning Region 5",
# "Homeless Planning Region 6",
# "Homeless Planning Region 7",
# "Homeless Planning Region 8",
# "Homeless Planning Region 9",
# "Homeless Planning Region 10",
# "Homeless Planning Region 11",
# "Homeless Planning Region 12",
# "Homeless Planning Region 13",
# "Homeless Planning Region 14",
# "Homeless Planning Region 15",
# "Homeless Planning Region 16",
# "Homeless Planning Region 17"
# ),
#   radio_mean = "Average Days"
# )
# ProjectType <- rlang::as_function(~{input$ProjectType})
#' @title qpr_expr
#' @description List to hold all module dependency expressions
#' @export

qpr_expr <- list()