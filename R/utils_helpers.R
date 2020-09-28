
#' @title qstart_date
#' @description Transform quarter slider text input to start date for filtering
#' @param . \code{(character)} The input to transform
#' @return \code{(Date)} The corresponding Date
#' @keywords Internal
#' @examples \dontrun{
#' ReportStart <- qstart_date(input$slider)
#' }
qstart_date <- function(input) {
  format.Date(lubridate::ymd(paste0(
  substr(input, 1, 4),
  "-01-01"
)), "%m-%d-%Y")
}

#' @title qend_date
#' @description Transform quarter slider text input to end date for filtering
#' @param . \code{(character)} The input to transform
#' @return \code{(Date)} The corresponding Date
#' @keywords Internal
#' @examples \dontrun{
#' ReportEnd <- qend_date(input$slider)
#' }
qend_date <- function(input) {
  format.Date(lubridate::mdy(paste0(
    dplyr::case_when(
      substr(input, 7, 7) == 1 ~ "03-31-",
      substr(input, 7, 7) == 2 ~ "06-30-",
      substr(input, 7, 7) == 3 ~ "09-30-",
      substr(input, 7, 7) == 4 ~ "12-31-"
    ),
    substr(input, 1, 4)
  )), "%m-%d-%Y")
}
