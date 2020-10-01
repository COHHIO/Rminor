
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

# CHANGED Check and coerce dates as sub-function to split between_ into two logical segments, one which outputs a data.frame (for use in Rminor) and one which outputs a logical for use in COHHIO_HMIS
#' @title check_dates
#' @noRd
#' @keywords Internal
#' @importFrom purrr map_lgl imap
#' @importFrom vctrs vec_cast.Date
#' @importFrom lubridate parse_date_time
#' @importFrom rlang abort
check_dates <- function(start, end) {
  # Add input dates to list
  .dates <- list(start = start, end = end)
  # Check if inputs are all Date or POSIXct
  .test_date <- purrr::map_lgl(.dates, ~{inherits(.x, c("Date", "POSIXct"))})
  # If not
  if (!all(.test_date)) {
    # map over the one's that arent
    .dates <- purrr::imap(.dates[!.test_date], ~{
      # try these formats
      .out <- lubridate::parse_date_time(.x, c("Ymd", "mdY", "dmY"))
      if (!inherits(.out, c("POSIXct","Date"))) {
        # if none of those formats worked throw error and inform user which argument was not able to be parsed
        rlang::abort(paste0(.y, " could not be parsed to a Datetime, please check argument."))
      }
      .out
    })
    # bind the coerced Date/Datetimes to the environment, overwriting the existing values
  }
  vctrs::vec_cast.Date(do.call(c,.dates))
}



#CHANGED New Between function
#' @title between_df
#' @keywords Internal
#' @description Performs quick filtering of qpr_* data.frames with the input of the type of filtering
#' @param . \code{(data.frame/tibble)} Input to be filtered. In a `magrittr` pipe this will always be the first object
#' @param status \code{(character)} One of:
#' \itemize{
#'   \item{\code{"served"/"se"}}
#'   \item{\code{"stayed"/"st"}}
#'   \item{\code{"entered"/"en"}}
#'   \item{\code{"exited"/"ex"}}
#'   \item{\code{"operating"/"op"}}
#'   \item{\code{"beds_available"/"be"/"ba`}}
#' }
#' that specifies the type of function to be performed
#' @param start The ReportStart variable created from user input - will be automatically retrieved from parent environments if not specified. If start is named other than ReportStart, it must be specified.
#' @param end The ReportEnd variable created from user input - will be automatically retrieved from parent environments if not specified. If end is named other than ReportEnd, it must be specified.
#' @return \code{data.frame} after filtering the appropriate columns
#' @importFrom rlang abort sym `!!`
#' @importFrom stringr str_detect

#TODO Test with additional qpr_*, test with operating_* and beds_available_* instances
between_df <- function(., status, start = ReportStart, end = ReportEnd) {
  #Check date format and coerce if need be
  dates <- check_dates(start, end)
  
  # if no status supplied, throw error
  if (missing(status)) {
    rlang::abort("Please supply a status. See ?between_ for details.")
  } 
  # Convert that to a character for regex parsing
  .cn_chr <- tolower(substr(status, 0, 2))
  # If it's one of served of stayed
  if (stringr::str_detect(.cn_chr, "se|st")) {
    if (stringr::str_detect(.cn_chr, "se")) {
      # if served use entrydate
      .col <- rlang::sym("EntryDate")
    } else if (stringr::str_detect(.cn_chr, "st")) {
      # if stayed used entryadjust
      .col <- rlang::sym("EntryAdjust")
    }
    #filter the appropriate columns
    .out <- dplyr::filter(., !!.col <= dates["end"] & (is.na(ExitDate) | ExitDate >= dates["start"]))
  } else if (stringr::str_detect(.cn_chr, "en|ex")) {
    # if its entered or exited
    if (stringr::str_detect(.cn_chr, "en")) {
      # if entered use entrydate
      .col <- rlang::sym("EntryDate")
    } else if (stringr::str_detect(.cn_chr, "ex")) {
      #if exited use exit date
      .col <- rlang::sym("ExitDate")
    }
    # Filter the appropriate column using between
    .out <- dplyr::filter(., !!.col >= dates["start"] & !!.col <= dates["end"])
  } else if (stringr::str_detect(.cn_chr, "ba|be|op")) {
    if (stringr::str_detect(.cn_chr, "op")) {
      .prefix <- "Operating"
    } else if (stringr::str_detect(.cn_chr, "be|ba")) {
      .prefix <- "Inventory"
    }
    # Construct column names from prefixes
    .cols <- paste0(.prefix, c("StartDate", "EndDate"))
    # Extract the appropriate columns
    .cols <- purrr::map(.cols, rlang::sym)
    # Do the filtering
    
    .out <- dplyr::filter(., 
                          !!.cols[[1]] <= dates["end"] &
                          (is.na(!!.cols[[2]]) | !!.cols[[2]] >= dates["start"])
                          )
    
  }
  .out
}




# Client Entry Exits Between Date Range Functions -------------------------------------
#' @title served_between
#' @family *_between
#' @description Filters a data.frame between the start and end dates specified
#' @inheritParams between_df
#' @return \code{data.frame} after filtering the appropriate columns
#' @export
#' @examples 
#' \dontrun{
#' ReportStart = Sys.Date() - lubridate::weeks(4)
#' ReportEnd = Sys.Date()
#' qpr_leavers %>% served_between()
#' }
served_between <- function(., start = ReportStart, end = ReportEnd) {
  between_df(., "served", start, end)
}

#' @title entered_between
#' @family *_between
#' @export
entered_between <- function(., start = ReportStart, end = ReportEnd) {
  between_df(., "entered", start, end)
}

#' @title exited_between
#' @family *_between
#' @export
exited_between <- function(., start = ReportStart, end = ReportEnd){
  between_df(., "exited", start, end)
}

#' @title stayed_between
#' @family *_between
#' @export
stayed_between <- function(., start = ReportStart, end = ReportEnd){
  between_df(., "stayed", start, end)
}

#' @title operating_between
#' @family *_between
#' @export
operating_between <- function(., start = ReportStart, end = ReportEnd){
  between_df(., "op", start, end)
}

#' @title beds_available_between
#' @family *_between
#' @export
beds_available_between <- function(., start = ReportStart, end = ReportEnd){
  between_df(., "ba", start, end)
}