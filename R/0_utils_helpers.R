

#' @title qstart_date
#' @description Transform quarter slider text input to start date for filtering
#' @param . \code{(character)} The input to transform
#' @return \code{(Date)} The corresponding Date
#' @keywords Internal
#' @examples \dontrun{
#' ReportStart <- qstart_date(input$slider)
#' }
#' @export
qstart_date <- function(.) {
  lubridate::ymd(paste0(
  substr(., 1, 4),
  "-01-01"
))
}

#' @title qend_date
#' @description Transform quarter slider text input to end date for filtering
#' @param . \code{(character)} The input to transform
#' @return \code{(Date)} The corresponding Date
#' @keywords Internal
#' @examples \dontrun{
#' ReportEnd <- qend_date(input$slider)
#' }
#' @export
qend_date <- function(.) {
  lubridate::mdy(paste0(
    dplyr::case_when(
      substr(., 7, 7) == 1 ~ "03-31-",
      substr(., 7, 7) == 2 ~ "06-30-",
      substr(., 7, 7) == 3 ~ "09-30-",
      substr(., 7, 7) == 4 ~ "12-31-"
    ),
    substr(., 1, 4)
  ))
}



#' @title find_path
#' @description Finds a file or directory by traversing the directory tree.
#' @param .fn The case-sensitive regex for the file passed to \code{\link[base]{list.files}}.
#' @param n the number of parent directories to traverse before erroring
#' @return The objects are loaded into the specified environment. 
#' @examples
#' find_path("Rminor.rds")
#' @importFrom rlang warn abort
#' @export

find_path <- function(.fn, n = 4) {
  .path <- '.'
  .fs <- list.files(.path, pattern = .fn, all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  i <- 1
  while (length(.fs) < 1 && i <= n) {
    .path <- file.path('..', .path)
    .fs <- list.files(.path, pattern = .fn, all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
    i <- 1 + i
  }
  if (length(.fs) > 1) {
    .fs <- .fs[which.min(nchar(.fs))]
   rlang::warn(paste0('More than one path found. Using ', 
                      .fs))
  } else if (length(.fs) < 1) {
    rlang::abort('No path found for ', .fn)
  }  
  return(.fs)
}

