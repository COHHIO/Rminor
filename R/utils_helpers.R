
#' @title qstart_date
#' @description Transform quarter slider text input to start date for filtering
#' @param . \code{(character)} The input to transform
#' @return \code{(Date)} The corresponding Date
#' @keywords Internal
#' @examples \dontrun{
#' ReportStart <- qstart_date(input$slider)
#' }
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

#' @title qpr_plotly
#' @description A boilerplate plot_ly layout for QPR tab bar graphs with arguments for values that change across tabs.
#' @param .d \code{(data.frame)} Input data
#' @param title \code{(character/list)} 
#' @param x \code{(formula)} Formula for variable on x-axis in plotly tilde format
#' @param xaxis \code{(list)} of x-axis label parameters
#' @param y \code{(formula)} Formula for variable on y-axis in plotly tilde format
#' @param yaxis \code{(list)} of y-axis label parameters
#' @param ... \code{(named lists)} of arguments passed on to \link[plotly]{layout}. These will take the place of boilerplate values, preserving unspecified boilerplate parameters. If boilerplate parameters should be replaced entirely with specified values, set `.sub = FALSE`. For boilerplate layout parameters, see examples.
#' @param .sub \code{(logical)} flag to specify whether boilerplate parameters should be switched out for those specified to `...` or replaced entirely.
#' @return \code{(plotly)} graph object to be passed to \link[plotly]{renderPlotly}
#' @examples 
#' \dontrun{
#' # Boilerplate plotly layout options
#' .opts_layout <- list(
#'p = .p,
#'xaxis = xaxis,
#'yaxis = yaxis,
#'title = list(
#'  text = title,
#'  font = list(
#'    size = 15
#'  )),
#'margin = list(
#'  l = 50,
#'  r = 50,
#'  b = 100,
#'  t = 100,
#'  pad = 4
#'),
#'shapes = list(
#'  type = "rect",
#'  name = "CoC Goal",
#'  fillcolor = "#008000",
#'  line = list(color = "white", width = .01),
#'  layer = "below",
#'  xref = "paper",
#'  yref = "y",
#'  x0 = 0,
#'  x1 = 1,
#'  y0 = ~ Goal[1],
#'  y1 = 1,
#'  opacity = .2
#'),
#'title = title
#')
#' }
#' @importFrom purrr map2 list_modify
#' @importFrom rlang dots_list
qpr_plotly <- function(.d, title, x = ~ FriendlyProjectName, xaxis = list(title = ""), y = ~ Percent, yaxis =  list(title = "Households", tickformat = "%"), ..., .sub = TRUE) {
  # If no data return no graph
  if (nrow(.d) < 1) return(NULL)
  .p <- plotly::plot_ly(
    .d,
    x = x,
    y = y,
    text = ~ hover,
    hoverinfo = 'text',
    type = "bar"
  )
  # Get the named dot params
  .dot_params <- rlang::dots_list(..., .named = TRUE)
  # Create the boilerplate options
  .opts_layout <- list(
    p = .p,
    xaxis = xaxis,
    yaxis = yaxis,
    title = list(
      text = title,
      font = list(
        size = 15
      )),
    margin = list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    ),
    shapes = list(
      type = "rect",
      name = "CoC Goal",
      fillcolor = "#008000",
      line = list(color = "white", width = .01),
      layer = "below",
      xref = "paper",
      yref = "y",
      x0 = 0,
      x1 = 1,
      y0 = ~ Goal[1],
      y1 = 1,
      opacity = .2
    ),
    title = title
  )
  # If dot params were specified
  if (length(.dot_params) > 0) {
    # If we're substituting them into the boilerplate
    if (isTRUE(.sub)) {
      # Map over the parameters specified with dots
      .opts_layout[names(.dot_params)] <- purrr::map2(.opts_layout[names(.dot_params)], .dot_params, ~{
        # Trade the values in boilerplate list with those specified by dots
        purrr::list_modify(.x, .y)
      })
    } else {
      # Replace the values outright
      .opts[names(.dot_params)] <- .dot_params
    }
  }
  .out <- do.call(plotly::layout, .opts_layout)
  return(.out)
}



#' @title find_path
#' @description Finds a file or directory by traversing the directory tree.
#' @param .fn The case-sensitive regex for the file passed to \code{\link[base]{list.files}}.
#' @param n the number of parent directories to traverse before erroring
#' @return The objects are loaded into the specified environment. 
#' @examples
#' find_path("Rminor.RData")
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

