#' @title Create a default header block
#'
#' @param title \code{(character)} Title to be wrapped in \link[shiny]{h2}
#' @param program \code{(character)} Program name to be wrapped in \link[shiny]{h4}
#' @param date_range \code{(Date)} vector to be collapsed with `" - "` in \link[shiny]{h4}
#'
#' @return \code{shiny.tag.list}
#' @export

server_header <- function(title, ..., program, date_range, region, county) {
  out <- list()
  out$header <- shiny::h2(title)
  if (!missing(program))
    out$program <- shiny::h4(names(programs)[programs %in% program])
  if (!missing(region))
    out$region <- shiny::h4("Region(s):", paste0(names(regions)[regions %in% region], collapse = ", "))
  if (!missing(county))
    out$county <- shiny::h4("County(ies):", paste0(names(counties)[counties %in% county], collapse = ", "))
  if (!missing(date_range)) {
    out$dr <- purrr::when(date_range, length(.) > 1 ~ shiny::h4(paste0(.[1]," - ", .[2])),
                          ~ shiny::h4(.))
  }
  .dots <- rlang::dots_list(...)
  if (UU::is_legit(.dots))
    out <- append(out, .dots)
  do.call(tagList, out)
}


#' @title DT Datatable with some helpful defaults
#'
#' @inheritParams DT::datatable
#' @inheritDotParams DT::datatable
#' @param add_options Options to add to the existing defaults
#' @param add_extensions Extensions to add to the existing defaults
#' @return \code{(shiny.tag)}
#' @export

datatable_default <- function(data,
                              rownames = FALSE,
                              options = list(
                                dom = 'Blfrtip',
                                buttons = list(
                                  'copy',
                                  'excel',
                                  "csvHtml5",
                                  list(
                                    extend = "csvHtml5",
                                    text = "Full CSV",
                                    filename = "data_full",
                                    exportOptions = list(modifier = list(page = "all"))
                                  )
                                ),
                                responsive = TRUE,
                                lengthMenu = c(10, 25, 50, 75, 100),
                                lengthChange = TRUE,
                                pageLength = 10
                              ),
                              filter = list(position = 'top',
                                            clear = TRUE,
                                            plain = FALSE),
                              width = "100%",
                              height = "auto",
                              extensions = "Buttons",
                              style = "bootstrap4",
                              elementId = NULL,
                              add_options,
                              add_extensions,
                              ...) {
  
  if (!missing(add_options))
    options <- purrr::list_modify(options, !!!add_options)
  if (!missing(add_extensions))
    extensions <- c(extensions, add_extensions)
  
  DT::datatable(
    data,
    rownames = rownames,
    filter = filter,
    options = options,
    extensions = extensions,
    style = style,
    elementId = elementId,
    width = width,
    height = height,
    ...
  )
}


#' @title Give DT ready column numbers from names or numbers
#'
#' @param x \code{(character/numeric)} Column numbers or names
#'
#' @return
#' @export

which_cols <- function(x, .data) {
  UseMethod("which_cols")
}

#' @export
which_cols.numeric <- function(x, .data)
  x

#' @export
which_cols.character <- function(x, .data) {
  dplyr::matches(UU::regex_or(x), vars = names(.data))
}

#' @title Update datatable options
#'
#' @param x \code{(datatables)}
#' @param options \code{(list)} of options to replace
#'
#' @return \code{(datatables)}
#' @export

datatable_options_update <- function(x, options, hide_cols) {
  out <- x
  
  if (missing(options))
    options <- out$x$options
  if (!missing(hide_cols))
    options <- append(options, list(columnDefs = list(
      list(
        visible = FALSE,
        targets = which_cols(hide_cols, x$x$data) - 1 # js numbers start with 0
      )
    )))
  if (UU::is_legit(out$x$options$columnDefs) &&
      UU::is_legit(options$columnDefs)) {
    out$x$options$columnDefs <-
      append(out$x$options$columnDefs, options$columnDefs)
    options$columnDefs <- NULL
  }
  if (UU::is_legit(options))
    out$x$options <- purrr::list_modify(out$x$options,!!!options)
  
  out
}

#' @title Add \link[DT]{styleColorBar} or `styleDivergentBar` to datatable
#'
#' @inheritParams DT::formatStyle
#' @inheritDotParams DT::formatStyle
#' @inheritDotParams DT::styleColorBar
#' @inheritDotParams styleDivergentBar
#' @param divergent \code{(logical)} Whether to use `styleDivergentBar`
#'
#' @return \code{(datatable)}
#' @export

datatable_add_bars <-
  function(table,
           columns,
           valueColumns,
           ...,
           divergent = FALSE) {
    .args <- rlang::dots_list(..., .named = TRUE)
    .args$table <- table
    .data <- table$x$data
    .data_nms <- names(.data)
    
    if (missing(columns) && divergent)
      .args$columns = stringr::str_which(.data_nms, stringr::regex("frequency", ignore_case = TRUE))
    else
      .args$columns <- columns
    
    if (missing(valueColumns) && divergent)
      .args$valueColumns = stringr::str_which(.data_nms,
                                              stringr::regex("rank|from_mean", ignore_case = TRUE))
    else
      .args$valueColumns <- valueColumns
    
    if (divergent)
      bar_args <-
      purrr::list_modify(.args[names(.args) %in% c("color_pos", "color_neg")], color_pos = "#28a745", color_neg = "#dc3545")
    else
      bar_args <- .args[names(.args) %in% c("data", "color", "angle")]
    
    .args$background <-
      purrr::map(.args$valueColumns,
                 ~ rlang::exec(
                   purrr::when(divergent, . ~ styleDivergentBar, ~ DT::styleColorBar),
                   range(.data[[.x]]),
                   !!!bar_args
                 )) |>
      {
        \(x) {
          purrr::when(length(x), . == 1 ~ x[[1]], ~ x)
        }
      }()
    
    rlang::exec(DT::formatStyle,!!!.args)
  }



server_debounce <-
  function(...,
           wait = 1500,
           e = rlang::caller_env()) {
    ex <- rlang::enexprs(..., .named = TRUE)
    exs <- purrr::imap(ex, ~ {
      ex <- rlang::expr({
        `<<-`(
          !!rlang::sym(stringr::str_remove(.y, "^input\\$")),
          shiny::debounce(shiny::eventReactive(!!.x,!!.x),!!wait)
        )
      })
    })
    insts <-
      purrr::imap(ex, ~ rlang::expr(`<-`(!!rlang::sym(stringr::str_remove(.y, "^input\\$")), function() {
        
      })))
    inst <- rlang::expr({
      !!!insts
    })
    ob <- rlang::expr(observeEvent(!!ex[[1]], {
      !!!exs
    }, ignoreInit = TRUE))
    purrr::map(list(inst, ob), rlang::eval_bare, env = e)
  }


#' @title Style DT divergent color bar
#'
#' Style DT color bars for values that diverge from 0. From \href{https://github.com/federicomarini/GeneTonic}{federicomarini/GeneTonic}
#'
#' @details This function draws background color bars behind table cells in a column,
#' width the width of bars being proportional to the column values *and* the color
#' dependent on the sign of the value.
#'
#' A typical usage is for values such as `log2FoldChange` for tables resulting from
#' differential expression analysis.
#' Still, the functionality of this can be quickly generalized to other cases -
#' see in the examples.
#'
#' The code of this function is heavily inspired from styleColorBar, and borrows
#' at full hands from an excellent post on StackOverflow -
#' https://stackoverflow.com/questions/33521828/stylecolorbar-center-and-shift-left-right-dependent-on-sign/33524422#33524422
#'
#' @param data The numeric vector whose range will be used for scaling the table
#' data from 0-100 before being represented as color bars. A vector of length 2
#' is acceptable here for specifying a range possibly wider or narrower than the
#' range of the table data itself.
#' @param color_pos The color of the bars for the positive values
#' @param color_neg The color of the bars for the negative values
#'
#' @return This function generates JavaScript and CSS code from the values
#' specified in R, to be used in DT tables formatting.
#'
#' @export
#'
#' @examples
#' simplest_df <- data.frame(
#'   a = c(rep("a", 9)),
#'   value = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
#' )
#'
#' # or with a very simple data frame
#' DT::datatable(simplest_df) %>%
#'   DT::formatStyle(
#'     "value",
#'     background = styleDivergentBar(
#'       simplest_df$value,
#'       scales::alpha("forestgreen", 0.4),
#'       scales::alpha("gold", 0.4)
#'     ),
#'     backgroundSize = "100% 90%",
#'     backgroundRepeat = "no-repeat",
#'     backgroundPosition = "center"
#'   )
styleDivergentBar <- function(data,
                              color_pos,
                              color_neg) {
  max_val <- max(abs(data))
  htmlwidgets::JS(
    sprintf(
      "isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
      max_val,
      color_pos,
      max_val,
      color_pos,
      color_neg,
      color_neg,
      max_val,
      max_val
    )
  )
}
