#' @title A pickerInput that provides active programs to select from
#'
#' @param inputId \code{(namespaced character)} A character vector wrapped in `ns` from the parent environment.
#' @inherit shinyWidgets::pickerInput params return
#' @param inputId \code{(character)} Automatically namespace with ID `'project'` if non specified.
#' @inheritDotParams shinyWidgets::pickerInput
#' @param add_options \code{(list)} of options to add to existing defaults

#' @export

ui_picker_program <- function(
  label = "Select Program",
  inputId = rlang::caller_env()$ns("program"),
  choices = programs, 
  selected = NULL,
  multiple = TRUE,
  options = shinyWidgets::pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
  
  ...,
  add_options) {
  if (!missing(add_options))
    options <- purrr::list_modify(options, !!!do.call(shinyWidgets::pickerOptions, add_options))
  shinyWidgets::pickerInput(
    label = label,
    inputId = inputId,
    choices = choices,
    options = options,
    multiple = multiple,
    selected = selected,
    ...
  )
}


prev_quarter <- function() lubridate::floor_date(lubridate::floor_date(Sys.Date(), "quarter") - 1, "quarter")

prev_month <- function() lubridate::floor_date(lubridate::floor_date(Sys.Date(), "month") - 1, "month
                                               ")

#' @title The UI Header output
#'
#' @param outputId \code{(namespaced character)} A character vector wrapped in `ns` from the parent environment.
#' @inheritParams bs4Dash::box
#' @return A fluidrow containing a minimizable box with the header
#' @export

ui_header_row <-
  function(outputId = rlang::caller_env()$ns("header"),
           width = 12,
           headerBorder = FALSE) {
    shiny::fluidRow(bs4Dash::box(
      style = "padding: .1rem 1.25rem;",
      shiny::htmlOutput(outputId),
      width = width,
      headerBorder = headerBorder
    ))
  }

#' @title A date range picker with sensible defaults
#'
#' @inherit shiny::dateRangeInput params return
#' @inheritDotParams shiny::dateRangeInput
#' @export

ui_date_range <- function(
  inputId = rlang::caller_env()$ns("date_range"),
  label = "Date Range",
  start = prev_month(),
  end = Sys.Date(),
  min = rm_dates()$meta_HUDCSV$Export_Start,
  width = 300,
  ...
  ) {
  shiny::dateRangeInput(
    inputId = inputId,
    label = label,
    start = start,
    end = end,
    min = min,
    width = width,
    ...
  )
}

#' @title Make columns from assorted shiny.tag elements
#' Sorts shiny.tags into columns based on the maximum number of columns (`max_cols`) per row
#' @param x \code{(shiny.tags)}
#' @param max_cols \code{(logical/integer)} Either `TRUE` **Default** for a default of 4 columns per row, `FALSE` for no columns, or an integer indicating the max number of columns. 
#'
#' @return \code{(list(s))}
#' @export

make_columns <- function(x, max_cols = TRUE, fn = list(bs4Dash::box, bs4Dash::column)[[1]]) {
  max_cols <- purrr::when(isTRUE(max_cols),
                          . ~ 4, 
                          ~ max_cols)
  
  if (max_cols) {
    ld <- nrow(x)
    rows <- x |> 
      dplyr::mutate(.g = rep(1:ld, each = max_cols, length.out = ld)) |> 
      dplyr::group_by(.g) |> 
      dplyr::group_split(.keep = FALSE)
    
    out <- purrr::map(rows, ~{
      .cols <- .x
      .width = 12 %/% max_cols
      do.call(shiny::fluidRow, 
              purrr::pmap(.cols, ~ {
                .args <- rlang::dots_list(..., .named = TRUE)
                .lgl <- names(.args) %in% rlang::fn_fmls_names(fn)
                .args <- append(.args[.lgl], unname(.args[!.lgl]))
                
                rlang::exec(fn,
                            !!!.args,
                            width = .width
                            )
              })
      )
    })
  } else 
    out <- x
  out
}

#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @return A full-width \link[bs4Dash]{box} nested in a row
#' @export
#'
#' @examples
#' ui_row(shiny::tags$p("Hi"))
ui_row <- function(...,
                       title = NULL,
                       footer = NULL,
                       status = NULL,
                       solidHeader = FALSE,
                       background = NULL,
                       width = 12,
                       height = NULL,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       gradient = FALSE,
                       boxToolSize = "sm",
                       elevation = NULL,
                       headerBorder = TRUE,
                       label = NULL,
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       id = NULL,
                       box = TRUE
                       ) {
  .dots <- rlang::dots_list(...)
  .args <- list(title = title,
                footer = footer,
                status = status,
                solidHeader = solidHeader,
                background = background,
                width = width,
                height = height,
                collapsible = collapsible,
                collapsed = collapsed,
                closable = closable,
                maximizable = maximizable,
                icon = icon,
                gradient = gradient,
                boxToolSize = "sm",
                elevation = elevation,
                headerBorder = headerBorder,
                label = label,
                dropdownMenu = dropdownMenu,
                sidebar = sidebar,
                id = id)
  
  if (UU::is_legit(.dots)) {
    out <- shiny::fluidRow(class = "ui_row", eval(
      rlang::call2(
        purrr::when(box, . ~ bs4Dash::box, ~ shiny::tagList),
        !!!purrr::when(box,. && UU::is_legit(.dots) ~ append(.args, .dots), . ~ .args,  ~ .dots)
      )
    ))
  } else
    out <- NULL
  out
}

#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @return A \link[bs4Dash]{box} with solid header
#' @export
#'
#' @examples
#' ui_solid_box("Hi")
ui_solid_box <- function(...,
                       title = NULL,
                       footer = NULL,
                       status = NULL,
                       solidHeader = TRUE,
                       background = NULL,
                       width = 12,
                       height = NULL,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       gradient = FALSE,
                       boxToolSize = "sm",
                       elevation = NULL,
                       headerBorder = TRUE,
                       label = NULL,
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       id = NULL) {
  if (!missing(id))
    id = purrr::when(id, 
                     stringr::str_detect(., "^dq\\_box\\_", negate = TRUE) ~ paste0("dq_box_", .),
                     ~ .)
  .dots <- rlang::dots_list(...)
  if (UU::is_legit(.dots)) {
    out <- shiny::fluidRow(class = "ui_row", eval(
      rlang::call2(
        bs4Dash::box,
        title = title,
        footer = footer,
        status = status,
        solidHeader = solidHeader,
        background = background,
        width = width,
        height = height,
        collapsible = collapsible,
        collapsed = collapsed,
        closable = closable,
        maximizable = maximizable,
        icon = icon,
        gradient = gradient,
        boxToolSize = "sm",
        elevation = elevation,
        headerBorder = headerBorder,
        label = label,
        dropdownMenu = dropdownMenu,
        sidebar = sidebar,
        id = id,
        !!!.dots
      )
    ))
  } else {
    out <- NULL
  }
  out
}


fun_arg_maker <- function(fn) {
  rlang::fn_fmls(fn) |> purrr::imap_chr(~paste0(.y, ifelse(!is.null(.x), paste0(" = ", .x), ""),",")) |> cat(sep = "\n")
}

fun_arg_pass <- function(fn) {
  rlang::fn_fmls(fn) |> purrr::imap_chr(~paste0(.y, " = ",.y,",")) |> cat(sep = "\n")
}

#' @title Construct a list from various elements
#' @description The `icon` is placed before `text`. Any additional arguments will be added after `text`.
#' @param x \code{(data.frame)} with a **Required** `text` column and **Optional** `style` & `icon` columns
#' @param ... named elements with which to make a data.frame.
#' @param ordered \code{(logical)}  Whether the list should be ordered `<ol>`
#' @return \code{(shiny.tag)}
#' @export

ui_list <- function(x, ..., l_style = NULL, ordered = FALSE) {
  if (missing(x))
    x <- tibble::tibble(...)
  stopifnot(is.data.frame(x))
  rlang::exec(
    purrr::when(ordered, . ~ shiny::tags$ol, shiny::tags$ul),
    style = l_style,
    !!!(x |>
          purrr::pmap( ~ {
            .x <- list(...)
            rlang::exec(shiny::tags$li, style = .x$style, .x$icon, .x$text,!!!.x[!names(.x) %in% c("text", "icon", "style")])
          }))
  )
}



#' @title Iterative generate output functions
#'
#' @param x \code{(list)} of items to iterate over
#' @param fn \code{(fn)} output function to apply
#' @param outputId \code{(character)} The namespace ID (1,2,3 will be appended for each iteration)
#' @param header_names \code{(logical)} Whether to create \link[shiny]{h4} headers above each item using the name
#' @param ns \code{(function)} ns function from the enclosing shiny context
#' @param ... Further arguments passed on to `fn`
#' @return \code{(shiny.tag.list)}
#' @export

iterate <- function(x, fn, outputId, env = rlang::caller_env(), ..., rc = shiny::getDefaultReactiveDomain()) {
  # Check if we are generating UI or dealing with server-side output
  is_ui <- missing(rc)  

  out <- list()  # Initialize out at the beginning
  
  if (UU::is_legit(x)) {
    if (rlang::is_list(x)) {
      .x <- x
    } else {
      .x <- list(x)
    }
    
    if (is_ui) {
      out <- list()  # Initialize an empty list for UI elements
    } else {
      out <- rc$output  # Use the reactive context's output for server-side output
    }

    for (i in seq_along(.x)) {
      # Create headers if names of the list are valid
      if (UU::is_legit(names(.x))) {
        out[[paste0("header", i)]] <- h4(names(.x[i]))  # Add a header for each item
      }
      
      # Prepare arguments for the function to call (for UI or server-side handling)
      .args <- list(
        purrr::when(is_ui, ~ env$ns(paste0(outputId, i)), ~ .x[[i]]),  # Either generate UI ID or pass the data
        ...
      )
      
      # Call the output function `fn` and store the result in `out`
      out[[paste0(outputId, i)]] <- do.call(fn, .args, envir = env)
    }
  }
  
  # If generating UI, return the UI elements as a tag list; otherwise, return `out` (server-side)
  if (is_ui) {
    return(do.call(tagList, out))
  } else {
    return(out)
  }
}


#' @title Iterative generation of icons
#'
#' @inheritParams shiny::icon
#'
#' @return
#' @export

ui_icons <- function(name, class = NULL, lib = "font-awesome", ...) {
  tibble::tibble(name = name, class = class, lib = lib, ...) |> 
  purrr::pmap(~do.call(shiny::icon, list(...)))
}

simpleCard <- function(..., style = NULL, width = 4) {
  shiny::tags$div(class = glue::glue("col-12 col-md-{width}"),
                  shiny::tags$div(class = "card",
                                  if (!is.null(style))
                                    style = style,
                                  shiny::tags$div(class = "card-body",
                                                  ...))
                  )
  
}

icons = list(vet_active = ui_icons(name = c("check", "times",
                                          "question-circle", "exclamation-triangle"),
                                   style = glue::glue_data(list(color = c("teal", "tomato", "grey", "goldenrod"), fontsize = "150%"), "color: {color}; font-size: {fontsize}")) |> rlang::set_names(c("pass", "fail", "unknown", "alert")))


#' @title Create a bootstrap 4 Alert box
#' 
#' @param style \code{(character)} Inline style parameters to add
#' @inherit bs4Dash::bs4Card params return
#' @export

bs4Alert <- function(..., status = "primary", style = NULL, id = NULL, width = 6) {
  bs4Dash:::validateStatus(status)
  status <- UU::match_letters(status, n = 2, bs4Dash:::validStatuses)
  
   shiny::tags$div(class = paste0("alert alert-",status), role = "alert", ..., style = paste0("margin: 6px 5px 6px 15px;", ifelse(grepl(";$", style), style, paste0(style, ";"))), id = id)
}
