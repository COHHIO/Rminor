#' @title Filter data.frame with default filters for DQ
#' @description Filters by `input$program` & `input$date_range`
#' @param x \code{(data.frame)} with `ProjectName`, `EntryDate`, `ExitDate` 
#' @param ... \code{(character)} Expressions passed on to \link[dplyr]{filter}
#' @param program Program ID to filter for
#' @param env \code{(environment)} The parent environment from which to retrieve input reactiveValues
#'
#' @return \code{(data.frame)} filtered accordingly
#' @export
#'
#' @examples
#' test <- data.frame(Issue = 1:5, Type = sample(c("Warning", "Error"), 5, TRUE), ProjectName = letters[1:5], EntryDate = seq.Date(lubridate::floor_date(lubridate::today() - 4, "month"), Sys.Date(), length.out = 5), ExitDate = seq.Date(lubridate::today() - 4, Sys.Date(), by = "day"))
dq_filter_between <- function(x,
  ...,
  date_range,
  program
) {
  out <- x
  if (!missing(date_range) && UU::is_legit(date_range))
    out <- out |>
      HMIS::served_between(date_range[1], date_range[2])
  if (!missing(program) && UU::is_legit(program))
    out <- dplyr::filter(out, ProjectID %in% program)
  
  
  .dots <- rlang::enquos(...)
  
  purrr::reduce(.dots, ~dplyr::filter(.x, 
      !!.y
  ), .init = out)
}


#' @title Select default display columns for Data Quality Tables
#' 
#' @param x \code{(data.frame)}
#' @param ... \code{(columns to select)} These can be unquoted or quoted.
#' @param default \code{(list)} Columns to select can also be supplied as a list. If using `...` and you wish to not select the default columns, set to `FALSE`
#'
#' @return \code{(data.frame)} with selected columns. See `default` argument for defaults that will be selected.
#' @export
#'
#' @examples
#' dq_select_cols(data.frame(UniqueID = 1:3, Issue = letters[1:3], EntryDate = 1:3, blah = 1:3), blah)
dq_select_cols <- function(x, ..., default = list("UniqueID",
                                                  "EnrollmentID",
                                                  `Entry Date` = "EntryDate",
                                                  "Type",
                                                  "Issue")) {
  
  ex <- rlang::enexprs(...)
  if (UU::is_legit(default))
    ex <- rlang::exprs(!!!ex, !!!default)
  dplyr::select(x, 
                !!!ex
                ) |> 
    dplyr::select(dplyr::matches("Unique"), dplyr::matches("Date"), dplyr::everything())
}

dq_see_guidance <- function() tags$span("See ", tags$a(href = "#dq_box_dq_summary", "Guidance below"), " for instructions on how to fix these errors.")

dq_performance <- function(.data, .join_data, groups = c("ProjectID", "ProjectName"), join = FALSE, suffix = c("_issue", "_client"), date_range = NULL, program = NULL) {
  .groups <- purrr::map(groups, rlang::sym)
  
  obs <- list(
      data = .data
  )
    
  if (!missing(.join_data))
    obs$join_data <- .join_data
    
  
  summed <- purrr::map2(obs, suffix, ~{
    # just return the data if already summarized and no filter parameters are provided
    nm = "n"
    if (join)
      nm = paste0("n", .y)
    if ("n" %in% names(.x) || !UU::is_legit(.x)) {
      out <- dplyr::rename(.x, !!nm := n)
    } else {
      dq_filter_between(.x, date_range = date_range, program = program) |> 
        dplyr::group_by(!!!.groups) |>
        dplyr::summarise(!!rlang::sym(nm) := dplyr::n(), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(!!nm))
    }
    
  })
  
  if (join){
    summed <- dplyr::left_join(summed$data, summed$join_data, by = rlang::exec(UU::common_names, !!!summed), suffix = suffix)
    .ns = purrr::map(stringr::str_subset(names(summed), "^n"), rlang::sym)
    out <- summed |> 
      dplyr::mutate(
        p = !!rlang::expr(!!.ns[[1]] / !!.ns[[2]]),
        rank = .5 - dplyr::percent_rank(p)
      )   
  } else {
    out <- summed[[1]]
  }
  out
}
