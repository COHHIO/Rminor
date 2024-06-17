#' @title qpr_datatable
#' @description Function to render datatable from default template for QPR tabitems
#' @param .data \code{(any)} Data to be passed in and used subsequent arguments
#' @param .replace \code{(logical)} whether to replace the default arguments with those supplied and eliminate the default arguments, or to replace existing defaults & and add additional args specified
#' @inheritParams DT::datatable
#' @inheritDotParams DT::datatable
#' @param ... \code{(named arguments)} passed on to \link[DT]{datatable}
#' @export


qpr_datatable <- function(.data,
                          .replace = FALSE,
                          caption = NULL,
                          rownames = FALSE,
                          filter = 'top',
                          options = list(dom = 'ltpi'),
                          ...
) {
  # Get default args
  .dt_opts <- rlang::fn_fmls()
  # Remove . args
  .dt_opts <- .dt_opts[!grepl("^\\.", names(.dt_opts))]
  # Get user supplied args
  .user <- rlang::call_args(match.call())
  .user <- append(.user[!grepl("^\\.", names(.user))], rlang::dots_list(...))
  
  if (.replace) {
    .dt_opts <- .user
  } else if (!identical(.dt_opts, .user) && !rlang::is_empty(.user)) {
    .dt_opts <- purrr::list_modify(.dt_opts, !!!.user)
  }
  # replace data call with actual data
  .dt_opts$data <- .data
  #evaluate each item so calls are not passed in.
  for (i in which(purrr::map_lgl(.dt_opts, is.call))) {
    .dt_opts[[i]] <- eval(.dt_opts[[i]])
  }
  rlang::exec(DT::datatable, !!!.dt_opts)
}


#' @title qpr_infobox
#' @description Function to render infobox from default template for QPR tabitems
#' @inheritParams qpr_datatable
#' @inheritParams bs4Dash::bs4InfoBox
#' @inheritDotParams bs4Dash::infoBox
#' @param .replace \code{(logical)} whether to replace the default arguments with those supplied and eliminate the default arguments, or to replace existing defaults & and add additional args specified
#' @param ... \code{(named arguments)} passed on to \link[bs4Dash]{infoBox}
#' @export

qpr_infobox <- function(.data,
                        .replace = FALSE,
                        title = "Average Score",
                        color = "purple",
                        value = .data$AvgScore,
                        icon = shiny::icon("shopping-cart"),
                        subtitle = "See table below for detail.",
                        ...
) {
  
  .ib_opts <- rlang::fn_fmls()
  .ib_opts <- .ib_opts[!grepl("^\\.", names(.ib_opts))]
  .user <- rlang::call_args(match.call())
  .user <- append(.user[!grepl("^\\.", names(.user))], rlang::dots_list(..., .named = TRUE))
  if (inherits(.user$icon, "character")) .user$icon <- shiny::icon(.user$icon)
  if (.replace) {
    .ib_opts <- .user
  } else if (!identical(.ib_opts, .user) && !rlang::is_empty(.user)) {
    .ib_opts <- purrr::list_modify(.ib_opts, !!!.user)
  }  
  
  # Evaluate all calls in this environment (purrr::map does not work)
  for (i in which(purrr::map_lgl(.ib_opts, is.call))) {
    .ib_opts[[i]] <- eval(.ib_opts[[i]])
  }
  
  rlang::exec(bs4Dash::bs4InfoBox, !!!.ib_opts)
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
#' @importFrom purrr map2 list_modify
#' @importFrom rlang dots_list
#' @export
qpr_plotly <- function(.d,
                       title,
                       x_col = "clients",
                       xaxis_title = "",
                       y_col = "Average",
                       yaxis_title = "Measure",
                       y_label = "Average Length of Stay",
                       hover_text = NULL,
                       mode = 'markers',
                       project_type = NULL,
                       rect_above_line = TRUE,
                       percent_format = FALSE,
                       goals = list(),
                       ...) {
  # If no data return no graph
  if (nrow(.d) < 1) return(NULL)
  
  # Ensure the lengths of the x and y columns match
  if (length(.d[[x_col]]) != length(.d[[y_col]])) {
    stop("The lengths of the x and y columns do not match.")
  }
  
  # Generate hover text if not provided
  if (is.null(hover_text)) {
    if (percent_format) {
      hover_text <- paste0('Program: ', .d$ProjectName, 
                           '<br>', y_label, ': ', scales::percent(.d[[y_col]], accuracy = 0.1), 
                           '<br>Clients: ', .d[[x_col]])
    } else {
      hover_text <- paste0('Program: ', .d$ProjectName, 
                           '<br>', y_label, ': ', .d[[y_col]], 
                           '<br>Clients: ', .d[[x_col]])
    }
  }
  
  # Create the plot
  .p <- plotly::plot_ly(
    .d, 
    x = ~ .d[[x_col]], 
    y = ~ .d[[y_col]], 
    type = 'scatter', 
    mode = mode,
    text = hover_text,
    hoverinfo = 'text',
    ...
  )
  
    
    hline <- function(y = 0, color = "red") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dash")
      )
    }
    
    # Function to create colored rectangle
    rect <- function(y0 = 0, y1 = 0, color = "red", opacity = 0.2, rect_above_line = TRUE) {
      if (rect_above_line) {
        list(
          type = "rect",
          x0 = 0,
          x1 = max(.d[[x_col]]) * 1.1,
          y0 = y1,
          y1 = max(.d[[y_col]]) * 1.1,
          fillcolor = color,
          line = list(color = color),
          opacity = opacity
        )
      } else {
        list(
          type = "rect",
          x0 = 0,
          x1 = max(.d[[x_col]]),
          y0 = 0,
          y1 = y1,
          fillcolor = color,
          line = list(color = color),
          opacity = opacity
        )
      }
    }
    
    # Add goal lines if provided
    if (!is.null(project_type) && !is.null(goals) && project_type %in% names(goals)) {
      goal <- goals[[project_type]]
      # Customize the layout
      .p <- .p %>% plotly::layout(
        title = title,
        xaxis = list(title = xaxis_title),
        yaxis = list(title = yaxis_title, tickformat = if (percent_format) '.0%' else NULL),
        shapes = list(hline(y = goal), 
                      rect(y1 = goal, rect_above_line = rect_above_line))
      )
    }
  
  return(.p)
}


