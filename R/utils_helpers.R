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
                       ...) {
  # If no data return no graph
  if (nrow(.d) < 1) return(NULL)
  
  # Generate hover text if not provided
  if (is.null(hover_text)) {
    hover_text <- paste0('Program: ', .d$ProjectName, 
                         '<br>', y_label, ': ', .d[[y_col]], 
                         '<br>Clients: ', .d[[x_col]])
  }
  
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

    # Customize the layout
    .p <- .p %>% plotly::layout(
      title = title,
      xaxis = list(title = xaxis_title),
      yaxis = list(title = yaxis_title)
    )
  
  return(.p)
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
qpr_pct_plotly <- function(.d,
                       title,
                       x = ~ clients,
                       xaxis = list(title = ""),
                       y = ~ Average,
                       yaxis = list(title = "Measure"),
                       ...,
                       .sub = TRUE) {
  # If no data return no graph
  if (nrow(.d) < 1) return(NULL)
  .p <- plotly::plot_ly(.d, 
                        x = x, 
                        y = y, 
                        type = 'scatter', 
                        mode = 'markers',
                        text = ~ paste0('Program:', .d$ProjectName, 
                                        '<br>Average Length of Stay:', .d$Average, 
                                        '<br>Clients:', .d$clients),
                        hoverinfo = 'text'
  )
}

