if (nrow(LoSDetail) > 0) {
  plotly::plot_ly(
    data = LoSSummary,
    x = ~ FriendlyProjectName,
    y = ~ Days,
    text = ~ hover,
    hoverinfo = 'text'
  ) %>%
    plotly::add_trace(type = "bar") %>%
    plotly::layout(
      shapes = list(
        type = "rect",
        name = "CoC Goal",
        fillcolor = "#008000",
        line = list(color = "white"),
        layer = "below",
        xref = "paper",
        yref = "y",
        x0 = 0,
        x1 = 1,
        y0 = 0,
        y1 = ~ Goal[1],
        opacity = .2
      ),
      title = list(text = title,
                   font = list(size = 15)),
      margin = list(
        l = 50,
        r = 50,
        b = 100,
        t = 100,
        pad = 4
      ),
      yaxis = list(
        title = "Days",
        rangemode = "tozero",
        showgrid = TRUE
      ),
      xaxis = list(
        title = "",
        showgrid = TRUE,
        rangemode = "tozero"
      )
    )
}
else {
  
}