plotly::plot_ly(
  summaryDays,
  x = ~ FriendlyProjectName,
  y = ~ AvgDays,
  text = ~ hover,
  hoverinfo = 'text',
  type = "bar"
) %>%
  plotly::layout(
    xaxis = list(title = ~ FriendlyProjectName),
    yaxis = list(title = "Average Days to House"),
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
      y1 = 0,
      opacity = .2
    ),
    title = "Days to House"
  )