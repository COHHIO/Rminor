if (nrow(notUnsheltered) > 0) {
  plotly::plot_ly(
    notUnsheltered,
    x = ~ FriendlyProjectName,
    y = ~ Percent,
    text = ~ hover,
    hoverinfo = 'text',
    type = "bar"
  ) %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Exited to Temporary or Permanent Housing",
                   tickformat = "%"),
      title = list(text = title,
                   font = list(size = 15)),
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
      title = "Obtaining and Maintaining Permanent Housing"
    )
  
}
else{
  NULL
}