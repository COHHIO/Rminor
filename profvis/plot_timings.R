timings <- readRDS("profvis/tictoc.rds")
timings %>%
  dplyr::group_by(Message) %>% 
  dplyr::summarise(Duration = mean(Duration)) %>% 
  dplyr::filter(Duration > 1) %>%
  dplyr::arrange(dplyr::desc(Duration)) %>% 
  dplyr::mutate_at(dplyr::vars(Message), ~stringr::str_remove(., "lines")) %>% 
  ggplot2::ggplot(mapping = ggplot2::aes(x = reorder(Message, order(Duration, decreasing = TRUE)))) +
  ggplot2::geom_col(ggplot2::aes(y = Duration), fill = "orange", alpha = .3) +
  ggplot2::scale_y_continuous(breaks = seq(min(timings$Duration), max(timings$Duration), by = .5), minor_breaks = seq(min(timings$Duration), max(timings$Duration), by = .25)) +
  ggplot2::labs(title = "Rminor Timings",
    subtitle = "",
    caption = "",
    x = "Lines", 
    y = "Duration (s)") +
  ggplot2::theme(
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.text.x = element_text(angle = 45))
