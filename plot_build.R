library(tidyverse)
library(lubridate)
library(plotly)

ReportStart <- "01012019"
ReportEnd <- "03312019"

load("data/QPR_EEs.RData")

LoSGoals <- goals %>%
  select(-Measure) %>%
  filter(SummaryMeasure == "Length of Stay" &
           !is.na(Goal)) %>%
  unique()

LoSDetail <- QPR_EEs %>%
  filter((((!is.na(MoveInDateAdjust) &
              ProjectType %in% c(13)) |
             (ProjectType %in% c(1, 2, 8)) &
             !is.na(ExitDate)
  )) &
    exited_between(., ReportStart, ReportEnd)) %>%
  mutate(
    ProjectType = case_when(
      ProjectType == 1 ~ "Emergency Shelter",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 12 ~ "Homelessness Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    ),
    Region = paste("Homeless Planning Region", Region)
  ) %>%
  filter(Region == "Homeless Planning Region 5") # this filter needs
# to be here so the selection text matches the mutated data

LoSSummary <- LoSDetail %>%
  group_by(brokenProjectNames,
           FriendlyProjectName,
           Region,
           County,
           ProjectType) %>%
  summarise(Days = case_when(
    "Average Days" == "Average Days" ~ 
      as.integer(mean(DaysinProject, na.rm = TRUE)),
    "Average Days" == "Median Days" ~
      as.integer(median(DaysinProject, na.rm = TRUE))
  ))

esdata <- LoSSummary %>% filter(ProjectType == "Emergency Shelter")

es <-
  ggplot(
    esdata,
    aes(x = FriendlyProjectName)
  ) +
  ggtitle("Emergency Shelter", subtitle = "date range") +
  geom_col(aes(y = Days), fill = "#7156e9") +
  ylab("Average Length of Stay") +
  xlab("") +
  # geom_hline(yintercept = as.integer(LoSGoals %>%
  #                                      filter(ProjectType == 1) %>%
  #                                      select(Goal))) +
  # annotate(
  #   "text",
  #   x = 0.65,
  #   y = as.integer(LoSGoals %>%
  #                    filter(ProjectType == 1) %>%
  #                    select(Goal)) + 1,
  #   label = "CoC Goal"
  # ) +
theme_light() +
  theme(axis.text.x = element_text(angle = 45))

plot_ly(data = esdata,
        x = ~FriendlyProjectName,
        y = ~Days) %>%
  add_trace(type = "bar") %>%
  layout(shapes=list(type='line', 
                     xref = "x",
                     yref = "y",
                     x0= -.5,
                     # x1= 5.5,
                     y0 = as.integer(LoSGoals %>%
                       filter(ProjectType == 1) %>%
                       select(Goal)),
                     y1= as.integer(LoSGoals %>%
                       filter(ProjectType == 1) %>%
                       select(Goal)), 
                     line=list(width=1)
                     ),
         title = 'Emergency Shelters',
         yaxis = list(title = "Average Days", showgrid = TRUE),
         xaxis = list(title = "Project Name", showgrid = TRUE))
