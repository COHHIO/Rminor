library(tidyverse)
library(lubridate)
library(plotly)
library(viridis)

ReportStart <- "20180101"
ReportEnd <- "20181231"
ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))

Provider <- "Warren - Warren MHA - Transitions - TH"

load("data/Utilization.RData")

bedPlot <- BedUtilization %>% select(-FilePeriod) %>%
  gather("Month",
         "Utilization",
         -ProjectID,
         -ProjectName,
         -ProjectType) %>%
  filter(ProjectName == Provider,
         mdy(Month) %within% ReportingPeriod) %>%
  mutate(
    Month = floor_date(mdy(Month), unit = "month"),
    Bed = Utilization,
    Utilization = NULL
  )

unitPlot <- UnitUtilization %>% select(-FilePeriod) %>%
  gather("Month",
         "Utilization",
         -ProjectID,
         -ProjectName,
         -ProjectType) %>%
  filter(ProjectName == Provider,
         mdy(Month) %within% ReportingPeriod) %>%
  mutate(
    Month = floor_date(mdy(Month), unit = "month"),
    Unit = Utilization,
    Utilization = NULL
  )

utilizationPlot <- unitPlot %>%
  full_join(bedPlot,
            by = c("ProjectID", "ProjectName", "ProjectType", "Month")) 

plot_ly(utilizationPlot, 
        x = ~Month) %>%
  add_trace(y = ~ Unit,
            name = "Unit Utilization",
            type = "scatter",
            mode = "lines+markers",
            hoverinfo = 'y') %>%
  add_trace(y = ~Bed,
            name = "Bed Utilization",
            type = "scatter",
            mode = "lines+markers",
            hoverinfo = 'y') %>%
  layout(yaxis = list(
    title = "Utilization",
    tickformat = "%",
    range = c(0, 2)
  ),
  title = paste("Bed and Unit Utilization,",
                Provider,
                "\n", 
                format(ymd(ReportStart), "%B %Y"), 
                "to", 
                format(ymd(ReportEnd), "%B %Y")))




ggplot(utilizationPlot,
       aes(x = Month,
           y = Utilization,
           color = UtilizationType)) +
  theme_light() +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 2),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(
    date_labels = "%B %Y",
    date_breaks = "3 months",
    minor_breaks = "1 month"
  ) +
  scale_colour_manual(values = c("#56B4E9", "#6be956")) +
  labs(
    title = Provider,
    subtitle = paste(
      "Date Range:",
      format.Date(ymd(ReportStart), "%b %Y"),
      "to",
      format.Date(ymd(ReportEnd), "%b %Y")
    ),
    caption = "Client and household enrollment data comes from the Ohio
          Balance of State CoC HMIS. This visualization was created by the
          COHHIO HMIS team."
  )