library(tidyverse)
library(lubridate)

load("data/Utilization.RData")

# Bed Utilization Plot ----------------------------------------------------
# input$providerListUtilization
ReportStart <- ymd(20190501) - years(1)
ReportEnd <- ymd(20190531)
ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))

bedPlot <- BedUtilization %>% select(-FilePeriod) %>%
  gather("Month",
         "Utilization", -ProjectID, -ProjectName, -ProjectType) %>%
  filter(ProjectName == "Warren - Warren MHA - Transitions - THap",
         mdy(Month) %within% ReportingPeriod) %>%
  mutate(Month = floor_date(mdy(Month), unit = "month"),
         Bed = Utilization,
         Utilization = NULL) %>%
  arrange(Month)

unitPlot <- UnitUtilization %>% select(-FilePeriod) %>%
  gather("Month",
         "Utilization", -ProjectID, -ProjectName, -ProjectType) %>%
  filter(ProjectName == "Warren - Warren MHA - Transitions - THap",
         mdy(Month) %within% ReportingPeriod) %>%
  mutate(Month = floor_date(mdy(Month), unit = "month"),
         Unit = Utilization,
         Utilization = NULL) %>%
  arrange(Month)

utilizationPlot <- unitPlot %>%
  full_join(bedPlot,
            by = c("ProjectID", "ProjectName", "ProjectType", "Month")) %>%
  gather("UtilizationType",
         "Utilization",
         -ProjectID,-ProjectName,
         -ProjectType,-Month) 

ggplot(utilizationPlot,
       aes(x = Month, 
           y = Utilization, 
           color = UtilizationType)) +
  theme_light() + 
  geom_point() + 
  scale_y_continuous(limits = c(0,2),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%B %Y", date_minor_breaks = "1 month") 

