load("data/Utilization.RData")

# Bed Utilization Plot ----------------------------------------------------

ReportStart <- ymd(20190501) - years(1)
ReportEnd <- ymd(20190501)
ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))

bedPlot <- BedUtilization %>% select(-FilePeriod) %>%
  gather("Month",
         "Utilization", -ProjectID, -ProjectName, -ProjectType) %>%
  filter(ProjectName == "Warren - Warren MHA - Transitions - THap",
         mdy(Month) %within% ReportingPeriod) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

unitPlot <- UnitUtilization %>% select(-FilePeriod) %>%
  gather("Month",
         "Utilization", -ProjectID, -ProjectName, -ProjectType) %>%
  filter(ProjectName == "Warren - Warren MHA - Transitions - THap",
         mdy(Month) %within% ReportingPeriod) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

ggplot(unitPlot,
       aes(x = Month, 
           y = Utilization, 
           group = 1, 
           color = "Unit Utilization")) +
  theme_light() + 
  geom_line() + 
  scale_y_continuous(limits = c(0,2),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%B %Y", date_minor_breaks = "1 month") +
  geom_line(data = bedPlot,
            aes(x = Month,
                y = Utilization,
                group = 1,
                color = "Bed Utilization"))
