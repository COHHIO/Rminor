load("data/Utilization.RData")

# Bed Utilization Plot ----------------------------------------------------

bedPlot <- BedUtilization %>% select(-ReportingPeriod) %>%
  gather("Month",
         "Utilization",-ProjectID,-ProjectName,-ProjectType) %>%
  filter(
      ProjectName == "Ashland - Appleseed CMHC - Targeted RRH"
  ) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

ggplot(bedPlot,
       aes(x = Month, y = Utilization, group = 1)) +
  geom_line() +
  ylab("Bed Utilization") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Unit Utilization Plot ---------------------------------------------------

unitPlot <- UnitUtilization %>% select(-ReportingPeriod) %>%
  gather("Month",
         "Utilization",
         -ProjectID, -ProjectName, -ProjectType) %>%
  filter(ProjectName == "Ashland - Appleseed CMHC - Targeted RRH") %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

ggplot(unitPlot,
       aes(x = Month, y = Utilization, group = 1)) +
  geom_line() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
