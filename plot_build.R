load("data/Utilization.RData")

# Bed Utilization Plot ----------------------------------------------------

bedPlot <- BedUtilization %>% select(-ReportingPeriod) %>%
  gather("Month",
         "Utilization",-ProjectID,-ProjectName,-ProjectType) %>%
  filter(
      ProjectName == "Sandusky - GLCAP - Homenet Permanent Housing - PSH"
  ) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

ggplot(bedPlot,
       aes(x = Month, y = Utilization, group = 1)) +
   geom_line()

# Unit Utilization Plot ---------------------------------------------------

unitPlot <- UnitUtilization %>% select(-ReportingPeriod) %>%
  gather("Month",
         "Utilization",
         -ProjectID, -ProjectName, -ProjectType) %>%
  filter(ProjectName == "Sandusky - GLCAP - Homenet Permanent Housing - PSH"
  ) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

ggplot(unitPlot,
       aes(x = Month, y = Utilization, group = 1)) +
  geom_line()
