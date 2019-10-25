


ReportStart <- "01-01-2019"
ReportEnd <- "09-30-2019"

rrhSpending <- QPR_RRH_HP_Spending %>%
  filter(
    !is.na(OrganizationName) &
      Region %in% c(5) &
      entered_between(., ReportStart, ReportEnd)
  ) %>%
  mutate(ProjectType = if_else(ProjectType == 12,
                               "HP",
                               "RRH"))

# here's help on this: 
#https://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels

x <- QPR_RRH_HP_Spending %>%
  filter(
    !is.na(OrganizationName) &
      Region %in% c(5) &
      entered_between(., ReportStart, ReportEnd)
  ) %>%
  select(OrganizationName, Region) %>%
  unique() %>%
  arrange(OrganizationName)

x <- rbind(x, x) %>% arrange(OrganizationName)

y <- data.frame(ProjectType = c("HP", "RRH"))

z <- cbind(x, y)

rrhSpending <- rrhSpending %>% right_join(z, by = c("OrganizationName",
                                                    "Region",
                                                    "ProjectType")) %>%
  mutate(PersonalID = if_else(is.na(PersonalID), 4216, PersonalID),
         EntryDate = if_else(PersonalID == 4216,
                             mdy(ReportStart),
                             ymd(EntryDate)),
         MoveInDateAdjust = if_else(PersonalID == 4216,
                                    mdy(ReportStart),
                                    ymd(EntryDate)),
         ExitDate = if_else(PersonalID == 4216,
                            mdy(ReportEnd),
                            ymd(EntryDate)))


rrhSpending <- rrhSpending  %>%
  group_by(OrganizationName, Region, ProjectType) %>%
  summarise(Amount = sum(Amount),
            HHs = n()) %>%
  ungroup() %>%
  spread(ProjectType, Amount)

rrhSpending[is.na(rrhSpending)] <- 0

rrhSpending <- rrhSpending %>%
  group_by(OrganizationName, Region) %>%
  summarise(HHs = sum(HHs),
            RRH = sum(RRH),
            HP = sum(HP)) %>%
  ungroup() %>%
  filter(HP > 0 | RRH > 0) %>%
  mutate(Goal = 0.75,
         PercentRRH = RRH/(RRH + HP),
         hover = paste0(
           OrganizationName,
           "\nPercent Spent on RRH: ",
           percent(PercentRRH),
           "\nTotal Spent on RRH: $",
           RRH,
           "\nTotal Spent on Prevention: $",
           HP,
           "\nTotal Households: ",
           HHs,
           sep = "\n"
         ))

title <- paste0("Percent Spent on Rapid Rehousing\n",
                ReportStart, " to ", ReportEnd)

plot_ly(
  rrhSpending,
  x = ~ OrganizationName,
  y = ~ PercentRRH,
  text = ~ hover,
  hoverinfo = 'text',
  type = "bar"
) %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "RRH Spending",
                 tickformat = "%"),
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
      y1 = 1,
      opacity = .2
    ),
    title = "Percent Spent on RRH"
  )

