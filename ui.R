dashboardPage(
  skin = "black",
  dashboardHeader(title = "R minor"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Provider Dashboard",
               tabName = "providerDashboardTab"),
      menuItem("CoC Competition",
               tabName = "cocCompetitionTab"),
      menuItem("Performance and Outcomes",
        menuSubItem("Bed and Unit Utilization",
                    tabName = "utilizationTab"),
        menuSubItem("Community Need (by County)",
                    tabName = "spdatTab")
      )
    ),
    HTML(paste0(
      "<br>&emsp;Last update:&emsp;",
      format(updatedate, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")#,
      #      "<br>&emsp;Happy Passover and Easter and Spring Equinox!"
    ))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "providerDashboardTab",
        pickerInput(
          inputId = "providerList",
          choices = c(providerids$ProjectName),
          options = list(`live-search` = TRUE),
          width = "100%"
        ),
        infoBoxOutput("currentHHs"),
        infoBoxOutput("currentUnits"),
        infoBoxOutput("currentUnitUtilization"),
        infoBoxOutput("currentClients"),
        infoBoxOutput("currentBeds"),
        infoBoxOutput("currentBedUtilization")
      ),
      tabItem(tabName = "cocCompetitionTab"),
      tabItem(tabName = "utilizationTab",
              pickerInput(
                inputId = "providerListUtilization",
                choices = c(sort(BedUtilization$ProjectName)),
                options = list(`live-search` = TRUE),
                width = "100%"),
              plotOutput("bedPlot")),
      tabItem(
        tabName = "spdatTab",
        pickerInput(
          inputId = "regionList",
          choices = c(unique(Regions$RegionName)),
          options = list(`live-search` = TRUE),
          width = "70%"
        ),
        chooseSliderSkin("Round"),
        setSliderColor("#56B4E9", c(1, 2)),
        sliderInput(
          "y",
          "",
          year(today()) - 2,
          year(today()),
          year(today()),
          sep = "",
          ticks = FALSE
        ),
        sliderInput(
          "q", "", 1, 4,
          if_else(quarter(today()) - 1 == 0, 1,
                  quarter(today()) - 1),
          ticks = FALSE,
          pre = "Q"
        ),
        plotOutput("SPDATScoresByCounty"),
        HTML("<br>"),
        box(textOutput("CountyScoresText"), 
            title = "The Lines",
            collapsible = TRUE, 
            collapsed = TRUE),
        box(textOutput("HHsServedScoresText"), 
            title = "The Triangles",
            collapsible = TRUE, 
            collapsed = TRUE),
        box(textOutput("NoteToUsers"), 
            title = "A Note about Data Quality",
            collapsible = TRUE, 
            collapsed = TRUE)
      )
    )
  )
)
