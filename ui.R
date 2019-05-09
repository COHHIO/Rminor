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
                    tabName = "spdatTab"),
        menuSubItem("Length of Stay",
                    tabName = "LoSTab"),
        menuSubItem("Exits to Permanent Housing",
                    tabName = "PHTab"),
        menuSubItem("Non-Cash Benefits at Exit",
                    tabName = "NCBTab"),
        menuSubItem("Health Insurance at Exit",
                    tabName = "HITab"),
        menuSubItem("Income Growth",
                    tabName = "incomeTab"),
        menuSubItem("Recurrence",
                    tabName = "recurrenceTab"),
        menuSubItem("Rapid Placement for RRH",
                    tabName = "rapidTab"),
        menuSubItem("RRH HP Spending",
                    tabName = "spendingTab")
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
      tabItem(
        tabName = "utilizationTab",
        pickerInput(
          inputId = "providerListUtilization",
          choices = c(sort(BedUtilization$ProjectName)),
          options = list(`live-search` = TRUE),
          width = "100%"
        ),
        #             chooseSliderSkin("Round"),
        setSliderColor("#56B4E9", 1),
        sliderInput(
          "utilizationDateSlider",
          "Choose END DATE",
          min = floor_date(today() - years(2), unit = "month"),
          max = ceiling_date(today(), unit = "month") - 1,
          value = floor_date(today(), unit = "month"),
          timeFormat = "%b %Y"
        ),
        plotOutput("bedPlot")
      ), 
      tabItem(
        tabName = "spdatTab",
        pickerInput(
          inputId = "regionList",
          choices = c(unique(Regions$RegionName)),
          options = list(`live-search` = TRUE),
          width = "70%"
        ),
        chooseSliderSkin("Round"),
        setSliderColor("#56B4E9", 1),
        sliderTextInput(
          "spdatSlider",
          "",
          c(unique(Sys.yearqtr() - 6/4 : Sys.yearqtr() + 1/4)),
          selected = Sys.yearqtr() - 1/4
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
