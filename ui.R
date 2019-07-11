tagList(
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
        menuItem(
          "Performance and Outcomes",
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
          infoBoxOutput("currentUnitUtilization"),
          infoBoxOutput("currentBedUtilization"),
          infoBoxOutput("veteranEngagement")
        ),
        # tabItem providerDashboard
        tabItem(tabName = "cocCompetitionTab"),
        tabItem(
          tabName = "LoSTab",
          setSliderColor("#56B4E9", 1),
          sliderTextInput("LoSSlider",
                          "",
                          c(
                            unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                          ),
                          selected = Sys.yearqtr() - 1 / 4),
          prettyRadioButtons(
            inputId = "radioLoSPTC",
            label = "Program Type",
            thick = TRUE,
            animation = "pulse",
            status = "info",
            choices = c("Emergency Shelters", "Transitional Housing",
                        "Safe Haven", "Rapid Rehousing"),
            selected = "Emergency Shelters"
          ),
          pickerInput(
            inputId = "LoSRegionSelect",
            "Select Region(s)",
            choices = choices_regions,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = "Homeless Planning Region 6"
          ),
          prettyRadioButtons(
            inputId = "radioAvgMeanLoS",
            label = "",
            thick = TRUE,
            animation = "pulse",
            status = "info",
            choices = c("Average Days", "Median Days"),
            selected = "Average Days"
          ),
          # verbatimTextOutput("res"),
          plotlyOutput("QPRLoSPlot")
        ),
        # tabItem LengthOfStay LoS
        tabItem(tabName = "PHTab",
                setSliderColor("#56B4E9", 1),
                sliderTextInput("ExitsToPHSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4),
                pickerInput(
                  inputId = "ExitsToPHRegionSelect",
                  "Select Region(s)",
                  choices = choices_regions,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = "Homeless Planning Region 6"
                ),
                prettyRadioButtons(
                  inputId = "radioExitsToPHPTC",
                  label = "Program Type",
                  thick = TRUE,
                  animation = "pulse",
                  status = "info",
                  choices = c("Emergency Shelters", "Transitional Housing",
                              "Safe Haven", "Prevention", "Rapid Rehousing", 
                              "Permanent Supportive Housing", "Street Outreach"),
                  selected = "Emergency Shelters"
                ),
                plotlyOutput("ExitsToPH"),
                br(),
                br(),
                plotlyOutput("ExitsToPHOutreach")
        ),
        tabItem(tabName = "NCBTab"),
        tabItem(tabName = "HITab"),
        tabItem(tabName = "incomeTab"),
        tabItem(tabName = "recurrenceTab"),
        tabItem(tabName = "rapidTab"),
        tabItem(tabName = "spendingTab"),
        tabItem(
          tabName = "utilizationTab",
          pickerInput(
            inputId = "providerListUtilization",
            choices = c(sort(BedUtilization$ProjectName)),
            options = list(`live-search` = TRUE),
            width = "100%"
          ),
          setSliderColor("#56B4E9", 1),
          sliderTextInput(
            "utilizationSlider",
            label = "Select END DATE",
            choices = choices_month,
            selected = choices_month[24]
          ),
          # verbatimTextOutput("res"),
          # sliderInput(
          plotOutput("bedPlot")
        ),
        # tabItem utilizationTab
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
          sliderTextInput("spdatSlider",
                          "",
                          c(
                            unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                          ),
                          selected = Sys.yearqtr() - 1 / 4),
          plotOutput("SPDATScoresByCounty"),
          HTML("<br>"),
          box(
            textOutput("CountyScoresText"),
            title = "The Lines",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            textOutput("HHsServedScoresText"),
            title = "The Triangles",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            textOutput("NoteToUsers"),
            title = "A Note about Data Quality",
            collapsible = TRUE,
            collapsed = TRUE
          )
        ) #tabItem SPDAT tab
      ) # tabItems
    )
  ),
  tags$footer(
    HTML(
      "<p>R minor is created and maintained by the Coalition on Homelessness and
  Housing in Ohio (COHHIO)'s HMIS team.
  <p>R Core Team (2019). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria. URL
  https://www.R-project.org/.
  <p>Hadley Wickham (2017). tidyverse: Easily Install and Load the
  'Tidyverse'. R package version 1.2.1.
  https://CRAN.R-project.org/package=tidyverse
  <p>Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson
  (2019). shiny: Web Application Framework for R. R package version
  1.3.2. https://CRAN.R-project.org/package=shiny and shinydashboard: Create
  Dashboards with 'Shiny'. R package version 0.7.1.
  https://CRAN.R-project.org/package=shinydashboard"
    ),
    style = "width:100%; color: white; padding: 10px; background-color: black"
  )
)
