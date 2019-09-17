#COHHIO_HMIS
#Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published
#by the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

tagList(
  dashboardPage(
    skin = "black",
    dashboardHeader(title = "R minor"),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenuid",
        menuItem("Provider Dashboard",
                 tabName = "providerDashboardTab"),
        # menuItem("CoC Competition",
        #          tabName = "cocCompetitionTab"),
        menuItem("Bed and Unit Utilization",
                      tabName = "utilizationTab"),
        menuItem(
          "Performance and Outcomes",
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
          # menuSubItem("Recurrence",
          #             tabName = "recurrenceTab"),
          menuSubItem("Rapid Placement for RRH",
                      tabName = "rapidTab")#,
          # menuSubItem("RRH HP Spending",
          #             tabName = "spendingTab")
        )
      ),
      HTML(paste0(
        "<br>&emsp;Last update:&emsp;",
        format(update_date, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")#,
        #      "<br>&emsp;Happy Passover and Easter and Spring Equinox!"
      ))
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "providerDashboardTab",
          pickerInput(
            inputId = "providerList",
            choices = providers,
            options = list(`live-search` = TRUE),
            width = "100%",
            selected = sample(providers, 1)
          ),
          uiOutput("CurrentClientCount"),
          uiOutput("CurrentHHCount"),          
          uiOutput("currentUnitUtilization"),
          uiOutput("currentBedUtilization"),
          uiOutput("veteranEngagement"),
          uiOutput("ShelterExitsToRRH"),
          uiOutput("CurrentlyAwaitingPH")
        ), 
        tabItem(
          tabName = "utilizationTab",
          fluidRow(box(htmlOutput("headerUtilization"), width = 12)),
          fluidRow(box(
            pickerInput(
              inputId = "providerListUtilization",
              choices = c(sort(BedUtilization$ProjectName)),
              options = list(`live-search` = TRUE),
              width = "100%"
            ),
            airDatepickerInput(
              inputId = "utilizationDate",
              label = "Report End Month",
              max =
                ymd(floor_date(update_date, unit = "month") - days(1)),
              min =
                ymd(floor_date(update_date - days(335), unit = "month")),
              dateFormat = "MM yyyy",
              view = "month",
              value =
                ymd(floor_date(update_date, unit = "month") - days(1)),
              minView = "months",
              addon = "none",
              autoClose = TRUE
            ),
            width = 12
          )), 
          # verbatimTextOutput("res"),
          plotlyOutput("bedPlot"),
          br(),
          fluidRow(box(
            uiOutput("bedNote"),
            title = "What is Bed Utilization?",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            uiOutput("unitNote"),
            title = "What is Unit Utilization?",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            uiOutput("utilizationNote"),
            title = "Methodology",
            collapsible = TRUE,
            collapsed = TRUE
          ))
        ),
        # tabItem(tabName = "cocCompetitionTab",
        #         HTML("<h1>Under Construction</h1>")),
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
            selected = sample(choices_regions, 1)
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
        tabItem(tabName = "PHTab",
                fluidRow(box(htmlOutput("headerQPRExitsToPH"), width = 12)),
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
                  selected = sample(choices_regions, 1)
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
        tabItem(tabName = "NCBTab",
                fluidRow(box(htmlOutput("headerQPRNCBs"), width = 12)),
                setSliderColor("#56B4E9", 1),
                sliderTextInput("QPRNCBDateSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4),
                pickerInput(
                  inputId = "QPRNCBRegionSelect",
                  "Select Region(s)",
                  choices = choices_regions,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = sample(choices_regions, 1)
                ),
                prettyRadioButtons(
                  inputId = "radioQPR_NCB_PTC",
                  label = "Program Type",
                  thick = TRUE,
                  animation = "pulse",
                  status = "info",
                  choices = c("Emergency Shelters", "Transitional Housing",
                              "Safe Haven", "Prevention", "Rapid Rehousing", 
                              "Permanent Supportive Housing", "Street Outreach"),
                  selected = "Emergency Shelters"
                ),
                plotlyOutput("QPRNCBs"),
                br()
        ),
        tabItem(tabName = "HITab",
                fluidRow(box(htmlOutput("headerQPRHI"), width = 12)),
                setSliderColor("#56B4E9", 1),
                sliderTextInput("QPRHIDateSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4),
                pickerInput(
                  inputId = "QPRHIRegionSelect",
                  "Select Region(s)",
                  choices = choices_regions,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = sample(choices_regions, 1)
                ),
                prettyRadioButtons(
                  inputId = "radioQPR_HI_PTC",
                  label = "Program Type",
                  thick = TRUE,
                  animation = "pulse",
                  status = "info",
                  choices = c("Emergency Shelters", "Transitional Housing",
                              "Safe Haven", "Prevention", "Rapid Rehousing", 
                              "Permanent Supportive Housing", "Street Outreach"),
                  selected = "Emergency Shelters"
                ),
                plotlyOutput("QPRHIs"),
                br()),
        tabItem(tabName = "incomeTab",
                fluidRow(box(htmlOutput("headerQPRIncome"), width = 12)),
                setSliderColor("#56B4E9", 1),
                sliderTextInput("QPRIncomeDateSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4),
                pickerInput(
                  inputId = "QPRIncomeRegionSelect",
                  "Select Region(s)",
                  choices = choices_regions,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = sample(choices_regions, 1)
                ),
                prettyRadioButtons(
                  inputId = "radioQPR_Income_PTC",
                  label = "Program Type",
                  thick = TRUE,
                  animation = "pulse",
                  status = "info",
                  choices = c("Emergency Shelters", "Transitional Housing",
                              "Safe Haven", "Prevention", "Rapid Rehousing", 
                              "Permanent Supportive Housing", "Street Outreach"),
                  selected = "Emergency Shelters"
                ),
                plotlyOutput("QPRIncome"),
                br()),
        # tabItem(tabName = "recurrenceTab",
                # HTML("<h1>Under Construction</h1>")),
        tabItem(tabName = "rapidTab",
                setSliderColor("#56B4E9", 1),
                sliderTextInput("RapidRRHDateSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4),
                pickerInput(
                  inputId = "RapidRRHRegion",
                  "Select Region(s)",
                  choices = choices_regions,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = sample(choices_regions, 1)
                ),
                plotlyOutput("DaysToHouse")),
        # tabItem(tabName = "spendingTab",
        #         HTML("<h1>Under Construction</h1>")),
        
        tabItem(
          tabName = "spdatTab",
          fluidRow(box(htmlOutput("headerQPRCommunityNeed"), width = 12)),
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
          fluidRow(box(
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
          ))
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
