# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

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
        # menuItem("Coordinated Entry Access Points",
        #          tabName = "ceAPs"),
        menuItem("Covid-19 Analysis",
                 tabName = "covid19Tab"),
        menuItem("Bed and Unit Utilization",
                 tabName = "utilizationTab"),
        menuItem("Quarterly Performance Report",
          menuSubItem("System Performance Measures",
                      tabName = "SPMs"),
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
          menuSubItem("Rapid Placement for RRH",
                      tabName = "rapidTab"),
          menuSubItem("RRH vs HP Spending",
                      tabName = "spendingTab")
        ),
        menuItem("About",
                 tabName = "aboutTab")
      ),
      HTML(paste0(
        "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
        format(update_date, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")
        ,
              "<p><p>&emsp;Be well, take care of each other."
      ))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "providerDashboardTab",
                fluidRow(
                  box(
                    pickerInput(
                      inputId = "providerList",
                      choices = provider_dash_choices,
                      options = pickerOptions(dropupAuto = FALSE,
                                              liveSearch = TRUE),
                      width = "100%",
                      selected = sample_n(provider_dash_selected, 1)
                    ),
                    uiOutput("CurrentClientCount"),
                    uiOutput("CurrentHHCount"),
                    uiOutput("currentUnitUtilization"),
                    uiOutput("currentBedUtilization"),
                    uiOutput("veteranEngagement"),
                    uiOutput("TAYEngagement"),
                    uiOutput("ShelterExitsToRRH"),
                    uiOutput("CurrentlyAwaitingPH")
                  )
                )), 
        tabItem(
          tabName = "ceAPs",
          tabsetPanel(
            type = "tabs",
            tabPanel("By County", 
                     fluidRow(box(pickerInput(
                       inputId = "ap_by_county",
                       label = "Select County/-ies",
                       options = pickerOptions(dropupAuto = FALSE,
                                               actionsBox = TRUE),
                       choices = bos_counties,
                       multiple = TRUE
                     ))),
                     fluidRow(box(
                       title = "Coordinated Entry Access Points",
                       width = 12,
                       dataTableOutput("AP_list_county")
                       ))
                     ),
            tabPanel("By Service Area", 
                     fluidRow(box(pickerInput(
                       inputId = "ap_by_region",
                       label = "Select Service Area",
                       options = pickerOptions(dropupAuto = FALSE,
                                               actionsBox = TRUE),
                       choices = choices_service_areas,
                       multiple = TRUE)
                     )),
                     fluidRow(box(
                       title = "Coordinated Entry Access Points",
                       width = 12,
                       dataTableOutput("AP_list_region"))
                     )),
            tabPanel("By Organization", 
                     fluidRow(box(
                       pickerInput(
                         inputId = "ap_by_org",
                         label = "Select Organization",
                         selected = NULL,
                         options = pickerOptions(dropupAuto = FALSE,
                                                 actionsBox = TRUE),
                         multiple = TRUE,
                         choices = APs %>%
                           arrange(ProjectAKA) %>%
                           pull(ProjectAKA) %>%
                           unique()
                       )
                     )), 
                     fluidRow(box(
                       title = "Coordinated Entry Access Points",
                       width = 12,
                       dataTableOutput("AP_list_org")))
                     )
          ),
          fluidRow(box(
            title = "Ohio Balance of State CoC Homeless Planning Regions",
            HTML("The solid-colored counties are all part of the Ohio
                       Balance of State CoC. The Ohio Development Services 
                       Agency (ODSA) further divided the counties in the Balance
                       of State into 17 Homeless Planning Regions to make
                       implementation of state-funded programs in the Balance of
                       State more localized."),
            img(
              src =
                "Homeless-Region-Map-for-COHHIO-2017.png",
              height = '100%',
              width = '100%'
            ),
            width = 12
          )),
        ),
        tabItem(
          tabName = "covid19Tab",
          fluidRow(box(htmlOutput("headerCovid19"), width = 12)),
          fluidRow(box(
            uiOutput("covidText"),
            title = "Covid-19 Data Collection",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12
          )),
          fluidRow(box(plotOutput("covidStatus"),
                       title = "Covid-19 Status at Last Screening",
                       width = 6),
                   box(plotOutput("covidPrioritization"),
                       title = "Prioritization Category at Coordinated Entry",
                       width = 6))
        ),
        tabItem(
          tabName = "utilizationTab",
          fluidRow(box(htmlOutput("headerUtilization"), width = 12)),
          fluidRow(
            box(
              title = "NOTICE",
              status = "warning",
              solidHeader = TRUE,
              "During this time, congregate facilities should be aiming to deconcentrate. If this causes fluctuations in Utilization, that is okay. Please continue to keep your clients safe."
              ,
              width = 6
            )
          ), 
          fluidRow(box(
            pickerInput(
              inputId = "providerListUtilization",
              choices = c(sort(utilization_bed$ProjectName)),
              options = pickerOptions(dropupAuto = FALSE,
                                      liveSearch = TRUE),   
              selected = sample_n(utilization_bed %>% select(ProjectName), 1),
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
              autoClose = TRUE,
              width = '25%'
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
        #         fluidRow(box(
        #           htmlOutput("headerCoCCompetitionProjectLevel"),
        #           width = 12
        #         )),
        #         fluidRow(box(
        #           pickerInput(
        #             inputId = "pe_provider",
        #             label = "Select your CoC-funded Provider",
        #             choices = sort(pe_validation_summary$AltProjectName) %>%
        #               unique(),
        #             selected = sample(pe_validation_summary$AltProjectName, 1),
        #             options = list('live-search' = TRUE),
        #             width = "100%"
        #           ),
        #           width = 12
        #         )),
        #         fluidRow(
        #           box(
        #             DT::dataTableOutput("pe_ProjectSummary"),
        #             width = 12,
        #             title = "Score Summary",
        #             status = "info",
        #             solidHeader = TRUE,
        #             collapsible = TRUE
        #           )
        #         )),
        tabItem(
          tabName = "SPMs",
          fluidRow(box(htmlOutput("headerSPMs"), width = 12)),
          fluidRow(box(
            DT::dataTableOutput("spmLoTH"),
            title = "Metric 1b: Length of Time Homeless",
            footer = HTML("Persons in ES, SH, TH, RRH, and PSH.<br><br>
            CoC goal = no more than 90 days average and median"),
            width = 12
          )),
          fluidRow(box(
            DT::dataTableOutput("spmRecurrence"),
            title = "Metrics 2a1 & 2b1: Clients Returning to Homelessness After Successful Placement",
            footer = HTML("Persons in ES, SH, TH, Outreach, RRH, and PSH.<br><br>
            6 month goal = <10%, 24 month goal = <20%"),
            width = 12
          )),
          fluidRow(box(
            DT::dataTableOutput("spmPIT"),
            title = "Metric 3.1: January 2019 and January 2020 PIT Counts",
            footer = HTML("Total and Sheltered goals: reduce by 4% annually. <br>
            Veteran and Chronic goals: reduce by 10% annually."),
            width = 12
          )),
          fluidRow(box(
            DT::dataTableOutput("spmExitsToPH"),
            title = "Metrics 7b1 & 7b2: Exits to or Retention of Permanent Housing",
            footer = HTML("ES, SH, TH, RRH Goal: 75%<br>PSH Goal: 90%"),
            width = 12
          ))
        ),
        tabItem(
          tabName = "LoSTab",
          fluidRow(box(htmlOutput("headerLengthOfStay"), width = 12)),
          chooseSliderSkin("Round"),
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
            options = pickerOptions(dropupAuto = FALSE,
                                    actionsBox = TRUE),
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
                chooseSliderSkin("Round"),
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
                  options = pickerOptions(dropupAuto = FALSE,
                                          actionsBox = TRUE),
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
                chooseSliderSkin("Round"),
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
                  options = pickerOptions(dropupAuto = FALSE,
                                          actionsBox = TRUE),
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
                chooseSliderSkin("Round"),
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
                  options = pickerOptions(dropupAuto = FALSE,
                                          actionsBox = TRUE),
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
                chooseSliderSkin("Round"),
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
                  options = pickerOptions(dropupAuto = FALSE,
                                          actionsBox = TRUE),
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
        tabItem(tabName = "rapidTab",
                fluidRow(box(htmlOutput("headerRRHRapidPlacement"), width = 12)),
                chooseSliderSkin("Round"),
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
                  options = pickerOptions(dropupAuto = FALSE,
                                          actionsBox = TRUE),
                  multiple = TRUE,
                  selected = sample(choices_regions, 1)
                ),
                plotlyOutput("DaysToHouse")),
        tabItem(tabName = "spendingTab",
                fluidRow(box(htmlOutput("headerRRHSpending"), width = 12)),
                chooseSliderSkin("Round"),
                setSliderColor("#56B4E9", 1),
                sliderTextInput("RRHSpendingDateSlider",
                                "",
                                c(
                                  unique(Sys.yearqtr() - 6 / 4:Sys.yearqtr() + 1 / 4)
                                ),
                                selected = Sys.yearqtr() - 1 / 4),
                pickerInput(
                  inputId = "RRHRegion",
                  "Select Region(s)",
                  choices = choices_regions,
                  options = pickerOptions(dropupAuto = FALSE,
                                          actionsBox = TRUE),
                  multiple = TRUE,
                  selected = sample(choices_regions, 1)
                ),
                plotlyOutput("RRHSpending")
                ),
        tabItem(
          tabName = "spdatTab",
          fluidRow(box(htmlOutput("headerQPRCommunityNeed"), width = 12)),
          pickerInput(
            inputId = "regionList",
            choices = choices_regions,
            options = pickerOptions(dropupAuto = FALSE,
                                    liveSearch = TRUE),
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
        ), #tabItem SPDAT tab
        tabItem(tabName = "aboutTab",
                fluidRow(
                box(
                  title = "Ohio Balance of State CoC Homeless Planning Regions",
                  HTML("The solid-colored counties are all part of the Ohio
                       Balance of State CoC. The Ohio Development Services 
                       Agency (ODSA) further divided the counties in the Balance
                       of State into 17 Homeless Planning Regions to make
                       implementation of state-funded programs in the Balance of
                       State more localized.
                       <p> Throughout R minor, you will notice references to 
                       Homeless Planning Regions. Please consult this map if you 
                       are unsure what Region your county is in."),
                  img(
                    src =
                      "Homeless-Region-Map-for-COHHIO-2017.png",
                    height = '100%',
                    width = '100%'
                  ),
                  width = 6
                ),
                box(HTML("The Ohio Balance of State Continuum of Care (BoSCoC) 
                         represents 80 of the 88 counties in Ohio and is the
                         planning body for homeless services in the area. The Ohio 
                         Development Services Agency (ODSA) and the Coalition on 
                         Homelessness and Housing in Ohio (COHHIO) serve as the 
                         lead staffing agencies and co-chairs of the Steering 
                         Committee for the Ohio BoSCoC. ODSA serves as the Ohio 
                         BoSCoC Collaborative Applicant (submits the annual 
                         consolidated CoC Application) while COHHIO serves as 
                         the HMIS Lead Agency."),
                    title = "Ohio Balance of State CoC"),
                box(
                  HTML(
                    "<p>R minor is a free and open source project created and 
                    maintained by the HMIS team at Coalition on Homelessness 
                    and Housing in Ohio (COHHIO). Please find the code here: 
                    <a href=\"https://github.com/COHHIO/Rminor\">R minor code</a>

                    <p>This project would not exist were it not for the 
                    existence of other quality free and open source products. 
                    Following are citations for the products R minor relies on."
                  ),
                  title = "About R minor"
                ),
                box(
                  HTML(
                    "<p>R Core Team (2019). R: A language and environment for 
                    statistical computing. R Foundation for Statistical 
                    Computing, Vienna, Austria.
                    <a href=\"https://www.R-project.org/\">R programming language</a>.
                    
                    <p>Hadley Wickham (2017). tidyverse: Easily Install and Load 
                    the 'Tidyverse'. R package version 1.2.1.
                    <a href=\"https://CRAN.R-project.org/package=tidyverse\">Tidyverse package</a>
                    
                    <p>Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and 
                    Jonathan McPherson (2019). shiny: Web Application Framework 
                    for R. R package version 1.3.2. 
                    <a href=\"https://CRAN.R-project.org/package=shiny\">R Shiny package</a>
                    and shinydashboard: Create Dashboards with 'Shiny'. R 
                    package version 0.7.1.
                    <a href=\"https://CRAN.R-project.org/package=shinydashboard\">shinydashboard package</a>"
                  ),
                  title = "Citations"
                )
                )
                ) # aboutTab
      ) # tabItems
    )
  )
