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


#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @importFrom htmltools HTML br img
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem box
#' @importFrom shinyWidgets pickerInput pickerOptions airDatepickerInput chooseSliderSkin setSliderColor sliderTextInput prettyRadioButtons
#' @importFrom dplyr sample_n select
#' @importFrom lubridate ymd floor_date days
#' @importFrom plotly plotlyOutput
#' @importFrom DT dataTableOutput
#' @importFrom zoo Sys.yearqtr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      skin = "black",
      shinydashboard::dashboardHeader(title = "R minor"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "sidebarmenuid",
          #CHANGED standardize format of tabnames to shiny compatible namespaces [NS]-* where * in this case is "Tab". These will allow submenuitems to be generated programmatically
          shinydashboard::menuItem("Provider Dashboard",
                                   tabName = "providerDashboard-Tab"),
          # menuItem("CoC Competition",
          #          tabName = "cocCompetitionTab"),
          shinydashboard::menuItem("Coordinated Entry Access Points",
                                   tabName = "ceAPs-Tab"),
          shinydashboard::menuItem("Covid-19 Analysis",
                                   tabName = "covid19-Tab"),
          shinydashboard::menuItem("Bed and Unit Utilization",
                                   tabName = "utilization-Tab"),
          shinydashboard::menuItem(
            "Quarterly Performance Report",
            
            shinydashboard::menuSubItem("System Performance Measures",
                                        tabName = "SPM-Tab"),
            shinydashboard::menuSubItem("Community Need (by County)",
                                        tabName = "spdat-Tab"),
            shinydashboard::menuSubItem("Length of Stay",
                                        tabName = "LoS-Tab"),
            shinydashboard::menuSubItem("Exits to Permanent Housing",
                                        tabName = "PH-Tab"),
            shinydashboard::menuSubItem("Non-Cash Benefits at Exit",
                                        tabName = "NCB-Tab"),
            shinydashboard::menuSubItem("Health Insurance at Exit",
                                        tabName = "HI-Tab"),
            shinydashboard::menuSubItem("Income Growth",
                                        tabName = "income-Tab"),
            shinydashboard::menuSubItem("Rapid Placement for RRH",
                                        tabName = "RRH-Tab"),
            shinydashboard::menuSubItem("RRH vs HP Spending",
                                        tabName = "spending-Tab")
          ),
          shinydashboard::menuItem("About",
                                   tabName = "about-Tab")
        ),
        htmltools::HTML(paste0(
          "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
          format(update_date, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")
          ,
          "<p><p>&emsp;Wear your mask! Be well."
        ))
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "providerDashboard-Tab",
                                  shiny::fluidRow(
                                    shinydashboard::box(
                                      shinyWidgets::pickerInput(
                                        inputId = "providerList",
                                        choices = provider_dash_choices,
                                        options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                                              liveSearch = TRUE),
                                        width = "100%",
                                        selected = dplyr::sample_n(provider_dash_selected, 1)
                                      ),
                                      shiny::uiOutput("CurrentClientCount"),
                                      shiny::uiOutput("CurrentHHCount"),
                                      shiny::uiOutput("currentUnitUtilization"),
                                      shiny::uiOutput("currentBedUtilization"),
                                      shiny::uiOutput("veteranEngagement"),
                                      shiny::uiOutput("TAYEngagement"),
                                      shiny::uiOutput("ShelterExitsToRRH"),
                                      shiny::uiOutput("CurrentlyAwaitingPH")
                                    )
                                  )), 
          mod_ceAPs_ui("ceAPs"),
          shinydashboard::tabItem(
            tabName = "covid19-Tab",
            shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerCovid19"), width = 12)),
            shiny::fluidRow(shinydashboard::box(
              shiny::uiOutput("covidText"),
              title = "Covid-19 Data Collection",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(shiny::plotOutput("covidStatus"),
                                          title = "Covid-19 Status at Last Screening",
                                          width = 6),
                            shinydashboard::box(shiny::plotOutput("covidPrioritization"),
                                          title = "Prioritization Category at Coordinated Entry",
                                          width = 6))
          ),
          shinydashboard::tabItem(
            tabName = "utilization-Tab",
            shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerUtilization"), width = 12)),
            shiny::fluidRow(
              shinydashboard::box(
                title = "NOTICE",
                status = "warning",
                solidHeader = TRUE,
                "During this time, congregate facilities should be aiming to deconcentrate. If this causes fluctuations in Utilization, that is okay. Please continue to keep your clients safe."
                ,
                width = 6
              )
            ), 
            shiny::fluidRow(shinydashboard::box(
              shinyWidgets::pickerInput(
                inputId = "providerListUtilization",
                choices = c(sort(utilization_bed$ProjectName)),
                options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                      liveSearch = TRUE),   
                selected = dplyr::sample_n(utilization_bed %>% dplyr::select(ProjectName), 1),
                width = "100%"
              ),
              shinyWidgets::airDatepickerInput(
                inputId = "utilizationDate",
                label = "Report End Month",
                max =
                  lubridate::ymd(lubridate::floor_date(update_date, unit = "month") - lubridate::days(1)),
                min =
                  lubridate::ymd(lubridate::floor_date(update_date - lubridate::days(335), unit = "month")),
                dateFormat = "MM yyyy",
                view = "month",
                value =
                  lubridate::ymd(lubridate::floor_date(update_date, unit = "month") - lubridate::days(1)),
                minView = "months",
                addon = "none",
                autoClose = TRUE,
                width = '25%'
              ),
              width = 12
            )), 
            # verbatimTextOutput("res"),
            plotly::plotlyOutput("bedPlot"),
            htmltools::br(),
            shiny::fluidRow(shinydashboard::box(
              shiny::uiOutput("bedNote"),
              title = "What is Bed Utilization?",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              shiny::uiOutput("unitNote"),
              title = "What is Unit Utilization?",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              shiny::uiOutput("utilizationNote"),
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
          shinydashboard::tabItem(
            tabName = "SPM-Tab",
            shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerSPMs"), width = 12)),
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmLoTH"),
              title = "Metric 1b: Length of Time Homeless",
              footer = htmltools::HTML("Persons in ES, SH, TH, RRH, and PSH.<br><br>
            CoC goal = no more than 90 days average and median"),
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmRecurrence"),
              title = "Metrics 2a1 & 2b1: Clients Returning to Homelessness After Successful Placement",
              footer = htmltools::HTML("Persons in ES, SH, TH, Outreach, RRH, and PSH.<br><br>
            6 month goal = <10%, 24 month goal = <20%"),
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmPIT"),
              title = "Metric 3.1: January 2019 and January 2020 PIT Counts",
              footer = htmltools::HTML("Total and Sheltered goals: reduce by 4% annually. <br>
            Veteran and Chronic goals: reduce by 10% annually."),
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmExitsToPH"),
              title = "Metrics 7b1 & 7b2: Exits to or Retention of Permanent Housing",
              footer = htmltools::HTML("ES, SH, TH, RRH Goal: 75%<br>PSH Goal: 90%"),
              width = 12
            ))
          ),
          mod_QPR_tabItem_ui("LoS"
                             , project_choices = choices_project_type[tab_choices$LoS]
                             , region_choices = choices_regions
                             , radio_mean = TRUE)
          ,
          mod_QPR_tabItem_ui("PH"
                             , project_choices = choices_project_type[tab_choices$PH]
                             , region_choices = choices_regions)
          ,
          mod_QPR_tabItem_ui("NCB"
                             , project_choices = choices_project_type[tab_choices$NCB]
                             , region_choices = choices_regions)
          ,
          shinydashboard::tabItem(tabName = "HI-Tab",
                                  shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerQPRHI"), width = 12)),
                                  shinyWidgets::chooseSliderSkin("Round"),
                                  shinyWidgets::setSliderColor("#56B4E9", 1),
                                  shinyWidgets::sliderTextInput("QPRHIDateSlider",
                                                                "",
                                                                c(
                                                                  unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4)
                                                                ),
                                                                selected = zoo::Sys.yearqtr() - 1 / 4),
                                  shinyWidgets::pickerInput(
                                    inputId = "QPRHIRegionSelect",
                                    "Select Region(s)",
                                    choices = choices_regions,
                                    options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                                          actionsBox = TRUE),
                                    multiple = TRUE,
                                    selected = sample(choices_regions, 1)
                                  ),
                                  shinyWidgets::prettyRadioButtons(
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
                                  plotly::plotlyOutput("QPRHIs"),
                                  htmltools::br()),
          shinydashboard::tabItem(tabName = "income-Tab",
                                  shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerQPRIncome"), width = 12)),
                                  shinyWidgets::chooseSliderSkin("Round"),
                                  shinyWidgets::setSliderColor("#56B4E9", 1),
                                  shinyWidgets::sliderTextInput("QPRIncomeDateSlider",
                                                                "",
                                                                c(
                                                                  unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4)
                                                                ),
                                                                selected = zoo::Sys.yearqtr() - 1 / 4),
                                  shinyWidgets::pickerInput(
                                    inputId = "QPRIncomeRegionSelect",
                                    "Select Region(s)",
                                    choices = choices_regions,
                                    options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                                          actionsBox = TRUE),
                                    multiple = TRUE,
                                    selected = sample(choices_regions, 1)
                                  ),
                                  shinyWidgets::prettyRadioButtons(
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
                                  plotly::plotlyOutput("QPRIncome"),
                                  htmltools::br()),
      mod_QPR_tabItem_ui("RRH", region_choices = choices_regions),
          shinydashboard::tabItem(tabName = "spending-Tab",
                                  shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerRRHSpending"), width = 12)),
                                  shinyWidgets::chooseSliderSkin("Round"),
                                  shinyWidgets::setSliderColor("#56B4E9", 1),
                                  shinyWidgets::sliderTextInput("RRHSpendingDateSlider",
                                                                "",
                                                                c(
                                                                  unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4)
                                                                ),
                                                                selected = zoo::Sys.yearqtr() - 1 / 4),
                                  shinyWidgets::pickerInput(
                                    inputId = "RRHRegion",
                                    "Select Region(s)",
                                    choices = choices_regions,
                                    options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                                          actionsBox = TRUE),
                                    multiple = TRUE,
                                    selected = sample(choices_regions, 1)
                                  ),
                                  plotly::plotlyOutput("RRHSpending")
          ),
          shinydashboard::tabItem(
            tabName = "spdat-Tab",
            shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerQPRCommunityNeed"), width = 12)),
            shinyWidgets::pickerInput(
              inputId = "regionList",
              choices = choices_regions,
              options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                    liveSearch = TRUE),
              width = "70%"
            ),
            shinyWidgets::chooseSliderSkin("Round"),
            shinyWidgets::setSliderColor("#56B4E9", 1),
            shinyWidgets::sliderTextInput("spdatSlider",
                                          "",
                                          c(
                                            unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() + 1 / 4)
                                          ),
                                          selected = zoo::Sys.yearqtr() - 1 / 4),
            shiny::plotOutput("SPDATScoresByCounty"),
            htmltools::HTML("<br>"),
            shiny::fluidRow(shinydashboard::box(
              shiny::textOutput("CountyScoresText"),
              title = "The Lines",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              shiny::textOutput("HHsServedScoresText"),
              title = "The Triangles",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              shiny::textOutput("NoteToUsers"),
              title = "A Note about Data Quality",
              collapsible = TRUE,
              collapsed = TRUE
            ))
          ), #tabItem SPDAT tab
          shinydashboard::tabItem(tabName = "about-Tab",
                                  shiny::fluidRow(
                                    actionButton("browser", "browser"),
                                    tags$script("$('#browser').hide();"),
                                    shinydashboard::box(
                                      title = "Ohio Balance of State CoC Homeless Planning Regions",
                                      htmltools::HTML("The solid-colored counties are all part of the Ohio
                       Balance of State CoC. The Ohio Development Services 
                       Agency (ODSA) further divided the counties in the Balance
                       of State into 17 Homeless Planning Regions to make
                       implementation of state-funded programs in the Balance of
                       State more localized.
                       <p> Throughout R minor, you will notice references to 
                       Homeless Planning Regions. Please consult this map if you 
                       are unsure what Region your county is in."),
                                      htmltools::img(
                                        src =
                                          "www/Homeless-Region-Map-for-COHHIO-2017.png",
                                        height = '100%',
                                        width = '100%'
                                      ),
                                      width = 6
                                    ),
                                    shinydashboard::box(htmltools::HTML("The Ohio Balance of State Continuum of Care (BoSCoC) 
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
                                    shinydashboard::box(
                                      htmltools::HTML(
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
                                    shinydashboard::box(
                                      htmltools::HTML(
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
      ) #dashboardBody
    ) #dashboardPage
  ) # tagList
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Rminorgolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

