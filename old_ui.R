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
#' @param data_ui \code{(list)} List with variables passed to UI objects.
#' @importFrom htmltools HTML br img
#' @import shiny
#' @importFrom 
#'   shinydashboard 
#'   dashboardPage 
#'   dashboardHeader 
#'   dashboardSidebar 
#'   sidebarMenu 
#'   menuItem 
#'   menuSubItem 
#'   dashboardBody 
#'   tabItems 
#'   tabItem 
#'   box
#' @importFrom 
#'   shinyWidgets 
#'   pickerInput 
#'   pickerOptions 
#'   airDatepickerInput 
#'   chooseSliderSkin 
#'   setSliderColor 
#'   sliderTextInput 
#'   prettyRadioButtons
#' @importFrom dplyr select
#' @importFrom lubridate ymd floor_date days
#' @importFrom plotly plotlyOutput
#' @importFrom zoo Sys.yearqtr
#' @noRd
#' 
app_ui <- function(request, data_ui) {
  if (!missing(data_ui)) {
    list2env(data_ui, environment())
  }
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
          #CHANGED standardize format of tabnames to shiny compatible namespaces 
          # [NS]-* where * in this case is "Tab". These will allow submenuitems 
          # to be generated programmatically
          shinydashboard::menuItem("Provider Dashboard",
                                   tabName = "providerDashboard-Tab"),
          menuItem("BoS CoC Competition",
                   tabName = "cocCompetitionTab"),
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
                                        tabName = "Income-Tab"),
            shinydashboard::menuSubItem("Rapid Placement for RRH",
                                        tabName = "RRH-Tab"),
            shinydashboard::menuSubItem("RRH vs HP Spending",
                                        tabName = "RRHspending-Tab")
          ),
          shinydashboard::menuItem("About",
                                   tabName = "about-Tab")
        ),
        htmltools::HTML(paste0(
          "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
          format(rm_dates$meta_HUDCSV$Export_Date, "%m-%d-%Y %I:%M %p")
          ,
          "<p><p>&emsp;" # <- add short message here if you want
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
                                        options = shinyWidgets::pickerOptions(
                                          dropupAuto = FALSE,
                                          liveSearch = TRUE,
                                          liveSearchStyle = 'contains'),
                                        width = "100%",
                                        ),
                                      shiny::uiOutput("CurrentClientCount"),
                                      shiny::uiOutput("CurrentHHCount"),
                                      shiny::uiOutput("currentUnitUtilization"),
                                      shiny::uiOutput("currentBedUtilization"),
                                      shiny::uiOutput("veteranEngagement"),
                                      shiny::uiOutput("TAYEngagement"),
                                      # shiny::uiOutput("ShelterExitsToRRH"),
                                      shiny::uiOutput("CurrentlyAwaitingPH")
                                    )
                                  )), 
          mod_ceAPs_ui("ceAPs"),
          shinydashboard::tabItem(
            tabName = "covid19-Tab",
            shiny::fluidRow(shinydashboard::box(shiny::htmlOutput("headerCovid19"), 
                                                width = 12)),
            shiny::fluidRow(shinydashboard::box(
              shiny::uiOutput("covidText"),
              title = "Covid-19 Data Collection",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(
              shiny::plotOutput("plotCovid19Status"),
              title = "Covid-19 Status at Last Screening",
              width = 12)), 
            shiny::fluidRow(shinydashboard::box(
              shiny::plotOutput("plotCovid19Priority"),
              title = "Prioritization Category at Coordinated Entry",
              width = 12))
          ),
          shinydashboard::tabItem(
            tabName = "utilization-Tab",
            shiny::fluidRow(
              shinydashboard::box(shiny::htmlOutput("headerUtilization"), 
                                  width = 12)),
            shiny::fluidRow(
              shinydashboard::box(
                title = "NOTICE",
                status = "warning",
                solidHeader = TRUE,
                "During this time, congregate facilities should be aiming to 
                deconcentrate. If this causes fluctuations in Utilization, that 
                is okay. Please continue to keep your clients safe."
                ,
                width = 6
              )
            ), 
            shiny::fluidRow(shinydashboard::box(
              shinyWidgets::pickerInput(
                inputId = "providerListUtilization",
                choices = c(sort(utilization_bed()$ProjectName)),
                options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                      liveSearch = TRUE,
                                                      liveSearchStyle = 'contains'),   
                selected = utilization_bed() %>% dplyr::pull(ProjectName) %>% `[`(1),
                width = "100%"
              ),
              shinyWidgets::airDatepickerInput(
                inputId = "utilizationDate",
                label = "Report End Month",
                max =
                  lubridate::ymd(lubridate::floor_date(rm_dates$meta_HUDCSV$Export_End, 
                                                       unit = "month") - 
                                 lubridate::days(1)),
                min =
                  lubridate::ymd(lubridate::floor_date(rm_dates$meta_HUDCSV$Export_End - 
                                                         lubridate::days(335), 
                                                       unit = "month")),
                dateFormat = "MM yyyy",
                view = "month",
                value =
                  lubridate::ymd(lubridate::floor_date(rm_dates$meta_HUDCSV$Export_End,
                                                       unit = "month") -
                                   lubridate::days(1)),
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
              HTML(utilization_notes$bed),
              title = "What is Bed Utilization?",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              HTML(utilization_notes$unit),
              title = "What is Unit Utilization?",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              shiny::HTML(utilization_notes$calc),
              title = "Methodology",
              collapsible = TRUE,
              collapsed = TRUE
            ))
          ),
          tabItem(tabName = "cocCompetitionTab",
                  fluidRow(box(
                    htmlOutput("headerCoCCompetitionProjectLevel"),
                    width = 12
                  )),
                  fluidRow(box(
                    pickerInput(
                      inputId = "pe_provider",
                      label = "Select your CoC-funded Provider",
                      choices = sort(pe_summary_validation()$AltProjectName) %>%
                        unique(),
                      selected = sample(pe_summary_validation()$AltProjectName, 1),
                      options = pickerOptions(liveSearch = TRUE,
                                              liveSearchStyle = 'contains'),
                      width = "100%"
                    ),
                    width = 12
                  )),
                  fluidRow(
                    box(
                      DT::dataTableOutput("pe_ProjectSummary"),
                      width = 12,
                      title = "Score Summary",
                      status = "info",
                      solidHeader = TRUE,
                      collapsible = TRUE
                    )
                  )),
          shinydashboard::tabItem(
            tabName = "SPM-Tab",
            shiny::fluidRow(shinydashboard::box(
              shiny::htmlOutput("headerSPMs"), 
              width = 12
              )),
            shiny::fluidRow(shinydashboard::box(
              shiny::radioButtons(
                inputId = "SPM_CoC_radio",
                inline = TRUE,
                label = "Choose CoC",
                choices = c("Ohio Balance of State CoC", "Mahoning County CoC"),
                selected = "Ohio Balance of State CoC",
                width = '100%'
              ),
              width = 12
            )),
            shiny::fluidRow(
              shinydashboard::box(
                DT::dataTableOutput("spmLoTH"),
                title = "Metric 1b: Length of Time Homeless",
                footer = shiny::textOutput("footerSPMLoTH"),
                width = 12
              )
            ), 
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmRecurrence"),
              title = "Metrics 2a1 & 2b1: Clients Returning to Homelessness After 
              Successful Placement",
              footer = shiny::textOutput("footerSPMRecurrence"),
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmPIT"),
              title = "Metric 3.1: January 2019 and January 2020 PIT Counts",
              footer = textOutput("footerSPMPIT"),
              width = 12
            )),
            shiny::fluidRow(shinydashboard::box(
              DT::dataTableOutput("spmExitsToPH"),
              title = "Metrics 7b1 & 7b2: Exits to or Retention of Permanent Housing",
              footer = textOutput("footerSPMExitsToPH"),
              width = 12
            ))
          ),
          mod_QPR_tabItem_ui(
            "LoS",
            project_choices = choices_project_type[tab_choices$LoS],
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"],
            radio_mean = TRUE
          ),
          mod_QPR_tabItem_ui(
            "PH",
            project_choices = choices_project_type[tab_choices$PH],
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"]),
          mod_QPR_tabItem_ui(
            "NCB",
            project_choices = choices_project_type[tab_choices$NCB],
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"]),
          mod_QPR_tabItem_ui(
            "HI",
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"],
            project_choices = choices_project_type[tab_choices$HI]),
          mod_QPR_tabItem_ui(
            "Income",
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"],
            project_choices = choices_project_type[tab_choices$Income]),
          mod_QPR_tabItem_ui(
            "RRH",
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"]),
          mod_QPR_tabItem_ui(
            "RRHspending",
            region_choices = choices_regions[choices_regions != "Mahoning County CoC"]),
          
          shinydashboard::tabItem(
            tabName = "spdat-Tab",
            shiny::fluidRow(shinydashboard::box(
              shiny::htmlOutput("headerQPRCommunityNeed"), width = 12)),
            shinyWidgets::pickerInput(
              inputId = "regionList",
              choices = choices_regions[choices_regions != "Mahoning County CoC"],
              options = shinyWidgets::pickerOptions(dropupAuto = FALSE,
                                                    liveSearch = TRUE,
                                                    liveSearchStyle = 'contains'),
              width = "70%"
            ),
            shinyWidgets::chooseSliderSkin("Round"),
            shinyWidgets::setSliderColor("#56B4E9", 1),
            shinyWidgets::sliderTextInput("spdatSlider",
                                          "",
                                          c(
                                            unique(zoo::Sys.yearqtr() - 6 / 4:
                                                     zoo::Sys.yearqtr() + 1 / 4)
                                          ),
                                          selected = zoo::Sys.yearqtr() - 1 / 4),
            shiny::plotOutput("SPDATScoresByCounty"),
            htmltools::HTML("<br>"),
            shiny::fluidRow(shinydashboard::box(
              HTML(qpr_notes$served_county),
              title = "The Lines",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              HTML(qpr_notes$housed_county),
              title = "The Triangles",
              collapsible = TRUE,
              collapsed = TRUE
            ),
            shinydashboard::box(
              HTML(qpr_notes$dq_community_need),
              title = "A Note about Data Quality",
              collapsible = TRUE,
              collapsed = TRUE
            ))
          ), #tabItem SPDAT tab
          shinydashboard::tabItem(
            tabName = "about-Tab",
            shiny::fluidRow(
              actionButton("browser", "browser"),
              tags$script("$('#browser').hide();"),
              shinydashboard::box(
                htmltools::HTML(
                  "<p>R minor is a free and open source project created and
              maintained by the HMIS team at Coalition on Homelessness and Housing
              in Ohio (COHHIO) for the Ohio Balance of State CoC and the Mahoning
              County CoC. Please find the code here:
              <a href=\"https://github.com/COHHIO/Rminor\">R minor code</a>

              <p>R minor contains up-to-date aggregate reporting at the project
              and system level. Its target audience is community planners, agency
              management, funders, and HMIS users who are looking for more system-
              level reporting."
                ),
                title = "About R minor",
                width = 12
              )
            ), 
              fluidRow(shinydashboard::box(htmltools::HTML("The Ohio Balance of
              State Continuum of Care (BoSCoC) represents 80 of the 88 counties 
              in Ohio and is the planning body for homeless services in the area. 
              The Ohio Development Services Agency (ODSA) and the Coalition on 
              Homelessness and Housing in Ohio (COHHIO) serve as the lead 
              staffing agencies and co-chairs of the Steering Committee for the 
              Ohio BoSCoC. ODSA serves as the Ohio BoSCoC Collaborative Applicant 
              (submits the annual consolidated CoC Application) while COHHIO 
              serves as the HMIS Lead Agency."),
                                  title = "Ohio Balance of State CoC",
                                  htmltools::img(src ="www/MapBoS.png",
                                                 height = '100%',
                                                 width = '100%'
                                  )),
              shinydashboard::box(
              htmltools::HTML("The Mahoning County Homeless Continuum of Care 
                              (MCHCoC) is one of nine Continua of Care in the 
                              State of Ohio and is the planning body for homeless 
                              services in Mahoning County. The Board of Mahoning 
                              County Commissioners serves as the Collaborative 
                              Applicant, HMIS Lead and employs the CoC Coordinator 
                              for the MCHCoC, while COHHIO serves as HMIS System 
                              Administrators."),
                                  title = "Mahoning County CoC",
                                  htmltools::img(src ="www/MapMahoningCountyCoC.png",
                                                 height = '100%',
                                                 width = '100%'
                                  ))),
            fluidRow(
              shinydashboard::box(
                title = "Ohio Balance of State CoC Homeless Planning Regions",
                htmltools::HTML(
                  "The solid-colored counties are all part of the Ohio
                       Balance of State CoC. The Ohio Development Services
                       Agency (ODSA) further divided the counties in the Balance
                       of State into 17 Homeless Planning Regions to make
                       implementation of state-funded programs in the Balance of
                       State more localized.
                       <p> Throughout R minor, you will notice references to
                       Homeless Planning Regions. Please consult this map if you
                       are unsure what Region your county is in."
                ),
                htmltools::img(
                  src = "www/Homeless-Region-Map-for-COHHIO-2017.png",
                  height = '100%',
                  width = '100%'
                ),
                width = 6,
                collapsible = TRUE,
                collapsed = TRUE
              ),
              shinydashboard::box(
                htmltools::HTML(
                  "<p>This project would not
              exist were it not for the existence of other quality free and open
              source products. Following are citations for the products R minor
              relies on.</p>

              <p>R Core Team (2019). R: A
              language and environment for statistical computing. R Foundation
              for Statistical Computing, Vienna, Austria.
              <a href=\"https://www.R-project.org/\">R programming language</a>.

              <p>Hadley Wickham (2017). tidyverse: Easily Install and Load
              the 'Tidyverse'. R package version 1.2.1.
              <a href=\"https://CRAN.R-project.org/package=tidyverse\">Tidyverse 
              package</a>

              <p>Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and
              Jonathan McPherson (2019). shiny: Web Application Framework for R.
              R package version 1.3.2.
              <a href=\"https://CRAN.R-project.org/package=shiny\">R Shiny package</a>
              and shinydashboard: Create Dashboards with 'Shiny'. R package 
              version 0.7.1.
              <a href=\"https://CRAN.R-project.org/package=shinydashboard\">
              shinydashboard
              package</a>"
                ),
                title = "Citations",
                collapsible = TRUE,
                collapsed = TRUE
              ) # box
            ) #fluidRow
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
      app_title = 'Rminor'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

