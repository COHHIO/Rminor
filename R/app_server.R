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

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import lubridate
#' @import dplyr
#' @importFrom DT renderDataTable datatable dataTableOutput
#' @importFrom stringr str_replace str_starts
#' @importFrom tidyr pivot_longer pivot_wider gather spread
#' @importFrom tidyselect everything
#' @importFrom shinydashboard renderInfoBox infoBox
#' @import ggplot2
#' @importFrom stats reorder median
#' @importFrom plotly renderPlotly plot_ly add_trace layout
#' @importFrom scales percent
#' @importFrom purrr as_vector
#' @importFrom feather read_feather
#' 
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  # output$res <- renderPrint({
  #  input$utilizationDate
  # cat("Length of Stay", input$LoSRegionSelect)
  # })
  observeEvent(input$browser,{
    # browser()
  })
  output$headerUtilization <- shiny::renderUI({
    ReportEnd <- 
      lubridate::ceiling_date(lubridate::ymd(input$utilizationDate), unit = "month") - lubridate::days(1)
    ReportStart <- 
      lubridate::floor_date(lubridate::ymd(ReportEnd), unit = "month") - lubridate::years(1) + months(1)
    
    ReportStart <- format.Date(lubridate::ymd(ReportStart), "%B %d, %Y")
    ReportEnd <- format.Date(lubridate::ymd(ReportEnd), "%B %d, %Y")
    
    list(
      shiny::h2("Bed and Unit Utilization"),
      shiny::h4(input$providerListUtilization),
      shiny::h4(ReportStart, "-", ReportEnd)
    )
  })
  
  output$headerCovid19 <- shiny::renderUI({
    
    ReportStart <- format.Date(hc$began_collecting_covid_data, "%B %d, %Y")
    ReportEnd <- format.Date(lubridate::ymd(meta_HUDCSV$Export_End), "%B %d, %Y")
    
    list(
      shiny::h2("Ohio Balance of State CoC Covid-19 Data Analysis"),
      shiny::h4(ReportStart, "-", ReportEnd)
    )
  })
  
  output$plotCovid19Status <- shiny::renderPlot(covid19_status_plot)
  output$plotCovid19Priority <- shiny::renderPlot(covid19_priority_plot)
  
  output$headerCoCCompetitionProjectLevel <- renderUI({
    
    next_thing_due <- tribble(
      ~ DueDate, ~ Event,
      ymd(hc$project_eval_docs_due), "Projects submit program documents to evidence 
      best practices and CE Prioritization compliance",
      ymd("20210506"), "All HMIS data corrections must be complete by 11:59pm",
      ymd("20210507"), "Project Evaluation data is saved as final data for scoring",
      ymd("20210527"), "CoC staff post online preliminary renewal project ranking",
      ymd("20210604"), "Recipients submit appeals of project evaluation results and 
      ranking to ohioboscoc@cohhio.org.",
      ymd("20210625"), "Final CoC project ranking released"
    ) %>%
      mutate(
        ShowStart = lag(ymd(DueDate), n = 1L, order_by = DueDate),
        ShowStart = if_else(is.na(ShowStart), today(), ShowStart + days(1)),
        ShowEnd = ymd(DueDate),
        DateRange = interval(ShowStart, ShowEnd)
      ) %>%
      filter(today() %within% DateRange) %>%
      select(Event, DueDate)
    
    list(
      h2("2021 BoS CoC Competition: Project Evaluation"), 
      h4(paste("Fixed Date Range:", 
               format.Date(hc$project_eval_start, "%B %d, %Y"), 
               "to",
               format.Date(hc$project_eval_end, "%B %d, %Y"))),
      # h4(strong("THE DATA ON THIS TAB DOES NOT SHOW CHANGES MADE ON OR AFTER
      #           5-7-2021.")),
      h4(input$pe_provider),
      hr(),
      h5(strong("Next Due Date:"),
         format(ymd(next_thing_due$DueDate), "%A %b %e, %Y"),
         "| ",
         next_thing_due$Event),
      p("Please consult the", 
        a("CoC Program",
          href = "https://cohhio.org/boscoc/coc-program/", target="_blank"), 
        "page for complete specifications and timeline.")
    )
  })
  
  output$pe_ProjectSummary <-
    DT::renderDataTable({
      ptc <- summary_pe_final_scoring() %>%
        filter(AltProjectName == input$pe_provider) %>%
        pull(ProjectType)
      
      summary_pe_final_scoring <- summary_pe_final_scoring() %>%
        mutate(
          ExitsToPHMath = str_replace(ExitsToPHMath, "/", "÷"),
          # OwnHousingMath = str_replace(OwnHousingMath, "/", "÷"),
          # IncreasedIncomeMath = str_replace(IncreasedIncomeMath, "/", "÷"),
          BenefitsAtExitMath = str_replace(BenefitsAtExitMath, "/", "÷"),
          AverageLoSMath = str_replace(AverageLoSMath, "/", "÷"),
          LHResPriorMath = str_replace(LHResPriorMath, "/", "÷"),
          NoIncomeAtEntryMath = str_replace(NoIncomeAtEntryMath, "/", "÷"),
          MedianHHIMath = str_replace(MedianHHIMath, "/", "÷"),
          LongTermHomelessMath = str_replace(LongTermHomelessMath, "/", "÷"),
          ScoredAtEntryMath = str_replace(ScoredAtEntryMath, "/", "÷"),
          DQMath = str_replace(DQMath, "/", "÷"),
          PrioritizationWorkgroupMath = str_replace(PrioritizationWorkgroupMath, "/", "÷"),
          HousingFirstMath = str_replace(HousingFirstMath, "/", "÷"),
          ChronicPrioritizationMath = str_replace(ChronicPrioritizationMath, "/", "÷")
        )
      
      a <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHPoints,
          # "Moved into Own Housing" = OwnHousingPoints,
          # "Increased Income" = IncreasedIncomePoints,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPoints,
          "Average Length of Stay" = AverageLoSPoints,
          "Living Situation at Entry" = LHResPriorPoints,
          "No Income at Entry" = NoIncomeAtEntryPoints,
          "Median Homeless History Index" = MedianHHIPoints,
          "Long Term Homeless" = LongTermHomelessPoints,
          "VISPDAT Completion at Entry" = ScoredAtEntryPoints,
          "Data Quality" = DQPoints,
          "Prioritization Workgroup" = PrioritizationWorkgroupScore,
          "Housing First" = HousingFirstScore,
          "Prioritization of Chronic" = ChronicPrioritizationScore
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Estimated Score")
      
      b <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHDQ,
          # "Moved into Own Housing" = OwnHousingDQ,
          # "Increased Income" = IncreasedIncomeDQ,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitDQ,
          "Average Length of Stay" = AverageLoSDQ,
          "Living Situation at Entry" = LHResPriorDQ,
          "No Income at Entry" = NoIncomeAtEntryDQ,
          "Median Homeless History Index" = MedianHHIDQ,
          "Long Term Homeless" = LTHomelessDQ,
          "VISPDAT Completion at Entry" = ScoredAtEntryDQ,
          "Housing First" = HousingFirstDQ,
          "Prioritization of Chronic" = ChronicPrioritizationDQ
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "DQflag")
      
      c <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHPossible,
          # "Moved into Own Housing" = OwnHousingPossible,
          # "Increased Income" = IncreasedIncomePossible,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
          "Average Length of Stay" = AverageLoSPossible,
          "Living Situation at Entry" = LHResPriorPossible,
          "No Income at Entry" = NoIncomeAtEntryPossible,
          "Median Homeless History Index" = MedianHHIPossible,
          "Long Term Homeless" = LongTermHomelessPossible,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryPossible,
          "Data Quality" = DQPossible,
          "Prioritization Workgroup" = PrioritizationWorkgroupPossible,
          "Housing First" = HousingFirstPossible,
          "Prioritization of Chronic" = ChronicPrioritizationPossible
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Possible Score")
      
      d <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHMath,
          # "Moved into Own Housing" = OwnHousingMath,
          # "Increased Income" = IncreasedIncomeMath,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
          "Average Length of Stay" = AverageLoSMath,
          "Living Situation at Entry" = LHResPriorMath,
          "No Income at Entry" = NoIncomeAtEntryMath,
          "Median Homeless History Index" = MedianHHIMath,
          "Long Term Homeless" = LongTermHomelessMath,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryMath,
          "Data Quality" = DQMath,
          "Prioritization Workgroup" = PrioritizationWorkgroupMath,
          "Housing First" = HousingFirstMath,
          "Prioritization of Chronic" = ChronicPrioritizationMath
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Calculation")
      
      psh <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in% c("Moved into Own Housing",
                               "Average Length of Stay")) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      rrh <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in%
                 c("Long Term Homeless",
                   "Prioritization of Chronic",
                   "Prioritization Workgroup")) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      th <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in% c(
          "Long Term Homeless",
          "Prioritization of Chronic",
          "Prioritization Workgroup"
        )) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      datatable(
        if (ptc == 3) {
          psh
        } else if (ptc == 13) {
          rrh
        } else if(ptc == 2) {
          th
        },
        rownames = FALSE,
        options = list(dom = 't',
                       pageLength = 100)
      )
    })
  
  shiny::observeEvent(c(input$providerList), {
    output$currentUnitUtilization <-
      if (nrow(utilization() %>%
               dplyr::filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Unit Utilization",
            subtitle = paste(
              utilization() %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(Households),
              "Households in",
              utilization() %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(UnitCount),
              "Units"
            ),
            color = "aqua",
            icon = shiny::icon("building"),
            value = utilization() %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(UnitUtilization)
          )
        })
      }
    else{
      
    }
    
    output$currentBedUtilization <-
      if (nrow(utilization() %>%
               dplyr::filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1:3, 8, 9)
               )) > 0) {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Bed Utilization",
            subtitle = paste(
              utilization() %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(Clients),
              "Clients in",
              utilization() %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(BedCount),
              "Beds"
            ),
            color = "purple",
            icon = shiny::icon("bed"),
            utilization() %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(BedUtilization)
          )
        })
      }
    else{
      
    }
    
    output$headerSPMs <- shiny::renderUI({

      ReportStart <- format.Date(lubridate::ymd(spm_current_start_date), "%B %d, %Y")
      ReportEnd <- format.Date(lubridate::ymd(spm_current_end_date), "%B %d, %Y")
      
      PriorReportStart <- format.Date(lubridate::ymd(spm_prior_start_date), "%B %d, %Y")
      PriorReportEnd <- format.Date(lubridate::ymd(spm_prior_end_date), "%B %d, %Y")
      
      list(
        shiny::h2("HUD System Performance Measures"),
        shiny::h4(sub(x = input$SPM_CoC_radio, "CoC", "Continuum of Care")),
        shiny::h4(ReportStart, "-", ReportEnd),
        p(
          "The data here is based on the HUD System Performance Measures. For a
          great primer as to what these measures are, you can view the following",
          a(href = "https://www.hudexchange.info/trainings/system-performance-measures/",
            target = "blank",
            "introductory videos"),
          " created by HUD."
        ),
        p("Prior Reporting Period ->",
          PriorReportStart,
          "to",
          PriorReportEnd),
        p("Current Reporting Period ->",
          ReportStart,
          "to",
          ReportEnd)
      )
    })
    
    output$footerSPMLoTH <- shiny::renderText({
      if_else(
        input$SPM_CoC_radio == "Ohio Balance of State CoC",
        "Persons in ES, SH, TH, RRH, and PSH. BoS CoC goal = no more than 90
          days average and median",
        "Persons in ES, SH, TH, RRH, and PSH."
      )
      
    })
    
    output$footerSPMRecurrence <- shiny::renderText({
      if_else(
        input$SPM_CoC_radio == "Ohio Balance of State CoC",
        "Persons in ES, SH, TH, Outreach, RRH, and PSH.  BoS CoC Goals: 6 month
        goal = <10%, 24 month goal = <20%",
        "Persons in ES, SH, TH, Outreach, RRH, and PSH."
      )
    })
    
    output$footerSPMPIT <- shiny::renderText({
      if_else(
        input$SPM_CoC_radio == "Ohio Balance of State CoC",
        "BoS CoC Goals: Total and Sheltered: reduce by 4% annually. Veteran
        and Chronic: reduce by 10% annually.",
        "Annual PIT Counts"
      )
    })
    
    output$footerSPMExitsToPH <- shiny::renderText({
      if_else(
        input$SPM_CoC_radio == "Ohio Balance of State CoC",
        "BoS CoC Goals: Persons in ES, SH, TH, RRH: 75%, PSH: 90%",
        ""
      )
    })
    
    output$veteranEngagement <-
      if (nrow(
        veteran_current_in_project() %>%
        dplyr::filter(ProjectName == input$providerList) %>%
        dplyr::select(Veterans)
      ) > 0) {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Veterans",
            subtitle = veteran_current_in_project() %>%
              dplyr::filter(ProjectName == input$providerList) %>% 
              dplyr::pull(Summary),
            color = "green",
            icon = shiny::icon("ribbon"),
            veteran_current_in_project() %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(Veterans)
          )
        })
      }
    else {
      
    }
    
    output$TAYEngagement <-
      if (nrow(
        current_tay_hohs() %>%
        dplyr::filter(ProjectName == input$providerList)
      ) > 0) {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Transition Aged Youth Households",
            subtitle = current_tay_hohs() %>%
              dplyr::filter(ProjectName == input$providerList) %>% dplyr::pull(Summary),
            color = "black",
            icon = shiny::icon("id-card"),
            current_tay_hohs() %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(TAYHHs)
          )
        })
      }
    else {
      
    }
    
    output$CurrentlyAwaitingPH <-
      if (nrow(validation() %>%
               dplyr::filter(ProjectType %in% c(3, 9, 13) &
                             ProjectName == input$providerList)) > 0) {
        shinydashboard::renderInfoBox({
          hhs <- nrow(
            validation() %>%
              dplyr::filter(
                ProjectName == input$providerList &
                  is.na(MoveInDateAdjust) &
                  is.na(ExitDate)
              ) %>%
              dplyr::select(HouseholdID) %>%
              unique()
          )
          
          daysWaiting <- validation() %>%
            dplyr::filter(ProjectName == input$providerList &
                            is.na(MoveInDateAdjust) &
                            is.na(ExitDate)) %>%
            dplyr::mutate(Waiting = as.numeric(lubridate::ymd(meta_HUDCSV$Export_End) - lubridate::ymd(EntryDate))) %>%
            dplyr::group_by(ProjectID) %>%
            dplyr::summarise(avgWait = as.integer(mean(Waiting)))
          
          shinydashboard::infoBox(
            title = "Active Households Not Yet Housed",
            subtitle = paste("Average Days Waiting:", daysWaiting$avgWait),
            color = "black",
            icon = shiny::icon("pause-circle"),
            hhs
          )
        })
      }
    else{
      
    }
    
    output$CurrentClientCount <-
      if (nrow(validation() %>%
               dplyr::filter(
                 ProjectType %in% c(12, 13, 4) &
                 ProjectName == input$providerList
               )) > 0) {
        current <- validation() %>%
          dplyr::filter(ProjectName == input$providerList &
                          is.na(ExitDate)) %>%
          dplyr::select(PersonalID) %>%
          unique()
        
        movedin <- validation() %>%
          dplyr::filter(
            ProjectName == input$providerList &
              is.na(ExitDate) &
              !is.na(MoveInDateAdjust) &
              ProjectType == 13
          ) %>%
          dplyr::select(PersonalID) %>%
          unique()
        
        PTC <- validation() %>%
          dplyr::filter(ProjectName == input$providerList) %>%
          dplyr::select(ProjectType) %>% unique()
        
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Clients",
            subtitle = dplyr::if_else(
              PTC == 13,
              paste(nrow(movedin),
                    "client(s) housed in the project"),
              ""
            ),
            color = "fuchsia",
            icon = shiny::icon("home"),
            nrow(current)
          )
        })
      }
    else{
      
    }
    
    output$CurrentHHCount <-
      if (nrow(validation() %>%
               dplyr::filter(
                 ProjectType %in% c(12, 13, 4) &
                 ProjectName == input$providerList
               )) > 0) {
        current <- validation() %>%
          dplyr::filter(ProjectName == input$providerList &
                          is.na(ExitDate)) %>%
          dplyr::select(HouseholdID) %>%
          unique()
        
        movedin <- validation() %>%
          dplyr::filter(
            ProjectName == input$providerList &
              is.na(ExitDate) &
              !is.na(MoveInDateAdjust) &
              ProjectType == 13
          ) %>%
          dplyr::select(HouseholdID) %>%
          unique()
        
        PTC <- validation() %>%
          dplyr::filter(ProjectName == input$providerList) %>%
          dplyr::select(ProjectType) %>% unique()
        
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Households",
            subtitle = dplyr::if_else(
              PTC == 13,
              paste(nrow(movedin),
                    "household(s) housed in the project"),
              ""
            ),
            color = "teal",
            icon = shiny::icon("users"),
            nrow(current)
          )
        })
      }
    else{
      
    }
    
    output$ShelterExitsToRRH <-
      if (nrow(validation() %>%
               dplyr::filter(ProjectType == 1 &
                             ProjectName == input$providerList)) > 0) {
        ReportStart <-
          format.Date(lubridate::floor_date(lubridate::today(), unit = "year"), "%m-%d-%Y")
        
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = paste("Client Exits to Rapid Rehousing in", 
                          lubridate::year(lubridate::ymd(meta_HUDCSV$Export_End))),
            subtitle = "Destination: Rental by client, with RRH...",
            color = "light-blue",
            icon = shiny::icon("door-open"),
            nrow(
              validation() %>%
                dplyr::filter(
                  ProjectName == input$providerList &
                    HMIS::exited_between(., ReportStart, ymd(meta_HUDCSV$Export_End)) &
                    Destination == 31
                )
            )
          )
        })
      }
    else{
      
    }
    
  })
  
  mod_ceAPs_server("ceAPs")
  
  output$bedPlot <-
    plotly::renderPlotly({
      ReportEnd <- lubridate::ymd(input$utilizationDate) 
      ReportStart <- lubridate::floor_date(lubridate::ymd(ReportEnd), unit = "month") -
        lubridate::years(1) +
        months(1)
      ReportingPeriod <- lubridate::interval(lubridate::ymd(ReportStart), lubridate::ymd(ReportEnd))
      
      Provider <- input$providerListUtilization
      
      bedPlot <- utilization_bed() %>% 
        tidyr::gather("Month",
                      "Utilization",
                      -ProjectID,
                      -ProjectName,
                      -ProjectType) %>%
        dplyr::filter(ProjectName == Provider,
                      lubridate::mdy(Month) %within% ReportingPeriod) %>%
        dplyr::mutate(
          Month = lubridate::floor_date(lubridate::mdy(Month), unit = "month"),
          Bed = Utilization,
          Utilization = NULL
        )
      
      unitPlot <- utilization_unit() %>% 
        tidyr::gather("Month",
                      "Utilization",
                      -ProjectID,
                      -ProjectName,
                      -ProjectType) %>%
        dplyr::filter(ProjectName == Provider,
                      lubridate::mdy(Month) %within% ReportingPeriod) %>%
        dplyr::mutate(
          Month = lubridate::floor_date(lubridate::mdy(Month), unit = "month"),
          Unit = Utilization,
          Utilization = NULL
        )
      
      utilizationPlot <- unitPlot %>%
        dplyr::full_join(bedPlot,
                         by = c("ProjectID", "ProjectName", "ProjectType", "Month")) 
      
      plotly::plot_ly(utilizationPlot, 
                      x = ~Month) %>%
        plotly::add_trace(y = ~ Unit,
                          name = "Unit Utilization",
                          type = "scatter",
                          mode = "lines+markers",
                          hoverinfo = 'y') %>%
        plotly::add_trace(y = ~Bed,
                          name = "Bed Utilization",
                          type = "scatter",
                          mode = "lines+markers",
                          hoverinfo = 'y') %>%
        plotly::layout(yaxis = list(
          title = "Utilization",
          tickformat = "%",
          range = c(0, 2)
        ),
        margin = list(
          t = 100
        ),
        title = paste("Bed and Unit Utilization",
                      "\n", 
                      Provider,
                      "\n", 
                      format(lubridate::ymd(ReportStart), "%B %Y"), 
                      "to", 
                      format(lubridate::ymd(ReportEnd), "%B %Y")))
      
    })  
  
  output$unitNote <-
    shiny::renderUI(note_unit_utilization)
  
  output$covidText <- shiny::renderUI(
    shiny::HTML("The Ohio Balance of State CoC is collecting COVID-19 data based on the 
         <a href=\"https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html\">CDC's guidelines</a>. 
         We began collecting this data in April 2020. 
         <p><br>While all Access Points into the Balance of State CoC
         homeless system are required to screen every household for Covid-19, 
         they are <strong>not</strong> required to enter that data into HMIS, so this data is
         not representative of the entire Continuum of Care. 
         <p>Please send inquiries to the COHHIO 
         <a href = \"mailto:hmis@cohhio.org.\">HMIS team</a>.")
  )
  
  output$bedNote <-
    shiny::renderUI(note_bed_utilization)
  
  output$utilizationNote <-
    shiny::renderUI(shiny::HTML(note_calculation_utilization))
  
  output$spmLoTH <- DT::renderDataTable({
    
    a <- spm_Metric_1b() %>%
      dplyr::filter(Metric1b == "Persons in ES, SH, TH, and PH" &
                      CoCName == case_when(
                        input$SPM_CoC_radio == "Ohio Balance of State CoC" ~ "OH-507",
                        input$SPM_CoC_radio == "Mahoning County CoC" ~ "OH-504"
                      )) %>%
      dplyr::mutate(AvgLoT_Current = paste(as.integer(AvgLoT_Current), "days"),
                    AvgLoT_Prior = paste(as.integer(AvgLoT_Prior), "days"),
                    MedLoT_Current = paste(MedLoT_Current, "days"),
                    MedLoT_Prior = paste(MedLoT_Prior, "days")) %>%
      dplyr::select(
        "Prior Year<br>Average" = AvgLoT_Prior,
        "Current Year<br>Average" = AvgLoT_Current,
        "Prior Year<br>Median" = MedLoT_Prior,
        "Current Year<br>Median" = MedLoT_Current
      )
    
    DT::datatable(a,
                  rownames = FALSE,
                  options = list(dom = 't'),
                  escape = FALSE)
    
  })
  
  output$spmRecurrence <- DT::renderDataTable({
    
    a <- spm_Metric_2() %>%
      dplyr::filter(ProjectType == "TOTAL Returns to Homelessness" &
                      CoCName == case_when(
                        input$SPM_CoC_radio == "Ohio Balance of State CoC" ~ "OH-507",
                        input$SPM_CoC_radio == "Mahoning County CoC" ~ "OH-504"
                      )) %>%
      dplyr::mutate_at(dplyr::vars(-ProjectType, -CoCName), as.integer) %>%
      dplyr::mutate(
        Percent6moPrior = scales::percent(LessThan6mo_Prior / ExitedToPHPast2Yrs_Prior,
                                          accuracy = .1),
        Percent6moCurrent = scales::percent(LessThan6mo_Current / ExitedToPHPast2Yrs_Current,
                                            accuracy = .1),
        Percent2yrPrior = scales::percent((ThirteenTo24mo_Prior +
                                             SixTo12mo_Prior +
                                             LessThan6mo_Prior) / ExitedToPHPast2Yrs_Prior,
                                          accuracy = .1),
        Percent2yrCurrent = scales::percent((ThirteenTo24mo_Current +
                                               LessThan6mo_Current +
                                               SixTo12mo_Current) / ExitedToPHPast2Yrs_Current,
                                            accuracy = .1)
      ) %>%
      dplyr::select(
        "Prior Year<br>Recurred in 6 months or less" = Percent6moPrior,
        "Current Year<br>Recurred in 6 months or less" = Percent6moCurrent,
        "Prior Year<br>Recurred up to 2 Years After Permanent Exit" = Percent2yrPrior,
        "Current Year<br>Recurred up to 2 Years After Permanent Exit" = Percent2yrCurrent
      )
    
    DT::datatable(a,
                  rownames = FALSE,
                  options = list(dom = 't'),
                  escape = FALSE)
    
  })
  
  output$spmExitsToPH <- DT::renderDataTable({
    
    a <- spm_Metric_7() %>%
      dplyr::filter(
        str_starts(ClientsCounted, "% Successful exits") &
          CoCName == case_when(
            input$SPM_CoC_radio == "Ohio Balance of State CoC" ~ "OH-507",
            input$SPM_CoC_radio == "Mahoning County CoC" ~ "OH-504"
          ) &
          Metric %in% c("7b1", "7b2")
      ) %>%
      dplyr::mutate(
        Metric = if_else(
          Metric == "7b1",
          "ES, TH, SH, RRH: Successful Exits",
          "PSH: Successful Exits/Retention of Housing in PSH"
        )
      ) %>%
      dplyr::select(Metric,
                    "Prior Year" = Prior,
                    "Current Year" = Current)
    
    DT::datatable(a,
                  rownames = FALSE,
                  options = list(dom = 't'))
    
  })
  
  output$spmPIT <- DT::renderDataTable({
    
    a <- BoS_PIT() %>%
      dplyr::mutate(
        Difference = scales::percent((January2020Count - January2019Count)
                                     /January2019Count,
                                     accuracy = .1),
        Difference = dplyr::if_else(stringr::str_starts(Difference, "-"),
                                    Difference,
                                    paste0("+", Difference))
      ) %>%
      dplyr::select(Population,
                    "January 2019 Count" = January2019Count,
                    "January 2020 Count" = January2020Count,
                    Difference)
    
    b <- Mah_PIT() %>%
      dplyr::mutate(
        Difference = scales::percent((January2020Count - January2019Count)
                                     /January2019Count,
                                     accuracy = .1),
        Difference = dplyr::if_else(stringr::str_starts(Difference, "-"),
                                    Difference,
                                    paste0("+", Difference))
      ) %>%
      dplyr::select(Population,
                    "January 2019 Count" = January2019Count,
                    "January 2020 Count" = January2020Count,
                    Difference)
    
    if (input$SPM_CoC_radio == "Ohio Balance of State CoC") {
      DT::datatable(a,
                    rownames = FALSE,
                    options = list(dom = 't'))
    } else {
      DT::datatable(b,
                    rownames = FALSE,
                    options = list(dom = 't'))
    }
  })
  
  output$headerQPRCommunityNeed <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$spdatSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$spdatSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$spdatSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$spdatSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$spdatSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$spdatSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3("Community Need"),
         shiny::h4(input$regionList),
         shiny::h4(ReportStart, "-", ReportEnd))
  })  
  
  output$SPDATScoresByCounty <-
    shiny::renderPlot({
      ReportStart <- format.Date(lubridate::ymd(paste0(
        substr(input$spdatSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      ReportEnd <- format.Date(lubridate::mdy(paste0(
        dplyr::case_when(
          substr(input$spdatSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$spdatSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$spdatSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$spdatSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$spdatSlider, 1, 4)
      )), "%m-%d-%Y")
      # counting all hhs who were scored AND SERVED between the report dates
      CountyAverageScores <- qpr_spdats_county() %>%
        dplyr::filter(HMIS::served_between(., ReportStart, ReportEnd)) %>%
        dplyr::select(CountyServed, PersonalID, Score) %>%
        dplyr::distinct() %>%
        dplyr::group_by(CountyServed) %>%
        dplyr::summarise(AverageScore = round(mean(Score), 1),
                         HHsLHinCounty = dplyr::n())
      # counting all hhs who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- qpr_spdats_project() %>%
        dplyr::filter(HMIS::entered_between(., ReportStart, ReportEnd)) %>%
        dplyr::group_by(CountyServed) %>%
        dplyr::summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
                         HHsHousedInCounty = dplyr::n())
      # pulling in both averages for each county plus adding Region for grouping
      Compare <-
        dplyr::full_join(CountyAverageScores,
                         CountyHousedAverageScores,
                         by = "CountyServed") %>%
        dplyr::arrange(CountyServed) %>%
        dplyr::left_join(., regions(), by = c("CountyServed" = "County")) %>%
        dplyr::filter(RegionName == input$regionList)
      # the plot
      ggplot2::ggplot(Compare, ggplot2::aes(x = CountyServed, y = AverageScore)) +
        ggplot2::geom_point(size = 12, shape = 95) +
        ggplot2::scale_y_continuous(limits = c(0, 17)) +
        ggplot2::geom_point(
          ggplot2::aes(y = HousedAverageScore),
          size = 6,
          shape = 17,
          colour = "#56B4E9"
        ) +
        ggplot2::xlab("County Where Served") +
        ggplot2::ylab("Average SPDAT Score") +
        ggplot2::theme_light() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(lineheight = 5, size = ggplot2::rel(1.8)),
          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.8)),
          axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.8)),
          plot.margin = ggplot2::margin(
            t = 15,
            r = 15,
            b = 15,
            l = 15
          )
        ) +
        ggplot2::labs(
          title = input$regionList,
          subtitle = paste("Date Range:", ReportStart, "to", ReportEnd),
          caption = "VI-SPDAT scores and household enrollment data comes from
          the Ohio Balance of State CoC HMIS. Detail may be found at R minor
          elevated."
        )
    })
  
  output$CountyScoresText <-
    shiny::renderText(note_qpr_served_county)
  
  output$HHsServedScoresText <-
    shiny::renderText(note_qpr_housed_county)
  
  output$NoteToUsers <-
    shiny::renderText(note_qpr_dq_community_need)
  
  #  QPR Length of Stay ----
  mod_QPR_server("LoS", "Length of Stay")
  
  # QPR Exits to PH 
  mod_QPR_server("PH", "Exits to Permanent Housing")
  
  # QPR NonCash Benefits
  mod_QPR_server("NCB", "Access to Mainstream Benefits: Non-cash Benefits")
  
  # QPR Health Insurance
  mod_QPR_server("HI", "Access to Mainstream Benefits: Health Insurance")
  
  # QPR Increase Income
  mod_QPR_server("Income", "Access to Mainstream Benefits: Increased Income")
  
  # QPR Rapid Placement into RRH
  mod_QPR_server("RRH", "Rapid Placement into Rapid Rehousing")
  
  #  QPR HP vs RRH Spending
  mod_QPR_server("RRHspending", "Rapid Rehousing Spending Goals")

}
