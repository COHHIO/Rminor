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
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  # output$res <- renderPrint({
  #  input$utilizationDate
  # cat("Length of Stay", input$LoSRegionSelect)
  # })
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
    
    ReportStart <- format.Date(lubridate::mdy("04012020"), "%B %d, %Y")
    ReportEnd <- format.Date(lubridate::ymd_hms(update_date), "%B %d, %Y")
    
    list(
      shiny::h2("Ohio Balance of State CoC Covid-19 Data Analysis"),
      shiny::h4(ReportStart, "-", ReportEnd)
    )
  })
  
  output$headerCoCCompetitionProjectLevel <- shiny::renderUI({
    next_thing_due <- dplyr::tribble(
      ~ DueDate, ~ Event,
      "7/20/2020", "All HMIS data corrections must be complete by 11:59pm",
      "7/21/2020", "COHHIO releases preliminary CoC project ranking (renewals only)",
      "7/31/2020", "Recipients submit appeals of project evaluation results and ranking to ohioboscoc@cohhio.org.",
      "8/7/2020", "Ohio BoSCoC Steering Committee will communicate decisions about all received appeals",
      "8/12/2020", "Final CoC project ranking released",
      "9/30/2020", "Final Ranking posted on cohhio.org"
    ) %>%
      dplyr::mutate(
        DueDate = lubridate::mdy(DueDate),
        ShowStart = dplyr::lag(lubridate::ymd(DueDate), n = 1L, order_by = DueDate),
        ShowStart = dplyr::if_else(is.na(ShowStart), lubridate::today(), ShowStart + lubridate::days(1)),
        ShowEnd = lubridate::ymd(DueDate),
        DateRange = lubridate::interval(ShowStart, ShowEnd)
      ) %>%
      dplyr::filter(lubridate::today() %within% DateRange) %>%
      dplyr::select(Event, DueDate)
    
    list(
      shiny::h2("2020 CoC Competition: Project Evaluation"),
      shiny::h4("Fixed Date Range: January 1, 2019 - December 31, 2019"),
      shiny::h4(shiny::strong("THE DATA ON THIS TAB DOES NOT SHOW CHANGES MADE ON OR AFTER
      JULY 21, 2020.")),
      shiny::h4(input$pe_provider),
      shiny::hr(),
      shiny::h5(shiny::strong("Next Due Date:"),
                format(lubridate::ymd(next_thing_due$DueDate), "%A %b %e, %Y"),
                "| ",
                next_thing_due$Event),
      p("Please consult the",
        a("CoC Competition Specifications and Timeline",
          href = "https://cohhio.org/boscoc/coc-program/"),
        "for complete specifications and timeline.")
    )
  })
  
  output$pe_ProjectSummary <-
    DT::renderDataTable({
      ptc <- summary_pe_final_scoring %>%
        dplyr::filter(AltProjectName == input$pe_provider) %>%
        dplyr::pull(ProjectType)
      
      summary_pe_final_scoring <- summary_pe_final_scoring %>%
        dplyr::mutate(
          ExitsToPHMath = stringr::str_replace(ExitsToPHMath, "/", "÷"),
          OwnHousingMath = stringr::str_replace(OwnHousingMath, "/", "÷"),
          IncreasedIncomeMath = stringr::str_replace(IncreasedIncomeMath, "/", "÷"),
          BenefitsAtExitMath = stringr::str_replace(BenefitsAtExitMath, "/", "÷"),
          AverageLoSMath = stringr::str_replace(AverageLoSMath, "/", "÷"),
          LHResPriorMath = stringr::str_replace(LHResPriorMath, "/", "÷"),
          NoIncomeAtEntryMath = stringr::str_replace(NoIncomeAtEntryMath, "/", "÷"),
          MedianHHIMath = stringr::str_replace(MedianHHIMath, "/", "÷"),
          LongTermHomelessMath = stringr::str_replace(LongTermHomelessMath, "/", "÷"),
          ScoredAtEntryMath = stringr::str_replace(ScoredAtEntryMath, "/", "÷"),
          DQMath = stringr::str_replace(DQMath, "/", "÷"),
          CostPerExitMath = stringr::str_replace(CostPerExitMath, "/", "÷"),
          HousingFirstMath = stringr::str_replace(HousingFirstMath, "/", "÷"),
          ChronicPrioritizationMath = stringr::str_replace(ChronicPrioritizationMath, "/", "÷"),
          OnTrackSpendingMath = stringr::str_replace(OnTrackSpendingMath, "/", "÷"),
          UnspentFundsMath = stringr::str_replace(UnspentFundsMath, "/", "÷")
        )
      
      a <- summary_pe_final_scoring %>%
        dplyr::filter(AltProjectName == input$pe_provider) %>%
        dplyr::select(
          "Exits to Permanent Housing" = ExitsToPHPoints,
          "Moved into Own Housing" = OwnHousingPoints,
          "Increased Income" = IncreasedIncomePoints,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPoints,
          "Average Length of Stay" = AverageLoSPoints,
          "Living Situation at Entry" = LHResPriorPoints,
          "No Income at Entry" = NoIncomeAtEntryPoints,
          "Median Homeless History Index" = MedianHHIPoints,
          "Long Term Homeless" = LongTermHomelessPoints,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryPoints,
          "Data Quality" = DQPoints,
          "Cost per Exit" = CostPerExitScore,
          "Housing First" = HousingFirstScore,
          "Prioritization of Chronic" = ChronicPrioritizationScore,
          "Spending On Track" = OnTrackSpendingScoring,
          "Unspent Funds within Range" = UnspentFundsScoring
        ) %>%
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = "Measure",
                            values_to = "Estimated Score")
      
      b <- summary_pe_final_scoring %>%
        dplyr::filter(AltProjectName == input$pe_provider) %>%
        dplyr::select(
          "Exits to Permanent Housing" = ExitsToPHDQ,
          "Moved into Own Housing" = OwnHousingDQ,
          "Increased Income" = IncreasedIncomeDQ,
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
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = "Measure",
                            values_to = "DQflag")
      
      c <- summary_pe_final_scoring %>%
        dplyr::filter(AltProjectName == input$pe_provider) %>%
        dplyr::select(
          "Exits to Permanent Housing" = ExitsToPHPossible,
          "Moved into Own Housing" = OwnHousingPossible,
          "Increased Income" = IncreasedIncomePossible,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
          "Average Length of Stay" = AverageLoSPossible,
          "Living Situation at Entry" = LHResPriorPossible,
          "No Income at Entry" = NoIncomeAtEntryPossible,
          "Median Homeless History Index" = MedianHHIPossible,
          "Long Term Homeless" = LongTermHomelessPossible,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryPossible,
          "Data Quality" = DQPossible,
          "Cost per Exit" = CostPerExitPossible,
          "Housing First" = HousingFirstPossible,
          "Prioritization of Chronic" = ChronicPrioritizationPossible,
          "Spending On Track" = OnTrackSpendingPossible,
          "Unspent Funds within Range" = UnspentFundsPossible
        ) %>%
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = "Measure",
                            values_to = "Possible Score")
      
      d <- summary_pe_final_scoring %>%
        dplyr::filter(AltProjectName == input$pe_provider) %>%
        dplyr::select(
          "Exits to Permanent Housing" = ExitsToPHMath,
          "Moved into Own Housing" = OwnHousingMath,
          "Increased Income" = IncreasedIncomeMath,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
          "Average Length of Stay" = AverageLoSMath,
          "Living Situation at Entry" = LHResPriorMath,
          "No Income at Entry" = NoIncomeAtEntryMath,
          "Median Homeless History Index" = MedianHHIMath,
          "Long Term Homeless" = LongTermHomelessMath,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryMath,
          "Data Quality" = DQMath,
          "Cost per Exit" = CostPerExitMath,
          "Housing First" = HousingFirstMath,
          "Prioritization of Chronic" = ChronicPrioritizationMath,
          "Spending On Track" = OnTrackSpendingMath,
          "Unspent Funds within Range" = UnspentFundsMath
        ) %>%
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = "Measure",
                            values_to = "Calculation")
      
      psh <- a %>% dplyr::left_join(b, by = "Measure") %>%
        dplyr::ungroup() %>%
        dplyr::left_join(c, by = "Measure") %>%
        dplyr::left_join(d, by = "Measure") %>%
        dplyr::mutate(
          DQ = dplyr::case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        dplyr::filter(!Measure %in% c("Moved into Own Housing",
                                      "Average Length of Stay"),
                      Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
        dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      rrh <- a %>% dplyr::left_join(b, by = "Measure") %>%
        dplyr::ungroup() %>%
        dplyr::left_join(c, by = "Measure") %>%
        dplyr::left_join(d, by = "Measure") %>%
        dplyr::mutate(
          DQ = dplyr::case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        dplyr::filter(!Measure %in%
                        c("Long Term Homeless",
                          "Prioritization of Chronic"),
                      Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
        dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      th <- a %>% dplyr::left_join(b, by = "Measure") %>%
        dplyr::ungroup() %>%
        dplyr::left_join(c, by = "Measure") %>%
        dplyr::left_join(d, by = "Measure") %>%
        dplyr::mutate(
          DQ = dplyr::case_when(
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        dplyr::filter(!Measure %in% c(
          "Long Term Homeless",
          "Prioritization of Chronic"
        ),
        Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
        dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      sh <- a %>% dplyr::left_join(b, by = "Measure") %>%
        dplyr::ungroup() %>%
        dplyr::left_join(c, by = "Measure") %>%
        dplyr::left_join(d, by = "Measure") %>%
        dplyr::mutate(
          DQ = dplyr::case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        dplyr::filter(!Measure %in% c(
          "Long Term Homeless",
          "VISPDAT Completion at Entry",
          "Prioritization of Chronic"
        ),
        Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
        dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      DT::datatable(
        if (ptc == 3) {
          psh
        } else if (ptc == 13) {
          rrh
        } else if(ptc == 2) {
          th
        } else {
          sh
        },
        rownames = FALSE,
        options = list(dom = 't',
                       pageLength = 100)
      )
    })
  
  shiny::observeEvent(c(input$providerList), {
    output$currentUnitUtilization <-
      if (nrow(utilization %>%
               dplyr::filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Unit Utilization",
            subtitle = paste(
              utilization %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(Households),
              "Households in",
              utilization %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(UnitCount),
              "Units"
            ),
            color = "aqua",
            icon = shiny::icon("building"),
            value = utilization %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(UnitUtilization)
          )
        })
      }
    else{
      
    }
    
    output$currentBedUtilization <-
      if (nrow(utilization %>%
               dplyr::filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1:3, 8, 9)
               )) > 0) {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Bed Utilization",
            subtitle = paste(
              utilization %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(Clients),
              "Clients in",
              utilization %>%
                dplyr::filter(ProjectName == input$providerList) %>%
                dplyr::select(BedCount),
              "Beds"
            ),
            color = "purple",
            icon = shiny::icon("bed"),
            utilization %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(BedUtilization)
          )
        })
      }
    else{
      
    }
    
    output$headerSPMs <- shiny::renderUI({
      ReportStart <- spm_current_start_date
      ReportEnd <- spm_current_end_date - lubridate::days(1)
      
      ReportStart <- format.Date(lubridate::ymd(ReportStart), "%B %d, %Y")
      ReportEnd <- format.Date(lubridate::ymd(ReportEnd), "%B %d, %Y")
      
      PriorReportStart <- spm_prior_start_date
      PriorReportEnd <- spm_prior_end_date - lubridate::days(1)
      
      PriorReportStart <- format.Date(lubridate::ymd(PriorReportStart), "%B %d, %Y")
      PriorReportEnd <- format.Date(lubridate::ymd(PriorReportEnd), "%B %d, %Y")
      
      list(
        shiny::h2("HUD System Performance Measures"),
        shiny::h4("Ohio Balance of State Continuum of Care"),
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
    
    output$veteranEngagement <-
      if (nrow(
        veteran_current_in_project %>%
        dplyr::filter(ProjectName == input$providerList) %>%
        dplyr::select(Veterans)
      ) > 0) {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Veterans",
            subtitle = veteran_current_in_project %>%
              dplyr::filter(ProjectName == input$providerList) %>% 
              dplyr::pull(Summary),
            color = "green",
            icon = shiny::icon("ribbon"),
            veteran_current_in_project %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(Veterans)
          )
        })
      }
    else {
      
    }
    
    output$TAYEngagement <-
      if (nrow(
        current_tay_hohs %>%
        dplyr::filter(ProjectName == input$providerList)
      ) > 0) {
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = "Current Transition Aged Youth Households",
            subtitle = current_tay_hohs %>%
              dplyr::filter(ProjectName == input$providerList) %>% dplyr::pull(Summary),
            color = "black",
            icon = shiny::icon("id-card"),
            current_tay_hohs %>%
              dplyr::filter(ProjectName == input$providerList) %>%
              dplyr::select(TAYHHs)
          )
        })
      }
    else {
      
    }
    
    output$CurrentlyAwaitingPH <-
      if (nrow(validation %>%
               dplyr::filter(ProjectType %in% c(3, 9, 13) &
                             ProjectName == input$providerList)) > 0) {
        shinydashboard::renderInfoBox({
          hhs <- nrow(
            validation %>%
              dplyr::filter(
                ProjectName == input$providerList &
                  is.na(MoveInDateAdjust) &
                  is.na(ExitDate)
              ) %>%
              dplyr::select(HouseholdID) %>%
              unique()
          )
          
          daysWaiting <- validation %>%
            dplyr::filter(ProjectName == input$providerList &
                            is.na(MoveInDateAdjust) &
                            is.na(ExitDate)) %>%
            dplyr::mutate(Waiting = as.numeric(lubridate::mdy(FileEnd) - lubridate::ymd(EntryDate))) %>%
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
      if (nrow(validation %>%
               dplyr::filter(
                 ProjectType %in% c(12, 13, 4) &
                 ProjectName == input$providerList
               )) > 0) {
        current <- validation %>%
          dplyr::filter(ProjectName == input$providerList &
                          is.na(ExitDate)) %>%
          dplyr::select(PersonalID) %>%
          unique()
        
        movedin <- validation %>%
          dplyr::filter(
            ProjectName == input$providerList &
              is.na(ExitDate) &
              !is.na(MoveInDateAdjust) &
              ProjectType == 13
          ) %>%
          dplyr::select(PersonalID) %>%
          unique()
        
        PTC <- validation %>%
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
      if (nrow(validation %>%
               dplyr::filter(
                 ProjectType %in% c(12, 13, 4) &
                 ProjectName == input$providerList
               )) > 0) {
        current <- validation %>%
          dplyr::filter(ProjectName == input$providerList &
                          is.na(ExitDate)) %>%
          dplyr::select(HouseholdID) %>%
          unique()
        
        movedin <- validation %>%
          dplyr::filter(
            ProjectName == input$providerList &
              is.na(ExitDate) &
              !is.na(MoveInDateAdjust) &
              ProjectType == 13
          ) %>%
          dplyr::select(HouseholdID) %>%
          unique()
        
        PTC <- validation %>%
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
      if (nrow(validation %>%
               dplyr::filter(ProjectType == 1 &
                             ProjectName == input$providerList)) > 0) {
        ReportStart <-
          format.Date(lubridate::floor_date(lubridate::today(), unit = "year"), "%m-%d-%Y")
        
        shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            title = paste("Client Exits to Rapid Rehousing in", 
                          lubridate::year(lubridate::mdy(FileEnd))),
            subtitle = "Destination: Rental by client, with RRH...",
            color = "light-blue",
            icon = shiny::icon("door-open"),
            nrow(
              validation %>%
                dplyr::filter(
                  ProjectName == input$providerList &
                    exited_between(., ReportStart, FileEnd) &
                    Destination == 31
                )
            )
          )
        })
      }
    else{
      
    }
    
  })
  output$covidPrioritization <- shiny::renderPlot({
    get_res_prior <- validation %>%
      dplyr::select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
      dplyr::group_by(PersonalID) %>%
      dplyr::arrange(dplyr::desc(EntryDate)) %>%
      dplyr::slice(1L)
    
    current_week <- lubridate::week(lubridate::today())
    
    priority <- covid19 %>%
      dplyr::left_join(get_res_prior, by = "PersonalID") %>%
      dplyr::filter(lubridate::ymd(COVID19AssessmentDate) >= lubridate::mdy("04012020") &
                      lubridate::ymd(COVID19AssessmentDate) <= lubridate::today()) %>%
      dplyr::mutate(
        Priority = dplyr::case_when(
          # if tested positive
          (
            Tested == 1 &
              TestResults == "Positive" &
              lubridate::ymd(TestDate) > lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) &
              !is.na(TestDate)
          ) |
            # if under investigation
            (
              UnderInvestigation == 1 &
                lubridate::ymd(DateUnderInvestigation) > lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14)
            ) |
            # contact with COVID-19
            (
              ContactWithConfirmedCOVID19Patient == 1 &
                (
                  lubridate::ymd(ContactWithConfirmedDate) >
                    lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) |
                    is.na(ContactWithConfirmedDate)
                )
              # compares contact date to the assessment date too since we want to
              # see severity at the time of assessment
            ) |
            (
              ContactWithUnderCOVID19Investigation == 1 &
                (
                  lubridate::ymd(ContactWithUnderInvestigationDate) >
                    lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) |
                    is.na(ContactWithUnderInvestigationDate)
                )
            ) |
            # if the client came from jail or nursing home
            (
              LivingSituation %in% c(7, 25) &
                EntryDate > lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) &
                EntryDate <= lubridate::ymd(COVID19AssessmentDate)
            ) |
            # if the client has any symptoms at all
            (
              Symptom1BreathingDifficult +
                Symptom1Cough +
                Symptom2Chills +
                Symptom2SoreThroat +
                Symptom2Fever +
                Symptom2Headache +
                Symptom2LostTasteSmell +
                Symptom2MusclePain +
                Symptom2Congestion +
                Symptom2Nausea +
                Symptom2Diarrhea +
                Symptom2Weak
            ) > 0 ~ "Needs Isolation/Quarantine",
          # if the client has any risks at all
          (
            HealthRiskHistoryOfRespiratoryIllness +
              HealthRiskChronicIllness +
              HealthRiskOver65 +
              HealthRiskKidneyDisease +
              HealthRiskImmunocompromised +
              HealthRiskSmoke > 0
          )  ~ "Has Health Risk(s)",
          TRUE ~ "No Known Risks or Exposure"
          # everyone else lands here ^
          # in the report, there will be a third level: "Not Assessed Recently"
        ),
        Priority = factor(Priority, levels = c("Needs Isolation/Quarantine", 
                                               "Has Health Risk(s)", 
                                               "No Known Risks or Exposure")),
        Week = format.Date(COVID19AssessmentDate, "%U"),
        Week = as.numeric(Week),
        Month = format.Date(COVID19AssessmentDate, "%m"),
        MonthName = format.Date(COVID19AssessmentDate, "%B")
      ) %>% 
      dplyr::filter(Week != current_week)
    
    week_names <- priority %>%
      dplyr::group_by(Week, MonthName) %>%
      dplyr::summarise(Clients = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = MonthName,
                         values_from = Clients) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        April_yn = dplyr::if_else(is.na(April), 0, 1),
        May_yn = dplyr::if_else(is.na(May), 0, 1),
        June_yn = dplyr::if_else(is.na(June), 0, 1),
        July_yn = dplyr::if_else(is.na(July), 0, 1),
        August_yn = dplyr::if_else(is.na(August), 0, 1),
        Sept_yn = dplyr::if_else(is.na(September), 0, 1),
        how_many = April_yn + May_yn + June_yn + July_yn + August_yn + Sept_yn,
        month_name = dplyr::case_when(
          how_many == 1 & April_yn == 1 ~ "April",
          how_many == 1 & May_yn == 1 ~ "May",
          how_many == 1 & June_yn == 1 ~ "June",
          how_many == 1 & July_yn == 1 ~ "July",
          how_many == 1 & August_yn == 1 ~ "August",
          how_many == 1 & Sept_yn == 1 ~ "September",
          April_yn + May_yn > 1 ~ "April-May",
          May_yn + June_yn > 1 ~ "May-June",
          June_yn + July_yn > 1 ~ "June-July",
          July_yn + August_yn > 1 ~ "July-August",
          August_yn + Sept_yn > 1 ~ "Aug-Sept"
        ),
        WeekName = paste(month_name, "Wk", Week),
        Week = as.numeric(Week)
      ) %>%
      dplyr::select(Week, WeekName)
    
    priority_plot <- priority %>%
      dplyr::select(PersonalID, Week, Priority) %>%
      dplyr::group_by(Week, Priority) %>%
      dplyr::summarise(Clients = dplyr::n()) %>%
      dplyr::left_join(week_names, by = "Week") %>%
      dplyr::arrange(Week)
    
    priority_plot %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(WeekName, Week), y = Clients,
                                   fill = Priority, label = Clients)) +
      ggplot2::scale_fill_brewer(palette = "GnBu", direction = -1) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = "Clients Assessed") +
      ggplot2::theme(legend.title=ggplot2::element_blank(),
                     legend.position = "top",
                     legend.text = ggplot2::element_text(size = 11),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 11))
  })  
  
  output$covidStatus <- shiny::renderPlot({
    get_res_prior <- validation %>%
      dplyr::select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
      dplyr::group_by(PersonalID) %>%
      dplyr::arrange(dplyr::desc(EntryDate)) %>%
      dplyr::slice(1L)
    
    current_week <- lubridate::week(lubridate::today())
    
    covid19_status <- covid19 %>%
      dplyr::left_join(get_res_prior, by = "PersonalID") %>%
      dplyr::filter(lubridate::ymd(COVID19AssessmentDate) >= lubridate::mdy("04012020") &
                      lubridate::ymd(COVID19AssessmentDate) <= lubridate::today()) %>%
      dplyr::mutate(
        COVID19Status = dplyr::case_when(
          Tested == 1 &
            TestResults == "Positive" &
            lubridate::ymd(TestDate) > lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) &
            !is.na(TestDate) ~ "Positive",
          # testing positive in the 14 days prior to assessment is the only way to
          # land in this bucket
          (
            ContactWithConfirmedCOVID19Patient == 1 &
              (
                lubridate::ymd(ContactWithConfirmedDate) >
                  lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) |
                  is.na(ContactWithConfirmedDate)
              )
            # compares contact date to date of the assessment
          ) |
            (
              ContactWithUnderCOVID19Investigation == 1 &
                (
                  lubridate::ymd(ContactWithUnderInvestigationDate) >
                    lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14) |
                    is.na(ContactWithUnderInvestigationDate)
                )
            ) |
            (
              Symptom1BreathingDifficult +
                Symptom1Cough +
                Symptom2Chills +
                Symptom2SoreThroat +
                Symptom2Fever +
                Symptom2Headache +
                Symptom2LostTasteSmell +
                Symptom2MusclePain +
                Symptom2Congestion +
                Symptom2Nausea +
                Symptom2Diarrhea +
                Symptom2Weak
            ) > 0
          |
            (
              UnderInvestigation == 1 &
                lubridate::ymd(DateUnderInvestigation) > lubridate::ymd(COVID19AssessmentDate) - lubridate::days(14)
            ) ~
            "May Have COVID-19",
          # being Under Investigation (past 14 days), any Symptom, or any Contact
          # in the 14 days prior to the assessment date will land you here ^
          TRUE ~ "No Current Indications"
          # everyone else lands here ^
        ),
        COVID19Status = factor(
          COVID19Status,
          levels = c("No Current Indications",
                     "May Have COVID-19",
                     "Positive")
        ),
        Week = format.Date(COVID19AssessmentDate, "%U"),
        Week = as.numeric(Week),
        Month = format.Date(COVID19AssessmentDate, "%m"),
        MonthName = format.Date(COVID19AssessmentDate, "%B")
      ) %>% 
      dplyr::filter(Week != current_week)
    
    week_names <- covid19_status %>%
      dplyr::group_by(Week, MonthName) %>%
      dplyr::summarise(Clients = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = MonthName,
                         values_from = Clients) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        April_yn = dplyr::if_else(is.na(April), 0, 1),
        May_yn = dplyr::if_else(is.na(May), 0, 1),
        June_yn = dplyr::if_else(is.na(June), 0, 1),
        July_yn = dplyr::if_else(is.na(July), 0, 1),
        August_yn = dplyr::if_else(is.na(August), 0, 1),
        Sept_yn = dplyr::if_else(is.na(September), 0, 1),
        how_many = April_yn + May_yn + June_yn + July_yn + August_yn + Sept_yn,
        month_name = dplyr::case_when(
          how_many == 1 & April_yn == 1 ~ "April",
          how_many == 1 & May_yn == 1 ~ "May",
          how_many == 1 & June_yn == 1 ~ "June",
          how_many == 1 & July_yn == 1 ~ "July",
          how_many == 1 & August_yn == 1 ~ "August",
          how_many == 1 & Sept_yn == 1 ~ "September",
          April_yn + May_yn > 1 ~ "April-May",
          May_yn + June_yn > 1 ~ "May-June",
          June_yn + July_yn > 1 ~ "June-July",
          July_yn + August_yn > 1 ~ "July-August",
          August_yn + Sept_yn > 1 ~ "Aug-Sept"
        ),
        WeekName = paste(month_name, "Wk", Week),
        Week = as.numeric(Week)
      ) %>%
      dplyr::select(Week, WeekName)
    
    
    plot <- covid19_status %>%
      dplyr::select(PersonalID, Week, COVID19Status) %>%
      dplyr::group_by(Week, COVID19Status) %>%
      dplyr::summarise(Clients = dplyr::n()) %>%
      dplyr::left_join(week_names, by = "Week") %>%
      dplyr::arrange(Week) %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(WeekName, Week), y = Clients,
                                   fill = COVID19Status)) +
      ggplot2::geom_bar(stat = "identity", 
                        position = ggplot2::position_stack(reverse = TRUE)) +  
      ggplot2::scale_fill_manual(values = c("#e0ecf4", "#9ebcda", "#8856a7")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = "Clients Assessed") +
      ggplot2::theme(legend.title=ggplot2::element_blank(),
                     legend.position = "top",
                     legend.text = ggplot2::element_text(size = 11),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 11))
    
    plot
    
  })
  
  output$bedPlot <-
    plotly::renderPlotly({
      ReportEnd <- lubridate::ymd(input$utilizationDate) 
      ReportStart <- lubridate::floor_date(lubridate::ymd(ReportEnd), unit = "month") -
        lubridate::years(1) +
        months(1)
      ReportingPeriod <- lubridate::interval(lubridate::ymd(ReportStart), lubridate::ymd(ReportEnd))
      
      Provider <- input$providerListUtilization
      
      bedPlot <- utilization_bed %>% 
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
      
      unitPlot <- utilization_unit %>% 
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
    
    a <- spm_1b_loth_self_report %>%
      dplyr::filter(Metric1b == "Persons in ES, SH, TH, and PH") %>%
      dplyr::mutate(AvgLoT = paste(as.integer(AvgLoT), "days"),
                    Prior_AvgLoT = paste(as.integer(Prior_AvgLoT), "days"),
                    MedLoT = paste(MedLoT, "days"),
                    Prior_MedLoT = paste(Prior_MedLoT, "days")) %>%
      dplyr::select(
        "Prior Year<br>Average" = Prior_AvgLoT,
        "Current Year<br>Average" = AvgLoT,
        "Prior Year<br>Median" = Prior_MedLoT,
        "Current Year<br>Median" = MedLoT
      )
    
    DT::datatable(a,
                  rownames = FALSE,
                  options = list(dom = 't'),
                  escape = FALSE)
    
  })
  
  output$spmRecurrence <- DT::renderDataTable({
    
    a <- spm_2_recurrence %>%
      dplyr::filter(ProjectType == "TOTAL Returns to Homelessness") %>%
      dplyr::mutate_at(dplyr::vars(-ProjectType), as.integer) %>%
      dplyr::mutate(
        Percent6moPrior = scales::percent(Prior_LessThan6mo / Prior_ExitedToPHPast2Yrs,
                                          accuracy = .1),
        Percent6moCurrent = scales::percent(LessThan6mo / ExitedToPHPast2Yrs,
                                            accuracy = .1),
        Percent2yrPrior = scales::percent((Prior_ThirteenTo24mo +
                                             Prior_SixTo12mo +
                                             Prior_LessThan6mo) / Prior_ExitedToPHPast2Yrs,
                                          accuracy = .1),
        Percent2yrCurrent = scales::percent((ThirteenTo24mo +
                                               LessThan6mo +
                                               SixTo12mo) / ExitedToPHPast2Yrs,
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
    
    a <- spm_7b1_exits_lh %>%
      dplyr::filter(Metric7b1 == "% Successful exits") %>%
      dplyr::mutate(Metric7b1 = "ES, TH, SH, RRH: Successful Exits") %>%
      dplyr::select(
        "Metric" = Metric7b1,
        "Prior Year" = PriorYear,
        "Current Year" = CurrentYear)
    
    b <- spm_7b2_exits_ph %>%
      dplyr::filter(Metric7b2 == "% Successful exits/retention") %>%
      dplyr::mutate(Metric7b2 = "PSH: Successful Exits/Retention of Housing in PSH") %>%
      dplyr::select(
        "Metric" = Metric7b2,
        "Prior Year" = PriorYear,
        "Current Year" = CurrentYear
      )
    
    c <- rbind(a, b)
    
    DT::datatable(c,
                  rownames = FALSE,
                  options = list(dom = 't'))
    
  })
  
  output$spmPIT <- DT::renderDataTable({
    
    a <- dplyr::tribble(
      ~Population, ~January2019Count, ~January2020Count,
      "Total", 3479, 3577,
      "Sheltered", 2665, 3577 - 986,
      "Veterans", 159, 162,
      "Chronic", 330, 192
    ) %>%
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
    
    DT::datatable(a,
                  rownames = FALSE,
                  options = list(dom = 't'))
    
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
      CountyAverageScores <- qpr_spdats_county %>%
        dplyr::filter(served_between(., ReportStart, ReportEnd)) %>%
        dplyr::select(CountyServed, PersonalID, Score) %>%
        dplyr::distinct() %>%
        dplyr::group_by(CountyServed) %>%
        dplyr::summarise(AverageScore = round(mean(Score), 1),
                         HHsLHinCounty = dplyr::n())
      # counting all hhs who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- qpr_spdats_project %>%
        dplyr::filter(entered_between(., ReportStart, ReportEnd)) %>%
        dplyr::group_by(CountyServed) %>%
        dplyr::summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
                         HHsHousedInCounty = dplyr::n())
      # pulling in both averages for each county plus adding Region for grouping
      Compare <-
        dplyr::full_join(CountyAverageScores,
                         CountyHousedAverageScores,
                         by = "CountyServed") %>%
        dplyr::arrange(CountyServed) %>%
        dplyr::left_join(., regions, by = c("CountyServed" = "County")) %>%
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
  
  output$headerLengthOfStay <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$LoSSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$LoSSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$LoSSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3(paste(input$radioAvgMeanLoS, "Length of Stay")),
         shiny::h4(ReportStart, "-", ReportEnd))
  })  
  
  
  # QPR Length of Stay
  # observeEvent(c(input$LoSRegionSelect, input$LoSSlider, input$radioLoSPTC),
  #              {
  output$QPRLoSPlot <-
    plotly::renderPlotly({
      ReportStart <- format.Date(lubridate::ymd(paste0(substr(
        input$LoSSlider, 1, 4
      ),
      "-01-01")), "%m-%d-%Y")
      ReportEnd <- format.Date(lubridate::mdy(paste0(
        dplyr::case_when(
          substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$LoSSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$LoSSlider, 1, 4)
      )), "%m-%d-%Y")
      
      LoSGoals <- goals %>%
        dplyr::select(-Measure) %>%
        dplyr::mutate(
          ProjectType = dplyr::case_when(
            ProjectType == 1 ~ "Emergency Shelters",
            ProjectType == 2 ~ "Transitional Housing",
            ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
            ProjectType == 4 ~ "Street Outreach",
            ProjectType == 8 ~ "Safe Haven",
            ProjectType == 12 ~ "Homelessness Prevention",
            ProjectType == 13 ~ "Rapid Rehousing"
          )
        )  %>%
        dplyr::filter(SummaryMeasure == "Length of Stay" &
                        ProjectType %in% c(input$radioLoSPTC)) %>%
        unique()
      
      LoSDetail <- qpr_leavers %>%
        dplyr::filter(((
          !is.na(MoveInDateAdjust) &
            ProjectType == 13
        ) |
          (
            ProjectType %in% c(1, 2, 8) &
              !is.na(ExitDate)
          )) &
          exited_between(., ReportStart, ReportEnd)) %>%
        dplyr::mutate(
          ProjectType = dplyr::case_when(
            ProjectType == 1 ~ "Emergency Shelters",
            ProjectType == 2 ~ "Transitional Housing",
            ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
            ProjectType == 4 ~ "Street Outreach",
            ProjectType == 8 ~ "Safe Haven",
            ProjectType == 12 ~ "Homelessness Prevention",
            ProjectType == 13 ~ "Rapid Rehousing"
          )
        ) %>%
        dplyr::filter(
          ProjectRegion %in% c(input$LoSRegionSelect) &
            ProjectType %in% c(input$radioLoSPTC)
        ) # this filter needs
      # to be here so the selection text matches the mutated data
      TotalLeavers <- LoSDetail %>%
        dplyr::group_by(FriendlyProjectName) %>%
        dplyr::summarise(Leavers = dplyr::n())
      
      title <-
        paste0(
          "Length of Stay (",
          input$radioAvgMeanLoS,
          ")\n",
          input$radioLoSPTC,
          "\n",
          ReportStart,
          " to ",
          ReportEnd
        )
      
      LoSSummary <- LoSDetail %>%
        dplyr::group_by(FriendlyProjectName,
                        ProjectRegion,
                        ProjectCounty,
                        ProjectType) %>%
        dplyr::summarise(
          Days = dplyr::case_when(
            input$radioAvgMeanLoS == "Average Days" ~
              as.numeric(mean(DaysinProject)),
            input$radioAvgMeanLoS == "Median Days" ~
              as.numeric(stats::median(DaysinProject))
          )
        ) %>%
        dplyr::left_join(LoSGoals, by = "ProjectType") %>%
        dplyr::left_join(TotalLeavers, by = ("FriendlyProjectName")) %>%
        dplyr::mutate(
          hover = paste0(
            FriendlyProjectName,
            "\nTotal Leavers: ",
            Leavers,
            "\nDays: ",
            Days,
            sep = "\n"
          )
        )
      
      if (nrow(LoSDetail) > 0) {
        plotly::plot_ly(
          data = LoSSummary,
          x = ~ FriendlyProjectName,
          y = ~ Days,
          text = ~ hover,
          hoverinfo = 'text'
        ) %>%
          plotly::add_trace(type = "bar") %>%
          plotly::layout(
            shapes = list(
              type = "rect",
              name = "CoC Goal",
              fillcolor = "#008000",
              line = list(color = "white"),
              layer = "below",
              xref = "paper",
              yref = "y",
              x0 = 0,
              x1 = 1,
              y0 = 0,
              y1 = ~ Goal[1],
              opacity = .2
            ),
            title = list(text = title,
                         font = list(size = 15)),
            margin = list(
              l = 50,
              r = 50,
              b = 100,
              t = 100,
              pad = 4
            ),
            yaxis = list(
              title = "Days",
              rangemode = "tozero",
              showgrid = TRUE
            ),
            xaxis = list(
              title = "",
              showgrid = TRUE,
              rangemode = "tozero"
            )
          )
      }
      else {
        
      }
    })
  # })
  
  # QPR Exits to PH 
  
  output$headerQPRExitsToPH <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$ExitsToPHSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$ExitsToPHSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      shiny::h2("Quarterly Performance Report"),
      shiny::h3("Exits to Permanent Housing"),
      shiny::h4(ReportStart, "-", ReportEnd)
    )
  })  
  
  # observeEvent(
  #   c(input$ExitsToPHRegionSelect, input$ExitsToPHSlider),
  #   {
  output$ExitsToPH <- plotly::renderPlotly({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$ExitsToPHSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$ExitsToPHSlider, 1, 4)
    )), "%m-%d-%Y")
    # title changes if you pick PSH since it's looking at Stayers as well
    yAxisTitle <- dplyr::if_else(
      input$radioExitsToPHPTC !=
        "Permanent Supportive Housing",
      "Exited to Permanent Housing",
      "Remained in or Exited to PH"
    )
    # hhs that achieved the goal
    SuccessfullyPlaced <- qpr_leavers %>%
      dplyr::filter(((
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust)
      ) |
        ProjectType %in% c(1, 2, 4, 8, 12)) &
        # excluding non-mover-inners
        (((DestinationGroup == "Permanent" |
             #exited to ph or still in PSH/HP
             is.na(ExitDate)) &
            ProjectType %in% c(3, 9, 12) &
            served_between(., ReportStart, ReportEnd)# PSH & HP
        ) |
          (
            DestinationGroup == "Permanent" & # exited to ph
              ProjectType %in% c(1, 2, 4, 8, 13) &
              exited_between(., ReportStart, ReportEnd)
          )
        )) %>% # ES, TH, SH, RRH, OUT) %>%
      dplyr::group_by(FriendlyProjectName, 
                      ProjectType, 
                      ProjectCounty, 
                      ProjectRegion) %>%
      dplyr::summarise(SuccessfullyPlacedHHs = dplyr::n())
    
    # calculating the total households to compare successful placements to
    TotalHHsSuccessfulPlacement <- qpr_leavers %>%
      dplyr::filter((
        served_between(., ReportStart, ReportEnd) &
          ProjectType %in% c(3, 9, 12) # PSH & HP
      ) |
        (
          exited_between(., ReportStart, ReportEnd) &
            ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
        )) %>%
      dplyr::group_by(FriendlyProjectName, 
                      ProjectType, 
                      ProjectCounty, 
                      ProjectRegion) %>%
      dplyr::summarise(TotalHHs = dplyr::n()) # For PSH & HP, it's total hhs served;
    # otherwise, it's total hhs *exited* during the reporting period
    
    SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
      dplyr::left_join(
        SuccessfullyPlaced,
        by = c("FriendlyProjectName", 
               "ProjectType", 
               "ProjectCounty", 
               "ProjectRegion")
      ) %>%
      dplyr::mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)
    
    SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0
    
    PlacementGoal <-
      goals %>%
      dplyr::filter(
        SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
          Measure != "Exits to Temporary or Permanent Housing"
      )
    
    title <- paste0("Exits to Permanent Housing\n", 
                    input$radioExitsToPHPTC, "\n",
                    ReportStart, " to ", ReportEnd)
    
    region <- input$ExitsToPHRegionSelect
    # translating the project type from radiobutton to numeric
    # since PSH is both 3 and 9, we have to account for that
    x <- c(1, 2, 3, 4, 8, 9, 12, 13)
    y <- c("Emergency Shelters", "Transitional Housing", 
           "Permanent Supportive Housing", "Street Outreach", "Safe Haven",
           "Permanent Supportive Housing", "Prevention",  "Rapid Rehousing")
    PTC <- as.data.frame(cbind(x, y))
    ptc <- PTC %>% dplyr::filter(y == input$radioExitsToPHPTC) %>% dplyr::select(x)
    ptc <- purrr::as_vector(ptc)
    
    stagingExitsToPH <- SuccessfulPlacement %>%
      dplyr::left_join(PlacementGoal, by = "ProjectType") %>%
      dplyr::filter(ProjectType %in% ptc, ProjectRegion %in% region) %>%
      dplyr::mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nExited to PH: ", SuccessfullyPlacedHHs, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingExitsToPH) > 0) {
      plotly::plot_ly(
        stagingExitsToPH,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = yAxisTitle,
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
          title = "Obtaining and Maintaining Permanent Housing"
        )}
    else{
      
    }
  })
  
  output$ExitsToPHOutreach <- plotly::renderPlotly({
    if (input$radioExitsToPHPTC == "Street Outreach") {
      ReportStart <- format.Date(lubridate::ymd(paste0(
        substr(input$ExitsToPHSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      ReportEnd <- format.Date(lubridate::mdy(paste0(
        dplyr::case_when(
          substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$ExitsToPHSlider, 1, 4)
      )), "%m-%d-%Y")
      
      totalServed <- qpr_leavers %>%
        dplyr::filter(exited_between(., ReportStart, ReportEnd) &
                        ProjectType == 4) %>%
        dplyr::group_by(FriendlyProjectName,
                        ProjectType,
                        ProjectCounty,
                        ProjectRegion) %>%
        dplyr::summarise(TotalHHs = dplyr::n())
      
      notUnsheltered <- qpr_leavers %>%
        dplyr::filter(
          ProjectType == 4 &
            Destination != 16 &
            DestinationGroup %in% c("Temporary", "Permanent") &
            exited_between(., ReportStart, ReportEnd)
        ) %>%
        dplyr::group_by(FriendlyProjectName,
                        ProjectType,
                        ProjectCounty,
                        ProjectRegion) %>%
        dplyr::summarise(NotUnsheltered = dplyr::n())
      
      goalOutreach <- goals %>%
        dplyr::filter(Measure == "Exits to Temporary or Permanent Housing") %>%
        dplyr::select(Goal, ProjectType)
      
      notUnsheltered <- notUnsheltered %>%
        dplyr::left_join(goalOutreach, by = "ProjectType") %>%
        dplyr::left_join(
          totalServed,
          by = c(
            "FriendlyProjectName",
            "ProjectType",
            "ProjectCounty",
            "ProjectRegion"
          )
        ) %>%
        dplyr::mutate(
          Percent = NotUnsheltered / TotalHHs,
          hover = paste0(
            FriendlyProjectName,
            "\nExited to Temp or PH: ",
            NotUnsheltered,
            "\nTotal Households: ",
            TotalHHs,
            "\n",
            as.integer(Percent * 100),
            "%",
            sep = "\n"
          )
        ) %>%
        dplyr::filter(ProjectRegion %in% c(input$ExitsToPHRegionSelect))
      
      title <-
        paste0(
          "Exits to Temporary or Permanent Housing\n",
          "Street Outreach\n",
          ReportStart,
          " to ",
          ReportEnd
        )
      if (nrow(notUnsheltered) > 0) {
        plotly::plot_ly(
          notUnsheltered,
          x = ~ FriendlyProjectName,
          y = ~ Percent,
          text = ~ hover,
          hoverinfo = 'text',
          type = "bar"
        ) %>%
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Exited to Temporary or Permanent Housing",
                         tickformat = "%"),
            title = list(text = title,
                         font = list(size = 15)),
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
            title = "Obtaining and Maintaining Permanent Housing"
          )
        
      }
      else{
        NULL
      }
    }
  })
  
  # })
  
  # QPR NonCash Benefits
  
  output$headerQPRNCBs <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$QPRNCBDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$QPRNCBDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRNCBDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRNCBDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3("Access to Mainstream Benefits: Non-cash Benefits"),
         shiny::h4(ReportStart, "-", ReportEnd))
  })  
  
  output$QPRNCBs <- plotly::renderPlotly({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$QPRNCBDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$QPRNCBDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRNCBDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRNCBDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    meeting_objective <- qpr_benefits %>%
      dplyr::filter(
        ProjectRegion %in% input$QPRNCBRegionSelect &
          ProjectType == input$radioQPR_NCB_PTC &
          exited_between(., ReportStart, ReportEnd) &
          BenefitsFromAnySource == 1
      ) %>% 
      dplyr::group_by(FriendlyProjectName, 
                      ProjectType, 
                      ProjectCounty, 
                      ProjectRegion) %>%
      dplyr::summarise(BenefitsAtExit = dplyr::n())
    
    # calculating the total households for comparison
    all_hhs <- qpr_benefits %>%
      dplyr::filter(ProjectRegion %in% c(input$QPRNCBRegionSelect) &
                      ProjectType == input$radioQPR_NCB_PTC &
                      exited_between(., ReportStart, ReportEnd)) %>%
      dplyr::group_by(FriendlyProjectName, 
                      ProjectType, 
                      ProjectCounty, 
                      ProjectRegion) %>%
      dplyr::summarise(TotalHHs = dplyr::n()) 
    
    NCBsAtExit <- all_hhs %>%
      dplyr::left_join(
        meeting_objective,
        by = c("FriendlyProjectName", 
               "ProjectType", 
               "ProjectCounty", 
               "ProjectRegion")
      )
    
    NCBsAtExit[is.na(NCBsAtExit)] <- 0
    
    NCBsAtExit <- NCBsAtExit %>%
      dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
    
    NCBGoal <-
      goals %>%
      dplyr::filter(Measure == "Non-cash Benefits") %>%
      dplyr::mutate(ProjectType = dplyr::case_when(
        ProjectType == 1 ~ "Emergency Shelters", 
        ProjectType == 2 ~ "Transitional Housing", 
        ProjectType == 3 ~ "Permanent Supportive Housing", 
        ProjectType == 4 ~ "Street Outreach", 
        ProjectType == 8 ~ "Safe Haven",
        ProjectType == 9 ~ "Permanent Supportive Housing", 
        ProjectType == 12 ~ "Prevention",  
        ProjectType == 13 ~ "Rapid Rehousing"
      )) %>% unique()
    
    title <- paste0("Non-Cash Benefits at Exit\n", 
                    input$radioQPR_NCB_PTC, "\n",
                    ReportStart, " to ", ReportEnd)
    
    region <- c(input$QPRNCBRegionSelect)
    
    stagingNCBs <- NCBsAtExit %>%
      dplyr::left_join(NCBGoal, by = "ProjectType") %>%
      dplyr::filter(ProjectType == input$radioQPR_NCB_PTC & 
                      ProjectRegion %in% region) %>%
      dplyr::mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nReceiving Non-Cash Benefits at Exit: ", BenefitsAtExit, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingNCBs) > 0) {
      plotly::plot_ly(
        stagingNCBs,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Households",
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
          title = "Accessing Mainstream Benefits: Non-Cash Benefits at Exit"
        )}
    else{
      
    }
  })
  
  # QPR Health Insurance
  
  output$headerQPRHI <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$QPRHIDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$QPRHIDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRHIDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRHIDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3("Access to Mainstream Benefits: Health Insurance"),
         shiny::h4(ReportStart, "-", ReportEnd))
  })  
  
  output$QPRHIs <- plotly::renderPlotly({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$QPRHIDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$QPRHIDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRHIDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRHIDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    meeting_objective <- qpr_benefits %>%
      dplyr::filter(
        ProjectRegion %in% input$QPRHIRegionSelect &
          ProjectType == input$radioQPR_HI_PTC &
          exited_between(., ReportStart, ReportEnd) &
          InsuranceFromAnySource == 1
      ) %>% 
      dplyr::group_by(FriendlyProjectName,
                      ProjectType,
                      ProjectCounty,
                      ProjectRegion) %>%
      dplyr::summarise(InsuranceAtExit = dplyr::n())
    
    # calculating the total households for comparison
    all_hhs <- qpr_benefits %>%
      dplyr::filter(ProjectRegion %in% input$QPRHIRegionSelect &
                      ProjectType == input$radioQPR_HI_PTC &
                      exited_between(., ReportStart, ReportEnd)) %>%
      dplyr::group_by(FriendlyProjectName,
                      ProjectType,
                      ProjectCounty,
                      ProjectRegion) %>%
      dplyr::summarise(TotalHHs = dplyr::n()) 
    
    HIAtExit <- all_hhs %>%
      dplyr::left_join(
        meeting_objective,
        by = c("FriendlyProjectName",
               "ProjectType",
               "ProjectCounty",
               "ProjectRegion")
      )
    
    HIAtExit[is.na(HIAtExit)] <- 0
    
    HIAtExit <- HIAtExit %>%
      dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
    
    HIGoal <-
      goals %>%
      dplyr::filter(Measure == "Health Insurance at Exit") %>%
      dplyr::mutate(ProjectType = dplyr::case_when(
        ProjectType == 1 ~ "Emergency Shelters", 
        ProjectType == 2 ~ "Transitional Housing", 
        ProjectType == 3 ~ "Permanent Supportive Housing", 
        ProjectType == 4 ~ "Street Outreach", 
        ProjectType == 8 ~ "Safe Haven",
        ProjectType == 9 ~ "Permanent Supportive Housing", 
        ProjectType == 12 ~ "Prevention",  
        ProjectType == 13 ~ "Rapid Rehousing"
      )) %>% unique()
    
    title <- paste0("Health Insurance at Exit\n", 
                    input$radioQPR_NCB_PTC, "\n",
                    ReportStart, " to ", ReportEnd)
    
    region <- c(input$QPRHIRegionSelect)
    
    stagingHI <- HIAtExit %>%
      dplyr::left_join(HIGoal, by = "ProjectType") %>%
      dplyr::filter(ProjectType == input$radioQPR_HI_PTC & 
                      ProjectRegion %in% region) %>%
      dplyr::mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nHealth Insurance at Exit: ", InsuranceAtExit, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingHI) > 0) {
      plotly::plot_ly(
        stagingHI,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Households",
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
          title = "Accessing Mainstream Benefits: Health Insurance at Exit"
        )}
    else{
      
    }
  })
  
  # QPR Increase Income
  
  output$headerQPRIncome <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$QPRIncomeDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$QPRIncomeDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRIncomeDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3("Access to Mainstream Benefits: Increased Income"),
         shiny::h4(ReportStart, "-", ReportEnd))
  })  
  
  output$QPRIncome <- plotly::renderPlotly({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$QPRIncomeDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$QPRIncomeDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRIncomeDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    meeting_objective <- qpr_income %>%
      dplyr::filter(
        ProjectRegion %in% input$QPRIncomeRegionSelect &
          ProjectType == input$radioQPR_Income_PTC &
          stayed_between(., ReportStart, ReportEnd) &
          Difference > 0
      ) %>% 
      dplyr::group_by(FriendlyProjectName,
                      ProjectType,
                      ProjectCounty,
                      ProjectRegion) %>%
      dplyr::summarise(Increased = dplyr::n())
    
    # calculating the total households for comparison
    all_hhs <- qpr_income %>%
      dplyr::filter(ProjectRegion %in% input$QPRIncomeRegionSelect &
                      ProjectType == input$radioQPR_Income_PTC &
                      stayed_between(., ReportStart, ReportEnd)) %>%
      dplyr::group_by(FriendlyProjectName,
                      ProjectType,
                      ProjectCounty,
                      ProjectRegion) %>%
      dplyr::summarise(TotalHHs = dplyr::n()) 
    
    IncreasedIncome <- all_hhs %>%
      dplyr::left_join(
        meeting_objective,
        by = c("FriendlyProjectName",
               "ProjectType",
               "ProjectCounty",
               "ProjectRegion")
      )
    
    IncreasedIncome[is.na(IncreasedIncome)] <- 0
    
    IncreasedIncome <- IncreasedIncome %>%
      dplyr::mutate(Percent = Increased / TotalHHs)
    
    IncomeGoal <-
      goals %>%
      dplyr::filter(Measure == "Gain or Increase Income") %>%
      dplyr::mutate(ProjectType = dplyr::case_when(
        ProjectType == 1 ~ "Emergency Shelters", 
        ProjectType == 2 ~ "Transitional Housing", 
        ProjectType == 3 ~ "Permanent Supportive Housing", 
        ProjectType == 4 ~ "Street Outreach", 
        ProjectType == 8 ~ "Safe Haven",
        ProjectType == 9 ~ "Permanent Supportive Housing", 
        ProjectType == 12 ~ "Prevention",  
        ProjectType == 13 ~ "Rapid Rehousing"
      )) %>% unique()
    
    title <- paste0("Increased Income\n", 
                    input$radioQPR_Income_PTC, "\n",
                    ReportStart, " to ", ReportEnd)
    
    region <- c(input$QPRIncomeRegionSelect)
    
    stagingIncome <- IncreasedIncome %>%
      dplyr::left_join(IncomeGoal, by = "ProjectType") %>%
      dplyr::filter(ProjectType == input$radioQPR_Income_PTC &
                      ProjectRegion %in% region) %>%
      dplyr::mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nIncreased Income: ", Increased, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingIncome) > 0) {
      plotly::plot_ly(
        stagingIncome,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Households",
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
          title = "Accessing Mainstream Benefits: Health Insurance at Exit"
        )}
    else{
      
    }
  })
  
  output$headerRRHRapidPlacement <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$RapidRRHDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RapidRRHDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3("Rapid Placement into Rapid Rehousing"),
         shiny::h4(ReportStart, "-", ReportEnd))
  })  
  
  # QPR Rapid Placement into RRH
  output$DaysToHouse <- 
    plotly::renderPlotly({
      
      ReportStart <- format.Date(lubridate::ymd(paste0(
        substr(input$RapidRRHDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(lubridate::mdy(paste0(
        dplyr::case_when(
          substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RapidRRHDateSlider, 1, 4)
      )), "%m-%d-%Y")
      
      daysToHouse <- qpr_rrh_enterers %>%
        dplyr::filter(
          !is.na(MoveInDateAdjust) &
            ProjectRegion %in% c(input$RapidRRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        )
      
      RRHgoal <- goals %>%
        dplyr::filter(SummaryMeasure == "Rapid Placement") %>%
        dplyr::select(ProjectType, Goal)
      
      summaryDays <- daysToHouse %>%
        dplyr::group_by(FriendlyProjectName,
                        ProjectCounty,
                        ProjectRegion,
                        ProjectType) %>%
        dplyr::summarise(AvgDays = as.integer(mean(DaysToHouse)),
                         TotalHHs = dplyr::n()) %>%
        dplyr::left_join(RRHgoal, by = "ProjectType") %>%
        dplyr::mutate(hover = paste0(
          FriendlyProjectName,
          "\nAverage Days to House: ",
          AvgDays,
          "\nTotal Households: ",
          TotalHHs,
          sep = "\n"
        ))
      
      title <- paste0("Average Days to House\nRapid Rehousing\n",
                      ReportStart, " to ", ReportEnd)
      
      plotly::plot_ly(
        summaryDays,
        x = ~ FriendlyProjectName,
        y = ~ AvgDays,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        plotly::layout(
          xaxis = list(title = ~ FriendlyProjectName),
          yaxis = list(title = "Average Days to House"),
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
            y1 = 0,
            opacity = .2
          ),
          title = "Days to House"
        )
    })
  
  output$headerRRHSpending <- shiny::renderUI({
    ReportStart <- format.Date(lubridate::ymd(paste0(
      substr(input$RRHSpendingDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(lubridate::mdy(paste0(
      dplyr::case_when(
        substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RRHSpendingDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(shiny::h2("Quarterly Performance Report"),
         shiny::h3("Rapid Rehousing Spending Goals"),
         shiny::h4(ReportStart, "-", ReportEnd))
  })
  
  #  QPR HP vs RRH Spending
  output$RRHSpending <-
    plotly::renderPlotly({
      ReportStart <- format.Date(lubridate::ymd(paste0(
        substr(input$RRHSpendingDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(lubridate::mdy(paste0(
        dplyr::case_when(
          substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RRHSpendingDateSlider, 1, 4)
      )), "%m-%d-%Y")
      
      rrhSpending <- qpr_spending %>%
        dplyr::filter(
          !is.na(OrganizationName) &
            ProjectRegion %in% c(input$RRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        ) %>%
        dplyr::mutate(ProjectType = dplyr::if_else(ProjectType == 12,
                                                   "HP",
                                                   "RRH"),
                      ProjectType = factor(ProjectType, levels = c("HP", "RRH")))
      
      x <- qpr_spending %>%
        dplyr::filter(
          !is.na(OrganizationName) &
            ProjectRegion %in% c(input$RRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        ) %>%
        dplyr::select(OrganizationName, ProjectRegion) %>%
        unique() %>%
        dplyr::arrange(OrganizationName)
      
      x <- rbind(x, x) %>% dplyr::arrange(OrganizationName)
      
      y <- data.frame(ProjectType = c("HP", "RRH"))
      
      z <- cbind(x, y)
      
      rrhSpending <- rrhSpending %>% dplyr::right_join(z, by = c("OrganizationName",
                                                                 "ProjectRegion",
                                                                 "ProjectType")) %>%
        dplyr::mutate(PersonalID = dplyr::if_else(is.na(PersonalID), 4216, PersonalID),
                      EntryDate = dplyr::if_else(PersonalID == 4216,
                                                 lubridate::mdy(ReportStart),
                                                 lubridate::ymd(EntryDate)),
                      MoveInDateAdjust = dplyr::if_else(PersonalID == 4216,
                                                        lubridate::mdy(ReportStart),
                                                        lubridate::ymd(EntryDate)),
                      ExitDate = dplyr::if_else(PersonalID == 4216,
                                                lubridate::mdy(ReportEnd),
                                                lubridate::ymd(EntryDate)))
      
      
      rrhSpending <- rrhSpending  %>%
        dplyr::group_by(OrganizationName, ProjectRegion, ProjectType) %>%
        dplyr::summarise(Amount = sum(Amount),
                         HHs = dplyr::n()) %>%
        dplyr::ungroup() %>%
        tidyr::spread(ProjectType, Amount)
      
      rrhSpending[is.na(rrhSpending)] <- 0
      
      rrhSpending <- rrhSpending %>%
        dplyr::group_by(OrganizationName, ProjectRegion) %>%
        dplyr::summarise(HHs = sum(HHs),
                         RRH = sum(RRH),
                         HP = sum(HP)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(HP > 0 | RRH > 0) %>%
        dplyr::mutate(Goal = 0.75,
                      PercentRRH = RRH/(RRH + HP),
                      hover = paste0(
                        OrganizationName,
                        "\nPercent Spent on RRH: ",
                        scales::percent(PercentRRH),
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
      
      plotly::plot_ly(
        rrhSpending,
        x = ~ OrganizationName,
        y = ~ PercentRRH,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        plotly::layout(
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
          )
        )
    })
  
}
