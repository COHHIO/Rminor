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

function(input, output, session) {
  # output$res <- renderPrint({
  #  input$utilizationDate
  # cat("Length of Stay", input$LoSRegionSelect)
  # })
  output$headerUtilization <- renderUI({
    ReportEnd <- 
      ceiling_date(ymd(input$utilizationDate), unit = "month") - days(1)
    ReportStart <- 
      floor_date(ymd(ReportEnd), unit = "month") - years(1) + months(1)
    
    ReportStart <- format.Date(ymd(ReportStart), "%B %d, %Y")
    ReportEnd <- format.Date(ymd(ReportEnd), "%B %d, %Y")
    
    list(
      h2("Bed and Unit Utilization"),
      h4(input$providerListUtilization),
      h4(ReportStart, "-", ReportEnd)
    )
  })

  
  output$headerCovid19 <- renderUI({

    ReportStart <- format.Date(mdy("04012020"), "%B %d, %Y")
    ReportEnd <- format.Date(ymd_hms(update_date), "%B %d, %Y")
    
    list(
      h2("Ohio Balance of State CoC Covid-19 Data Analysis"),
      h4(ReportStart, "-", ReportEnd)
    )
  })

  output$headerCoCCompetitionProjectLevel <- renderUI({
    next_thing_due <- tribble(
      ~ DueDate, ~ Event,
      "7/20/2020", "All HMIS data corrections must be complete by 11:59pm",
      "7/21/2020", "COHHIO releases preliminary CoC project ranking (renewals only)",
      "7/31/2020", "Recipients submit appeals of project evaluation results and ranking to ohioboscoc@cohhio.org.",
      "8/7/2020", "Ohio BoSCoC Steering Committee will communicate decisions about all received appeals",
      "8/12/2020", "Final CoC project ranking released",
      "9/30/2020", "Final Ranking posted on cohhio.org"
    ) %>%
      mutate(
        DueDate = mdy(DueDate),
        ShowStart = lag(ymd(DueDate), n = 1L, order_by = DueDate),
        ShowStart = if_else(is.na(ShowStart), today(), ShowStart + days(1)),
        ShowEnd = ymd(DueDate),
        DateRange = interval(ShowStart, ShowEnd)
      ) %>%
      filter(today() %within% DateRange) %>%
      select(Event, DueDate)

    list(
      h2("2020 CoC Competition: Project Evaluation"),
      h4("Fixed Date Range: January 1, 2019 - December 31, 2019"),
      h4(strong("THE DATA ON THIS TAB DOES NOT SHOW CHANGES MADE ON OR AFTER
      JULY 21, 2020.")),
      h4(input$pe_provider),
      hr(),
      h5(strong("Next Due Date:"),
         format(ymd(next_thing_due$DueDate), "%A %b %e, %Y"),
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
        filter(AltProjectName == input$pe_provider) %>%
        pull(ProjectType)

      summary_pe_final_scoring <- summary_pe_final_scoring %>%
        mutate_at(vars(ends_with("Math")), ~str_replace(., "/", "÷"))

      a <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
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
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Estimated Score")

      b <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
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
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "DQflag")

      c <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
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
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Possible Score")

      d <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
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
                               "Average Length of Stay"),
               Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
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
                   "Prioritization of Chronic"),
               Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
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
          "Prioritization of Chronic"
        ),
        Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)

      sh <- a %>% left_join(b, by = "Measure") %>%
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
        filter(!Measure %in% c(
          "Long Term Homeless",
          "VISPDAT Completion at Entry",
          "Prioritization of Chronic"
        ),
        Calculation != "NOT SCORED in 2020 due to COVID-19.") %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)

      datatable(
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
  
  observeEvent(c(input$providerList), {
    output$currentUnitUtilization <-
      if (nrow(utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            title = "Current Unit Utilization",
            subtitle = paste(
              utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(Households),
              "Households in",
              utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(UnitCount),
              "Units"
            ),
            color = "aqua",
            icon = icon("building"),
            value = utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitUtilization)
          )
        })
      }
    else{
      
    }
    
    output$currentBedUtilization <-
      if (nrow(utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1:3, 8, 9)
               )) > 0) {
        renderInfoBox({
          infoBox(
            title = "Current Bed Utilization",
            subtitle = paste(
              utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(Clients),
              "Clients in",
              utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(BedCount),
              "Beds"
            ),
            color = "purple",
            icon = icon("bed"),
            utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedUtilization)
          )
        })
      }
    else{
      
    }
    
    output$headerSPMs <- renderUI({
      ReportStart <- spm_current_start_date
      ReportEnd <- spm_current_end_date - days(1)
      
      ReportStart <- format.Date(ymd(ReportStart), "%B %d, %Y")
      ReportEnd <- format.Date(ymd(ReportEnd), "%B %d, %Y")
      
      PriorReportStart <- spm_prior_start_date
      PriorReportEnd <- spm_prior_end_date - days(1)
      
      PriorReportStart <- format.Date(ymd(PriorReportStart), "%B %d, %Y")
      PriorReportEnd <- format.Date(ymd(PriorReportEnd), "%B %d, %Y")
      
      list(
        h2("HUD System Performance Measures"),
        h4("Ohio Balance of State Continuum of Care"),
        h4(ReportStart, "-", ReportEnd),
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
        filter(ProjectName == input$providerList) %>%
        select(Veterans)
      ) > 0) {
        renderInfoBox({
          infoBox(
            title = "Current Veterans",
            subtitle = veteran_current_in_project %>%
              filter(ProjectName == input$providerList) %>% 
              pull(Summary),
            color = "green",
            icon = icon("ribbon"),
            veteran_current_in_project %>%
              filter(ProjectName == input$providerList) %>%
              select(Veterans)
          )
        })
      }
    else {
      
    }
    
    output$TAYEngagement <-
      if (nrow(
        current_tay_hohs %>%
        filter(ProjectName == input$providerList)
      ) > 0) {
        renderInfoBox({
          infoBox(
            title = "Current Transition Aged Youth Households",
            subtitle = current_tay_hohs %>%
              filter(ProjectName == input$providerList) %>% pull(Summary),
            color = "black",
            icon = icon("id-card"),
            current_tay_hohs %>%
              filter(ProjectName == input$providerList) %>%
              select(TAYHHs)
          )
        })
      }
    else {
      
    }
    
    output$CurrentlyAwaitingPH <-
      if (nrow(validation %>%
               filter(ProjectType %in% c(3, 9, 13) &
                      ProjectName == input$providerList)) > 0) {
        renderInfoBox({
          hhs <- nrow(
            validation %>%
              filter(
                ProjectName == input$providerList &
                  is.na(MoveInDateAdjust) &
                  is.na(ExitDate)
              ) %>%
              select(HouseholdID) %>%
              unique()
          )
          
          daysWaiting <- validation %>%
            filter(ProjectName == input$providerList &
                     is.na(MoveInDateAdjust) &
                     is.na(ExitDate)) %>%
            mutate(Waiting = as.numeric(mdy(FileEnd) - ymd(EntryDate))) %>%
            group_by(ProjectID) %>%
            summarise(avgWait = as.integer(mean(Waiting)))
          
          infoBox(
            title = "Active Households Not Yet Housed",
            subtitle = paste("Average Days Waiting:", daysWaiting$avgWait),
            color = "black",
            icon = icon("pause-circle"),
            hhs
          )
        })
      }
    else{
      
    }
    
    output$CurrentClientCount <-
      if (nrow(validation %>%
               filter(
                 ProjectType %in% c(12, 13, 4) &
                 ProjectName == input$providerList
               )) > 0) {
        current <- validation %>%
          filter(ProjectName == input$providerList &
                   is.na(ExitDate)) %>%
          select(PersonalID) %>%
          unique()
        
        movedin <- validation %>%
          filter(
            ProjectName == input$providerList &
              is.na(ExitDate) &
              !is.na(MoveInDateAdjust) &
              ProjectType == 13
          ) %>%
          select(PersonalID) %>%
          unique()
        
        PTC <- validation %>%
          filter(ProjectName == input$providerList) %>%
          select(ProjectType) %>% unique()
        
        renderInfoBox({
          infoBox(
            title = "Current Clients",
            subtitle = if_else(
              PTC == 13,
              paste(nrow(movedin),
                    "client(s) housed in the project"),
              ""
            ),
            color = "fuchsia",
            icon = icon("home"),
            nrow(current)
          )
        })
      }
    else{
      
    }
    
    output$CurrentHHCount <-
      if (nrow(validation %>%
               filter(
                 ProjectType %in% c(12, 13, 4) &
                 ProjectName == input$providerList
               )) > 0) {
        current <- validation %>%
          filter(ProjectName == input$providerList &
                   is.na(ExitDate)) %>%
          select(HouseholdID) %>%
          unique()
        
        movedin <- validation %>%
          filter(
            ProjectName == input$providerList &
              is.na(ExitDate) &
              !is.na(MoveInDateAdjust) &
              ProjectType == 13
          ) %>%
          select(HouseholdID) %>%
          unique()
        
        PTC <- validation %>%
          filter(ProjectName == input$providerList) %>%
          select(ProjectType) %>% unique()
        
        renderInfoBox({
          infoBox(
            title = "Current Households",
            subtitle = if_else(
              PTC == 13,
              paste(nrow(movedin),
                    "household(s) housed in the project"),
              ""
            ),
            color = "teal",
            icon = icon("users"),
            nrow(current)
          )
        })
      }
    else{
      
    }
    
    output$ShelterExitsToRRH <-
      if (nrow(validation %>%
               filter(ProjectType == 1 &
                      ProjectName == input$providerList)) > 0) {
        ReportStart <-
          format.Date(floor_date(today(), unit = "year"), "%m-%d-%Y")
        
        renderInfoBox({
          infoBox(
            title = paste("Client Exits to Rapid Rehousing in", 
                          year(mdy(FileEnd))),
            subtitle = "Destination: Rental by client, with RRH...",
            color = "light-blue",
            icon = icon("door-open"),
            nrow(
              validation %>%
                filter(
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
  
  
output$AP_list_county <- renderDataTable({
  AP_list <- APs %>%
    filter(ProjectCountyServed %in% c(input$ap_by_county)) %>%
    select(ProjectID) %>% unique()
  
  AP_final <- APs %>%
    right_join(AP_list, by = "ProjectID") %>%
    mutate(Address = if_else(!is.na(CoCCode),
                             paste(Addresses, City, sep = '<br>'),
                             "Please call- address not available.")) %>%    
    group_by(OrgLink,
             Address,
             ProjectHours,
             ProjectTelNo) %>%
    summarise(Regions = paste(unique(ProjectAreaServed), collapse = ",<br>")) %>%
    ungroup() %>%
    unique() %>%
    select(
      "Organization" = OrgLink,
      Address,
      "Hours" = ProjectHours,
      "Phone" = ProjectTelNo,
      "Service Area(s)" = Regions
    )
  
  datatable(AP_final,
            rownames = FALSE,
            options = list(dom = 'ltpi'),
            escape = FALSE)
  
})
  
output$AP_list_region <- renderDataTable({
  AP_list <- APs %>%
    filter(ProjectAreaServed %in% c(input$ap_by_region)) %>%
    select(ProjectID) %>% unique()
  
  AP_final <- APs %>%
    right_join(AP_list, by = "ProjectID") %>%
    mutate(Address = if_else(!is.na(CoCCode),
                             paste(Addresses, City, sep = '</br>'),
                             "Please call- address not available.")) %>%
    group_by(OrgLink,
             Address,
             ProjectHours,
             ProjectTelNo) %>%
    summarise(Counties = paste(unique(ProjectCountyServed), collapse = ", ")) %>%
    ungroup() %>%
    unique() %>%
    select(
      "Organization" = OrgLink,
      Address,
      "Hours" = ProjectHours,
      "Phone" = ProjectTelNo,
      "County/-ies Served" = Counties
    )
  
  datatable(AP_final,
            rownames = FALSE,
            options = list(dom = 'ltpi'),
            escape = FALSE)
  
})

output$AP_list_org <- renderDataTable({
  AP_list <- APs %>%
    filter(ProjectAKA %in% c(input$ap_by_org)) %>%    
    select(ProjectID) %>% unique()
  
  AP_final <- APs %>%
    right_join(AP_list, by = "ProjectID") %>%
    mutate(Address = if_else(!is.na(CoCCode),
                             paste(Addresses, City, sep = '</br>'),
                             "Please call- address not available.")) %>%
    group_by(OrgLink,
             Address,
             ProjectHours,
             ProjectTelNo) %>%
    summarise(Counties = paste(unique(ProjectCountyServed), collapse = ", "),
              Regions = paste(unique(ProjectAreaServed), collapse = ",</br>")) %>%
    ungroup() %>%
    unique() %>%
    select(
      "Organization" = OrgLink,
      Address,
      "Hours" = ProjectHours,
      "Phone" = ProjectTelNo,
      "County/-ies Served" = Counties,
      "Service Area(s)" = Regions
    )
  
  datatable(AP_final,
            rownames = FALSE,
            options = list(dom = 'ltpi'),
            escape = FALSE)
  
})   
output$covidPrioritization <- renderPlot({
  covid19_priority_plot
})  

output$covidStatus <- renderPlot({
  covid19_status_plot
  
})
  
    output$bedPlot <-
    renderPlotly({
      ReportEnd <- ymd(input$utilizationDate) 
      ReportStart <- floor_date(ymd(ReportEnd), unit = "month") -
        years(1) +
        months(1)
      ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))
      
      Provider <- input$providerListUtilization
      
      bedPlot <- utilization_bed %>% 
        gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) %>%
        filter(ProjectName == Provider,
               mdy(Month) %within% ReportingPeriod) %>%
        mutate(
          Month = floor_date(mdy(Month), unit = "month"),
          Bed = Utilization,
          Utilization = NULL
        )
      
      unitPlot <- utilization_unit %>% 
        gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) %>%
        filter(ProjectName == Provider,
               mdy(Month) %within% ReportingPeriod) %>%
        mutate(
          Month = floor_date(mdy(Month), unit = "month"),
          Unit = Utilization,
          Utilization = NULL
        )
      
      utilizationPlot <- unitPlot %>%
        full_join(bedPlot,
                  by = c("ProjectID", "ProjectName", "ProjectType", "Month")) 
      
      plot_ly(utilizationPlot, 
              x = ~Month) %>%
        add_trace(y = ~ Unit,
                  name = "Unit Utilization",
                  type = "scatter",
                  mode = "lines+markers",
                  hoverinfo = 'y') %>%
        add_trace(y = ~Bed,
                  name = "Bed Utilization",
                  type = "scatter",
                  mode = "lines+markers",
                  hoverinfo = 'y') %>%
        layout(yaxis = list(
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
                      format(ymd(ReportStart), "%B %Y"), 
                      "to", 
                      format(ymd(ReportEnd), "%B %Y")))
      
    })  
  
  output$unitNote <-
    renderUI(note_unit_utilization)
  
  output$covidText <- renderUI(
    HTML("The Ohio Balance of State CoC is collecting COVID-19 data based on the 
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
    renderUI(note_bed_utilization)
  
  output$utilizationNote <-
    renderUI(HTML(note_calculation_utilization))
  
  output$spmLoTH <- DT::renderDataTable({
    
    a <- spm_1b_loth_self_report %>%
      filter(Metric1b == "Persons in ES, SH, TH, and PH") %>%
      mutate(AvgLoT = paste(as.integer(AvgLoT), "days"),
             Prior_AvgLoT = paste(as.integer(Prior_AvgLoT), "days"),
             MedLoT = paste(MedLoT, "days"),
             Prior_MedLoT = paste(Prior_MedLoT, "days")) %>%
      select(
        "Prior Year<br>Average" = Prior_AvgLoT,
        "Current Year<br>Average" = AvgLoT,
        "Prior Year<br>Median" = Prior_MedLoT,
        "Current Year<br>Median" = MedLoT
      )
    
    datatable(a,
              rownames = FALSE,
              options = list(dom = 't'),
              escape = FALSE)
    
  })
  
  output$spmRecurrence <- DT::renderDataTable({
    
    a <- spm_2_recurrence %>%
      filter(ProjectType == "TOTAL Returns to Homelessness") %>%
      mutate_at(vars(-ProjectType), as.integer) %>%
      mutate(
        Percent6moPrior = percent(Prior_LessThan6mo / Prior_ExitedToPHPast2Yrs,
                                  accuracy = .1),
        Percent6moCurrent = percent(LessThan6mo / ExitedToPHPast2Yrs,
                                    accuracy = .1),
        Percent2yrPrior = percent((Prior_ThirteenTo24mo +
                                     Prior_SixTo12mo +
                                     Prior_LessThan6mo) / Prior_ExitedToPHPast2Yrs,
                                  accuracy = .1),
        Percent2yrCurrent = percent((ThirteenTo24mo +
                                       LessThan6mo +
                                       SixTo12mo) / ExitedToPHPast2Yrs,
                                    accuracy = .1)
      ) %>%
      select(
        "Prior Year<br>Recurred in 6 months or less" = Percent6moPrior,
        "Current Year<br>Recurred in 6 months or less" = Percent6moCurrent,
        "Prior Year<br>Recurred up to 2 Years After Permanent Exit" = Percent2yrPrior,
        "Current Year<br>Recurred up to 2 Years After Permanent Exit" = Percent2yrCurrent
      )
    
    datatable(a,
              rownames = FALSE,
              options = list(dom = 't'),
              escape = FALSE)
    
  })
  
  output$spmExitsToPH <- DT::renderDataTable({
    
    a <- spm_7b1_exits_lh %>%
      filter(Metric7b1 == "% Successful exits") %>%
      mutate(Metric7b1 = "ES, TH, SH, RRH: Successful Exits") %>%
      select(
        "Metric" = Metric7b1,
        "Prior Year" = PriorYear,
        "Current Year" = CurrentYear)
    
    b <- spm_7b2_exits_ph %>%
      filter(Metric7b2 == "% Successful exits/retention") %>%
      mutate(Metric7b2 = "PSH: Successful Exits/Retention of Housing in PSH") %>%
      select(
        "Metric" = Metric7b2,
        "Prior Year" = PriorYear,
        "Current Year" = CurrentYear
      )
    
    c <- rbind(a, b)
    
    datatable(c,
              rownames = FALSE,
              options = list(dom = 't'))
    
  })
  
  output$spmPIT <- DT::renderDataTable({
    
    a <- tribble(
      ~Population, ~January2019Count, ~January2020Count,
      "Total", 3479, 3577,
      "Sheltered", 2665, 3577 - 986,
      "Veterans", 159, 162,
      "Chronic", 330, 192
    ) %>%
      mutate(
        Difference = percent((January2020Count - January2019Count)
                             /January2019Count,
                             accuracy = .1),
        Difference = if_else(str_starts(Difference, "-"),
                             Difference,
                             paste0("+", Difference))
      ) %>%
      select(Population,
             "January 2019 Count" = January2019Count,
             "January 2020 Count" = January2020Count,
             Difference)
    
    datatable(a,
              rownames = FALSE,
              options = list(dom = 't'))
    
  })
  
  output$headerQPRCommunityNeed <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$spdatSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$spdatSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$spdatSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$spdatSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$spdatSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$spdatSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3("Community Need"),
         h4(input$regionList),
         h4(ReportStart, "-", ReportEnd))
  })  
  
  output$SPDATScoresByCounty <-
    renderPlot({
      ReportStart <- format.Date(ymd(paste0(
        substr(input$spdatSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$spdatSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$spdatSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$spdatSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$spdatSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$spdatSlider, 1, 4)
      )), "%m-%d-%Y")
      # counting all hhs who were scored AND SERVED between the report dates
      CountyAverageScores <- qpr_spdats_county %>%
        filter(served_between(., ReportStart, ReportEnd)) %>%
        select(CountyServed, PersonalID, Score) %>%
        distinct() %>%
        group_by(CountyServed) %>%
        summarise(AverageScore = round(mean(Score), 1),
                  HHsLHinCounty = n())
      # counting all hhs who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- qpr_spdats_project %>%
        filter(entered_between(., ReportStart, ReportEnd)) %>%
        group_by(CountyServed) %>%
        summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
                  HHsHousedInCounty = n())
      # pulling in both averages for each county plus adding Region for grouping
      Compare <-
        full_join(CountyAverageScores,
                  CountyHousedAverageScores,
                  by = "CountyServed") %>%
        arrange(CountyServed) %>%
        left_join(., regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList)
      # the plot
      ggplot(Compare, aes(x = CountyServed, y = AverageScore)) +
        geom_point(size = 12, shape = 95) +
        scale_y_continuous(limits = c(0, 17)) +
        geom_point(
          aes(y = HousedAverageScore),
          size = 6,
          shape = 17,
          colour = "#56B4E9"
        ) +
        xlab("County Where Served") +
        ylab("Average SPDAT Score") +
        theme_light() +
        theme(
          plot.title = element_text(lineheight = 5, size = rel(1.8)),
          axis.text.x = element_text(size = rel(1.8)),
          axis.text.y = element_text(size = rel(1.8)),
          plot.margin = margin(
            t = 15,
            r = 15,
            b = 15,
            l = 15
          )
        ) +
        labs(
          title = input$regionList,
          subtitle = paste("Date Range:", ReportStart, "to", ReportEnd),
          caption = "VI-SPDAT scores and household enrollment data comes from
          the Ohio Balance of State CoC HMIS. Detail may be found at R minor
          elevated."
        )
    })
  
  output$CountyScoresText <-
    renderText(note_qpr_served_county)
  
  output$HHsServedScoresText <-
    renderText(note_qpr_housed_county)
  
  output$NoteToUsers <-
    renderText(note_qpr_dq_community_need)
  
  output$headerLengthOfStay <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$LoSSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$LoSSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$LoSSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3(paste(input$radioAvgMeanLoS, "Length of Stay")),
         h4(ReportStart, "-", ReportEnd))
  })  
  
  
  # QPR Length of Stay
  # observeEvent(c(input$LoSRegionSelect, input$LoSSlider, input$radioLoSPTC),
  #              {
  output$QPRLoSPlot <-
    renderPlotly({
      ReportStart <- format.Date(ymd(paste0(substr(
        input$LoSSlider, 1, 4
      ),
      "-01-01")), "%m-%d-%Y")
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$LoSSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$LoSSlider, 1, 4)
      )), "%m-%d-%Y")
      
      LoSGoals <- goals %>%
        select(-Measure) %>%
        mutate(
          ProjectType = case_when(
            ProjectType == 1 ~ "Emergency Shelters",
            ProjectType == 2 ~ "Transitional Housing",
            ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
            ProjectType == 4 ~ "Street Outreach",
            ProjectType == 8 ~ "Safe Haven",
            ProjectType == 12 ~ "Homelessness Prevention",
            ProjectType == 13 ~ "Rapid Rehousing"
          )
        )  %>%
        filter(SummaryMeasure == "Length of Stay" &
                 ProjectType %in% c(input$radioLoSPTC)) %>%
        unique()
      
      LoSDetail <- qpr_leavers %>%
        filter(((
          !is.na(MoveInDateAdjust) &
            ProjectType == 13
        ) |
          (
            ProjectType %in% c(1, 2, 8) &
              !is.na(ExitDate)
          )) &
          exited_between(., ReportStart, ReportEnd)) %>%
        mutate(
          ProjectType = case_when(
            ProjectType == 1 ~ "Emergency Shelters",
            ProjectType == 2 ~ "Transitional Housing",
            ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
            ProjectType == 4 ~ "Street Outreach",
            ProjectType == 8 ~ "Safe Haven",
            ProjectType == 12 ~ "Homelessness Prevention",
            ProjectType == 13 ~ "Rapid Rehousing"
          )
        ) %>%
        filter(
          ProjectRegion %in% c(input$LoSRegionSelect) &
            ProjectType %in% c(input$radioLoSPTC)
        ) # this filter needs
      # to be here so the selection text matches the mutated data
      TotalLeavers <- LoSDetail %>%
        group_by(FriendlyProjectName) %>%
        summarise(Leavers = n())
      
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
        group_by(FriendlyProjectName,
                 ProjectRegion,
                 ProjectCounty,
                 ProjectType) %>%
        summarise(
          Days = case_when(
            input$radioAvgMeanLoS == "Average Days" ~
              as.numeric(mean(DaysinProject)),
            input$radioAvgMeanLoS == "Median Days" ~
              as.numeric(median(DaysinProject))
          )
        ) %>%
        left_join(LoSGoals, by = "ProjectType") %>%
        left_join(TotalLeavers, by = ("FriendlyProjectName")) %>%
        mutate(
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
        plot_ly(
          data = LoSSummary,
          x = ~ FriendlyProjectName,
          y = ~ Days,
          text = ~ hover,
          hoverinfo = 'text'
        ) %>%
          add_trace(type = "bar") %>%
          layout(
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
  
  output$headerQPRExitsToPH <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$ExitsToPHSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$ExitsToPHSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Quarterly Performance Report"),
      h3("Exits to Permanent Housing"),
      h4(ReportStart, "-", ReportEnd)
    )
  })  
  
  # observeEvent(
  #   c(input$ExitsToPHRegionSelect, input$ExitsToPHSlider),
  #   {
      output$ExitsToPH <- renderPlotly({
        ReportStart <- format.Date(ymd(paste0(
          substr(input$ExitsToPHSlider, 1, 4),
          "-01-01"
        )), "%m-%d-%Y")
        ReportEnd <- format.Date(mdy(paste0(
          case_when(
            substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
            substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
            substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
            substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
          ),
          substr(input$ExitsToPHSlider, 1, 4)
        )), "%m-%d-%Y")
        # title changes if you pick PSH since it's looking at Stayers as well
        yAxisTitle <- if_else(
          input$radioExitsToPHPTC !=
            "Permanent Supportive Housing",
          "Exited to Permanent Housing",
          "Remained in or Exited to PH"
        )
        # hhs that achieved the goal
        SuccessfullyPlaced <- qpr_leavers %>%
          filter(((
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
          group_by(FriendlyProjectName, 
                   ProjectType, 
                   ProjectCounty, 
                   ProjectRegion) %>%
          summarise(SuccessfullyPlacedHHs = n())
        
        # calculating the total households to compare successful placements to
        TotalHHsSuccessfulPlacement <- qpr_leavers %>%
          filter((
            served_between(., ReportStart, ReportEnd) &
              ProjectType %in% c(3, 9, 12) # PSH & HP
          ) |
            (
              exited_between(., ReportStart, ReportEnd) &
                ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
            )) %>%
          group_by(FriendlyProjectName, 
                   ProjectType, 
                   ProjectCounty, 
                   ProjectRegion) %>%
          summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
        # otherwise, it's total hhs *exited* during the reporting period
        
        SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
          left_join(
            SuccessfullyPlaced,
            by = c("FriendlyProjectName", 
                   "ProjectType", 
                   "ProjectCounty", 
                   "ProjectRegion")
          ) %>%
          mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)
        
        SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0
        
        PlacementGoal <-
          goals %>%
          filter(
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
        ptc <- PTC %>% filter(y == input$radioExitsToPHPTC) %>% select(x)
        ptc <- as_vector(ptc)
        
        stagingExitsToPH <- SuccessfulPlacement %>%
          left_join(PlacementGoal, by = "ProjectType") %>%
          filter(ProjectType %in% ptc, ProjectRegion %in% region) %>%
          mutate(
            hover = paste0(
              FriendlyProjectName, 
              "\nExited to PH: ", SuccessfullyPlacedHHs, 
              "\nTotal Households: ", TotalHHs, 
              "\n", as.integer(Percent * 100), "%",
              sep = "\n"
            )
          )
        
        if(nrow(stagingExitsToPH) > 0) {
        plot_ly(
          stagingExitsToPH,
          x = ~ FriendlyProjectName,
          y = ~ Percent,
          text = ~ hover,
          hoverinfo = 'text',
          type = "bar"
        ) %>%
          layout(
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
      
      output$ExitsToPHOutreach <- renderPlotly({
        if (input$radioExitsToPHPTC == "Street Outreach") {
          ReportStart <- format.Date(ymd(paste0(
            substr(input$ExitsToPHSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
              substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$ExitsToPHSlider, 1, 4)
          )), "%m-%d-%Y")
          
          totalServed <- qpr_leavers %>%
            filter(exited_between(., ReportStart, ReportEnd) &
                     ProjectType == 4) %>%
            group_by(FriendlyProjectName,
                     ProjectType,
                     ProjectCounty,
                     ProjectRegion) %>%
            summarise(TotalHHs = n())
          
          notUnsheltered <- qpr_leavers %>%
            filter(
              ProjectType == 4 &
                Destination != 16 &
                DestinationGroup %in% c("Temporary", "Permanent") &
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName,
                     ProjectType,
                     ProjectCounty,
                     ProjectRegion) %>%
            summarise(NotUnsheltered = n())
          
          goalOutreach <- goals %>%
            filter(Measure == "Exits to Temporary or Permanent Housing") %>%
            select(Goal, ProjectType)
          
          notUnsheltered <- notUnsheltered %>%
            left_join(goalOutreach, by = "ProjectType") %>%
            left_join(
              totalServed,
              by = c(
                "FriendlyProjectName",
                "ProjectType",
                "ProjectCounty",
                "ProjectRegion"
              )
            ) %>%
            mutate(
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
            filter(ProjectRegion %in% c(input$ExitsToPHRegionSelect))
          
          title <-
            paste0(
              "Exits to Temporary or Permanent Housing\n",
              "Street Outreach\n",
              ReportStart,
              " to ",
              ReportEnd
            )
          if (nrow(notUnsheltered) > 0) {
            plot_ly(
              notUnsheltered,
              x = ~ FriendlyProjectName,
              y = ~ Percent,
              text = ~ hover,
              hoverinfo = 'text',
              type = "bar"
            ) %>%
              layout(
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
  
  output$headerQPRNCBs <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$QPRNCBDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$QPRNCBDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRNCBDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRNCBDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3("Access to Mainstream Benefits: Non-cash Benefits"),
         h4(ReportStart, "-", ReportEnd))
  })  
  
  output$QPRNCBs <- renderPlotly({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$QPRNCBDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$QPRNCBDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRNCBDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRNCBDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRNCBDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    meeting_objective <- qpr_benefits %>%
      filter(
        ProjectRegion %in% input$QPRNCBRegionSelect &
          ProjectType == input$radioQPR_NCB_PTC &
          exited_between(., ReportStart, ReportEnd) &
          BenefitsFromAnySource == 1
      ) %>% 
      group_by(FriendlyProjectName, 
               ProjectType, 
               ProjectCounty, 
               ProjectRegion) %>%
      summarise(BenefitsAtExit = n())
    
    # calculating the total households for comparison
    all_hhs <- qpr_benefits %>%
      filter(ProjectRegion %in% c(input$QPRNCBRegionSelect) &
               ProjectType == input$radioQPR_NCB_PTC &
               exited_between(., ReportStart, ReportEnd)) %>%
      group_by(FriendlyProjectName, 
               ProjectType, 
               ProjectCounty, 
               ProjectRegion) %>%
      summarise(TotalHHs = n()) 
    
    NCBsAtExit <- all_hhs %>%
      left_join(
        meeting_objective,
        by = c("FriendlyProjectName", 
               "ProjectType", 
               "ProjectCounty", 
               "ProjectRegion")
      )
    
    NCBsAtExit[is.na(NCBsAtExit)] <- 0
    
    NCBsAtExit <- NCBsAtExit %>%
      mutate(Percent = BenefitsAtExit / TotalHHs)
    
    NCBGoal <-
      goals %>%
      filter(Measure == "Non-cash Benefits") %>%
      mutate(ProjectType = case_when(
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
      left_join(NCBGoal, by = "ProjectType") %>%
      filter(ProjectType == input$radioQPR_NCB_PTC & 
               ProjectRegion %in% region) %>%
      mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nReceiving Non-Cash Benefits at Exit: ", BenefitsAtExit, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingNCBs) > 0) {
      plot_ly(
        stagingNCBs,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        layout(
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
  
  output$headerQPRHI <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$QPRHIDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$QPRHIDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRHIDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRHIDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3("Access to Mainstream Benefits: Health Insurance"),
         h4(ReportStart, "-", ReportEnd))
  })  
  
  output$QPRHIs <- renderPlotly({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$QPRHIDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$QPRHIDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRHIDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRHIDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRHIDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    meeting_objective <- qpr_benefits %>%
      filter(
        ProjectRegion %in% input$QPRHIRegionSelect &
          ProjectType == input$radioQPR_HI_PTC &
          exited_between(., ReportStart, ReportEnd) &
          InsuranceFromAnySource == 1
      ) %>% 
      group_by(FriendlyProjectName,
               ProjectType,
               ProjectCounty,
               ProjectRegion) %>%
      summarise(InsuranceAtExit = n())
    
    # calculating the total households for comparison
    all_hhs <- qpr_benefits %>%
      filter(ProjectRegion %in% input$QPRHIRegionSelect &
               ProjectType == input$radioQPR_HI_PTC &
               exited_between(., ReportStart, ReportEnd)) %>%
      group_by(FriendlyProjectName,
               ProjectType,
               ProjectCounty,
               ProjectRegion) %>%
      summarise(TotalHHs = n()) 
    
    HIAtExit <- all_hhs %>%
      left_join(
        meeting_objective,
        by = c("FriendlyProjectName",
               "ProjectType",
               "ProjectCounty",
               "ProjectRegion")
      )
    
    HIAtExit[is.na(HIAtExit)] <- 0
    
    HIAtExit <- HIAtExit %>%
      mutate(Percent = InsuranceAtExit / TotalHHs)
    
    HIGoal <-
      goals %>%
      filter(Measure == "Health Insurance at Exit") %>%
      mutate(ProjectType = case_when(
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
      left_join(HIGoal, by = "ProjectType") %>%
      filter(ProjectType == input$radioQPR_HI_PTC & 
               ProjectRegion %in% region) %>%
      mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nHealth Insurance at Exit: ", InsuranceAtExit, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingHI) > 0) {
      plot_ly(
        stagingHI,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        layout(
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
  
  output$headerQPRIncome <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$QPRIncomeDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$QPRIncomeDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRIncomeDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3("Access to Mainstream Benefits: Increased Income"),
         h4(ReportStart, "-", ReportEnd))
  })  
  
  output$QPRIncome <- renderPlotly({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$QPRIncomeDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$QPRIncomeDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$QPRIncomeDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$QPRIncomeDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    meeting_objective <- qpr_income %>%
      filter(
        ProjectRegion %in% input$QPRIncomeRegionSelect &
          ProjectType == input$radioQPR_Income_PTC &
          stayed_between(., ReportStart, ReportEnd) &
          Difference > 0
      ) %>% 
      group_by(FriendlyProjectName,
               ProjectType,
               ProjectCounty,
               ProjectRegion) %>%
      summarise(Increased = n())
    
    # calculating the total households for comparison
    all_hhs <- qpr_income %>%
      filter(ProjectRegion %in% input$QPRIncomeRegionSelect &
               ProjectType == input$radioQPR_Income_PTC &
               stayed_between(., ReportStart, ReportEnd)) %>%
      group_by(FriendlyProjectName,
               ProjectType,
               ProjectCounty,
               ProjectRegion) %>%
      summarise(TotalHHs = n()) 
    
    IncreasedIncome <- all_hhs %>%
      left_join(
        meeting_objective,
        by = c("FriendlyProjectName",
               "ProjectType",
               "ProjectCounty",
               "ProjectRegion")
      )
    
    IncreasedIncome[is.na(IncreasedIncome)] <- 0
    
    IncreasedIncome <- IncreasedIncome %>%
      mutate(Percent = Increased / TotalHHs)
    
    IncomeGoal <-
      goals %>%
      filter(Measure == "Gain or Increase Income") %>%
      mutate(ProjectType = case_when(
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
      left_join(IncomeGoal, by = "ProjectType") %>%
      filter(ProjectType == input$radioQPR_Income_PTC &
               ProjectRegion %in% region) %>%
      mutate(
        hover = paste0(
          FriendlyProjectName, 
          "\nIncreased Income: ", Increased, 
          "\nTotal Households: ", TotalHHs, 
          "\n", as.integer(Percent * 100), "%",
          sep = "\n"
        )
      )
    
    if(nrow(stagingIncome) > 0) {
      plot_ly(
        stagingIncome,
        x = ~ FriendlyProjectName,
        y = ~ Percent,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        layout(
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
  
  output$headerRRHRapidPlacement <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$RapidRRHDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RapidRRHDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3("Rapid Placement into Rapid Rehousing"),
         h4(ReportStart, "-", ReportEnd))
  })  
  
  # QPR Rapid Placement into RRH
  output$DaysToHouse <- 
    renderPlotly({
      
      ReportStart <- format.Date(ymd(paste0(
        substr(input$RapidRRHDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RapidRRHDateSlider, 1, 4)
      )), "%m-%d-%Y")
      
      daysToHouse <- qpr_rrh_enterers %>%
        filter(
            !is.na(MoveInDateAdjust) &
            ProjectRegion %in% c(input$RapidRRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        )
      
      RRHgoal <- goals %>%
        filter(SummaryMeasure == "Rapid Placement") %>%
        select(ProjectType, Goal)
      
      summaryDays <- daysToHouse %>%
        group_by(FriendlyProjectName,
                 ProjectCounty,
                 ProjectRegion,
                 ProjectType) %>%
        summarise(AvgDays = as.integer(mean(DaysToHouse)),
                  TotalHHs = n()) %>%
        left_join(RRHgoal, by = "ProjectType") %>%
        mutate(hover = paste0(
          FriendlyProjectName,
          "\nAverage Days to House: ",
          AvgDays,
          "\nTotal Households: ",
          TotalHHs,
          sep = "\n"
        ))
      
      title <- paste0("Average Days to House\nRapid Rehousing\n",
                      ReportStart, " to ", ReportEnd)
      
      plot_ly(
        summaryDays,
        x = ~ FriendlyProjectName,
        y = ~ AvgDays,
        text = ~ hover,
        hoverinfo = 'text',
        type = "bar"
      ) %>%
        layout(
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
  
  output$headerRRHSpending <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$RRHSpendingDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")

    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RRHSpendingDateSlider, 1, 4)
    )), "%m-%d-%Y")

    list(h2("Quarterly Performance Report"),
         h3("Rapid Rehousing Spending Goals"),
         h4(ReportStart, "-", ReportEnd))
  })

#  QPR HP vs RRH Spending
  output$RRHSpending <-
    renderPlotly({
      ReportStart <- format.Date(ymd(paste0(
        substr(input$RRHSpendingDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")

      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RRHSpendingDateSlider, 1, 4)
      )), "%m-%d-%Y")

      rrhSpending <- qpr_spending %>%
        filter(
          !is.na(OrganizationName) &
            ProjectRegion %in% c(input$RRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(ProjectType = if_else(ProjectType == 12,
                                     "HP",
                                     "RRH"),
               ProjectType = factor(ProjectType, levels = c("HP", "RRH")))
      
      x <- qpr_spending %>%
        filter(
          !is.na(OrganizationName) &
            ProjectRegion %in% c(input$RRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        ) %>%
        select(OrganizationName, ProjectRegion) %>%
        unique() %>%
        arrange(OrganizationName)
      
      x <- rbind(x, x) %>% arrange(OrganizationName)
      
      y <- data.frame(ProjectType = c("HP", "RRH"))
      
      z <- cbind(x, y)
      
      rrhSpending <- rrhSpending %>% right_join(z, by = c("OrganizationName",
                                                          "ProjectRegion",
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
        group_by(OrganizationName, ProjectRegion, ProjectType) %>%
        summarise(Amount = sum(Amount),
                  HHs = n()) %>%
        ungroup() %>%
        spread(ProjectType, Amount)
      
      rrhSpending[is.na(rrhSpending)] <- 0
      
      rrhSpending <- rrhSpending %>%
        group_by(OrganizationName, ProjectRegion) %>%
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
          )
        )
  })
  
}