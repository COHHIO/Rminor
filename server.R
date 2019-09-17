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

function(input, output, session) {
  # output$res <- renderPrint({
  #  input$utilizationDate
  # cat("Length of Stay", input$LoSRegionSelect)
  # })
  output$headerUtilization <- renderUI({
    ReportEnd <- ceiling_date(ymd(input$utilizationDate), unit = "month") - days(1)
    ReportStart <- floor_date(ymd(ReportEnd), unit = "month") -
      years(1) +
      months(1)
    
    ReportStart <- format.Date(ymd(ReportStart), "%B %d, %Y")
    ReportEnd <- format.Date(ymd(ReportEnd), "%B %d, %Y")
    
    list(
      h2("Bed and Unit Utilization"),
      h4(input$providerListUtilization),
      h4(ReportStart, "-", ReportEnd)
    )
  })
  
  observeEvent(c(input$providerList), {
    output$currentUnitUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            title = "Current Unit Utilization",
            subtitle = paste(
              Utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(Households),
              "Households in",
              Utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(UnitCount),
              "Units"
            ),
            color = "aqua",
            icon = icon("building"),
            value = Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitUtilization)
          )
        })
      }
    else{
      
    }
    
    output$currentBedUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1:3, 8, 9)
               )) > 0) {
        renderInfoBox({
          infoBox(
            title = "Current Bed Utilization",
            subtitle = paste(
              Utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(Clients),
              "Clients in",
              Utilization %>%
                filter(ProjectName == input$providerList) %>%
                select(BedCount),
              "Beds"
            ),
            color = "purple",
            icon = icon("bed"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedUtilization)
          )
        })
      }
    else{
      
    }
    
    output$veteranEngagement <-
      if (nrow(
        CurrentVeteranCounts %>%
        filter(ProjectName == input$providerList) %>%
        select(Veterans)
      ) > 0) {
        renderInfoBox({
          infoBox(
            title = "Current Veterans",
            subtitle = VetEngagementSummary %>%
              filter(ProjectName == input$providerList) %>% pull(Summary),
            color = "green",
            icon = icon("ribbon"),
            CurrentVeteranCounts %>%
              filter(ProjectName == input$providerList) %>%
              select(Veterans)
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
            title = paste("Client Exits to Rapid Rehousing in", year(mdy(FileEnd))),
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
  
  output$bedPlot <-
    renderPlotly({
      ReportEnd <- ymd(input$utilizationDate) 
      ReportStart <- floor_date(ymd(ReportEnd), unit = "month") -
        years(1) +
        months(1)
      ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))
      
      Provider <- input$providerListUtilization
      
      bedPlot <- BedUtilization %>% select(-FilePeriod) %>%
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
      
      unitPlot <- UnitUtilization %>% select(-FilePeriod) %>%
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
    renderText(unit_utilization_note)
  
  output$bedNote <-
    renderText(bed_utilization_note)
  
  output$utilizationNote <-
    renderText(calculation_note)
  
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
      # counting all households who were scored AND SERVED between the report dates
      CountyAverageScores <- CountyData %>%
        filter(served_between(CountyData,
                              ReportStart,
                              ReportEnd)) %>%
        select(CountyServed, PersonalID, Score) %>%
        distinct() %>%
        group_by(CountyServed) %>%
        summarise(AverageScore = round(mean(Score), 1),
                  HHsLHinCounty = n())
      # counting all households who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- SPDATsByProject %>%
        filter(entered_between(SPDATsByProject,
                               ReportStart,
                               ReportEnd)) %>%
        group_by(CountyServed) %>%
        summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
                  HHsHousedInCounty = n())
      # pulling in both averages for each county plus adding Region for grouping
      Compare <-
        full_join(CountyAverageScores,
                  CountyHousedAverageScores,
                  by = "CountyServed") %>%
        arrange(CountyServed) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
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
    renderText(hhsServedInCounty)
  
  output$HHsServedScoresText <-
    renderText(hhsHousedInCounty)
  
  output$NoteToUsers <-
    renderText(noteToUsers)
  # QPR Length of Stay
  observeEvent(c(input$LoSRegionSelect, input$LoSSlider, input$radioLoSPTC),
               {
                 output$QPRLoSPlot <-
                   renderPlotly({
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
                     
                     LoSDetail <- QPR_EEs %>%
                       filter(((!is.na(MoveInDateAdjust) &
                                  ProjectType %in% c(13)) |
                                 (ProjectType %in% c(1, 2, 8) &
                                    !is.na(ExitDate))
                       ) &
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
                       filter(Region %in% c(input$LoSRegionSelect) &
                                ProjectType %in% c(input$radioLoSPTC)) # this filter needs
                     # to be here so the selection text matches the mutated data
                     TotalLeavers <- LoSDetail %>%
                       group_by(FriendlyProjectName) %>%
                       summarise(Leavers = n())
                     
                     title <- paste0("Length of Stay (", input$radioAvgMeanLoS, ")\n", 
                                    input$radioLoSPTC, "\n",
                                    ReportStart, " to ", ReportEnd)
                     
                     LoSSummary <- LoSDetail %>%
                       group_by(FriendlyProjectName,
                                Region,
                                County,
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
                       mutate(hover = paste0(
                         FriendlyProjectName, 
                         "\nTotal Leavers: ", Leavers, 
                         "\nDays: ", Days,
                         sep = "\n"))
                     
                     
                     if (nrow(LoSDetail) > 0) {
                       plot_ly(data = LoSSummary,
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
               })
 
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
    
    x <- renderText(input$ExitsToPHRegionSelect)
    # y <- if_else(sum(sapply(x, length)) == 2,
    #              renderText(input$ExitsToPHRegionSelect),
    #              "Multiple Regions")
    
    list(
      h2("Quarterly Performance Report"),
      h3("Exits to Permanent Housing"),
      h4(x),
      h4(ReportStart, "-", ReportEnd)
    )
  })  
  
  observeEvent(
    c(input$ExitsToPHRegionSelect, input$ExitsToPHSlider),
    {
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
        
        yAxisTitle <- if_else(input$radioExitsToPHPTC != "Permanent Supportive Housing",
                              "Exited to Permanent Housing",
                              "Remained in or Exited to PH")
        
        SuccessfullyPlaced <- QPR_EEs %>%
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
          group_by(FriendlyProjectName, ProjectType, County, Region) %>%
          summarise(SuccessfullyPlacedHHs = n())
        
        # calculating the total households to compare successful placements to
        TotalHHsSuccessfulPlacement <- QPR_EEs %>%
          filter((
            served_between(., ReportStart, ReportEnd) &
              ProjectType %in% c(3, 9, 12) # PSH & HP
          ) |
            (
              exited_between(., ReportStart, ReportEnd) &
                ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
            )) %>%
          group_by(FriendlyProjectName, ProjectType, County, Region) %>%
          summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
        # otherwise, it's total hhs *exited* during the reporting period
        
        SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
          left_join(
            SuccessfullyPlaced,
            by = c("FriendlyProjectName", "ProjectType", "County", "Region")
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
          filter(ProjectType %in% ptc, Region %in% region) %>%
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
          if(input$radioExitsToPHPTC == "Street Outreach") {
            
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
            
            totalServed <- QPR_EEs %>%
              filter(exited_between(., ReportStart, ReportEnd) &
                    ProjectType == 4) %>%
              group_by(FriendlyProjectName, ProjectType, County, Region) %>%
              summarise(TotalHHs = n())
            
            notUnsheltered <- QPR_EEs %>%
              filter(
                ProjectType == 4 &
                  Destination != 16 &
                  DestinationGroup %in% c("Temporary", "Permanent") &
                  exited_between(., ReportStart, ReportEnd)
              ) %>% 
              group_by(FriendlyProjectName, ProjectType, County, Region) %>%
              summarise(NotUnsheltered = n())
            
            goalOutreach <- goals %>%
              filter(Measure == "Exits to Temporary or Permanent Housing") %>%
              select(Goal, ProjectType)
            
            notUnsheltered <- notUnsheltered %>%
              left_join(goalOutreach, by = "ProjectType") %>%
              left_join(totalServed,
                        by = c("FriendlyProjectName",
                               "ProjectType",
                               "County",
                               "Region")) %>%
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
              filter(Region %in% c(input$ExitsToPHRegionSelect))
            
            title <- paste0("Exits to Temporary or Permanent Housing\n", 
                            "Street Outreach\n",
                            ReportStart, " to ", ReportEnd)
            
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
              )
          }
          else{
            NULL
          }
        })
        
  })
  
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
    
    meeting_objective <- QPR_MainstreamBenefits %>%
      filter(
        Region %in% parse_number(input$QPRNCBRegionSelect) &
          ProjectType == input$radioQPR_NCB_PTC &
          exited_between(., ReportStart, ReportEnd) &
          BenefitsFromAnySource == 1
      ) %>% 
      group_by(FriendlyProjectName, ProjectType, County, Region) %>%
      summarise(BenefitsAtExit = n())
    
    # calculating the total households for comparison
    all_hhs <- QPR_MainstreamBenefits %>%
      filter(Region %in% parse_number(input$QPRNCBRegionSelect) &
               ProjectType == input$radioQPR_NCB_PTC &
               exited_between(., ReportStart, ReportEnd)) %>%
      group_by(FriendlyProjectName, ProjectType, County, Region) %>%
      summarise(TotalHHs = n()) 
    
    NCBsAtExit <- all_hhs %>%
      left_join(
        meeting_objective,
        by = c("FriendlyProjectName", "ProjectType", "County", "Region")
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
    
    region <- parse_number(input$QPRNCBRegionSelect)
    
    stagingNCBs <- NCBsAtExit %>%
      left_join(NCBGoal, by = "ProjectType") %>%
      filter(ProjectType == input$radioQPR_NCB_PTC, Region %in% region) %>%
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
    
    meeting_objective <- QPR_MainstreamBenefits %>%
      filter(
        Region %in% parse_number(input$QPRHIRegionSelect) &
          ProjectType == input$radioQPR_HI_PTC &
          exited_between(., ReportStart, ReportEnd) &
          InsuranceFromAnySource == 1
      ) %>% 
      group_by(FriendlyProjectName, ProjectType, County, Region) %>%
      summarise(InsuranceAtExit = n())
    
    # calculating the total households for comparison
    all_hhs <- QPR_MainstreamBenefits %>%
      filter(Region %in% parse_number(input$QPRHIRegionSelect) &
               ProjectType == input$radioQPR_HI_PTC &
               exited_between(., ReportStart, ReportEnd)) %>%
      group_by(FriendlyProjectName, ProjectType, County, Region) %>%
      summarise(TotalHHs = n()) 
    
    HIAtExit <- all_hhs %>%
      left_join(
        meeting_objective,
        by = c("FriendlyProjectName", "ProjectType", "County", "Region")
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
    
    region <- parse_number(input$QPRHIRegionSelect)
    
    stagingHI <- HIAtExit %>%
      left_join(HIGoal, by = "ProjectType") %>%
      filter(ProjectType == input$radioQPR_HI_PTC, Region %in% region) %>%
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
    
    meeting_objective <- QPR_Income %>%
      filter(
        Region %in% parse_number(input$QPRIncomeRegionSelect) &
          ProjectType == input$radioQPR_Income_PTC &
          exited_between(., ReportStart, ReportEnd) &
          Difference > 0
      ) %>% 
      group_by(FriendlyProjectName, ProjectType, County, Region) %>%
      summarise(Increased = n())
    
    # calculating the total households for comparison
    all_hhs <- QPR_Income %>%
      filter(Region %in% parse_number(input$QPRIncomeRegionSelect) &
               ProjectType == input$radioQPR_Income_PTC &
               exited_between(., ReportStart, ReportEnd)) %>%
      group_by(FriendlyProjectName, ProjectType, County, Region) %>%
      summarise(TotalHHs = n()) 
    
    IncreasedIncome <- all_hhs %>%
      left_join(
        meeting_objective,
        by = c("FriendlyProjectName", "ProjectType", "County", "Region")
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
    
    region <- parse_number(input$QPRIncomeRegionSelect)
    
    stagingIncome <- IncreasedIncome %>%
      left_join(IncomeGoal, by = "ProjectType") %>%
      filter(ProjectType == input$radioQPR_Income_PTC, Region %in% region) %>%
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
      
      daysToHouse <- RRHEnterers %>%
        filter(
            !is.na(MoveInDateAdjust) &
            Region %in% c(input$RapidRRHRegion) &
            entered_between(., ReportStart, ReportEnd)
        )
      
      RRHgoal <- goals %>%
        filter(SummaryMeasure == "Rapid Placement") %>%
        select(ProjectType, Goal)
      
      summaryDays <- daysToHouse %>%
        group_by(FriendlyProjectName, County, Region, ProjectType) %>%
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
  
  
}