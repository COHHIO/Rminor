



function(input, output, session) {
  output$res <- renderPrint({
    input$LoSRegionSelect
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
            Utilization %>%
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
                 ProjectType %in% c(1, 2, 3, 8, 9)
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
    
  })
  
  output$SPDATScoresByCounty <-
    renderPlot({
      # ReportStart <- format.Date(mdy(paste0("01-01-", input$y)), "%m-%d-%Y")
      ReportStart <- format.Date(ymd(paste0(
        substr(input$spdatSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$spdatSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$spdatSlider, 7, 7) == 2 ~ "06-30",
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
  
  output$bedPlot <-
    renderPlot({
      ReportEnd <- ymd(paste0(
        substr(
          input$utilizationSlider,
          str_length(input$utilizationSlider) - 4,
          str_length(input$utilizationSlider)
        ),
        substr(
          input$utilizationSlider,
          1,
          str_length(input$utilizationSlider) - 5
        ),
        "01"
      )) +
        months(1) - 1
      ReportStart <- floor_date(ymd(ReportEnd), unit = "month") -
        years(1) +
        months(1)
      ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))
      
      bedPlot <- BedUtilization %>% select(-FilePeriod) %>%
        gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) %>%
        filter(
          ProjectName == input$providerListUtilization,
          mdy(Month) %within% ReportingPeriod
        ) %>%
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
        filter(
          ProjectName == input$providerListUtilization,
          mdy(Month) %within% ReportingPeriod
        ) %>%
        mutate(
          Month = floor_date(mdy(Month), unit = "month"),
          Unit = Utilization,
          Utilization = NULL
        )
      
      utilizationPlot <- unitPlot %>%
        full_join(bedPlot,
                  by = c("ProjectID", "ProjectName", "ProjectType", "Month")) %>%
        gather(
          "UtilizationType",
          "Utilization",
          -ProjectID,-ProjectName,-ProjectType,-Month
        ) %>%
        arrange(Month)
      
      ggplot(utilizationPlot,
             aes(x = Month,
                 y = Utilization,
                 color = UtilizationType)) +
        theme_light() +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_y_continuous(limits = c(0, 2),
                           labels = scales::percent_format(accuracy = 1)) +
        scale_x_date(
          date_labels = "%B %Y",
          date_breaks = "3 months",
          minor_breaks = "1 month"
        ) +
        scale_colour_manual(values = c("#56B4E9", "#6be956")) +
        labs(
          title = input$providerListUtilization,
          subtitle = paste(
            "Date Range:",
            format.Date(ymd(ReportStart), "%b %Y"),
            "to",
            format.Date(ymd(ReportEnd), "%b %Y")
          ),
          caption = "Client and household enrollment data comes from the Ohio
          Balance of State CoC HMIS. This visualization was created by the
          COHHIO HMIS team."
        )
      
    })
  
  output$CountyScoresText <-
    renderText(hhsServedInCounty)
  
  output$HHsServedScoresText <-
    renderText(hhsHousedInCounty)
  
  output$NoteToUsers <-
    renderText(noteToUsers)
  
  observeEvent(c(input$LoSRegionSelect), {
    output$QPRLoSPlotEE <- #should this be "ES" instead of "EE"???
      if (nrow(QPR_EEs %>% filter(Region %in% c(
        input$LoSRegionSelect
      ) &
      ProjectType == 1)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$LoSSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$LoSSlider, 7, 7) == 2 ~ "06-30",
              substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$LoSSlider, 1, 4)
          )), "%m-%d-%Y")
          
          LoSGoals <- goals %>%
            select(-Measure) %>%
            filter(SummaryMeasure == "Length of Stay" &
                     !is.na(Goal)) %>%
            unique()
          
          LoSDetail <- QPR_EEs %>%
            filter((((!is.na(MoveInDateAdjust) &
                        ProjectType %in% c(13)) |
                       (ProjectType %in% c(1, 2, 8)) &
                       !is.na(ExitDate)
            )) &
              exited_between(., ReportStart, ReportEnd)) %>%
            mutate(
              ProjectType = case_when(
                ProjectType == 1 ~ "Emergency Shelter",
                ProjectType == 2 ~ "Transitional Housing",
                ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
                ProjectType == 4 ~ "Street Outreach",
                ProjectType == 8 ~ "Safe Haven",
                ProjectType == 12 ~ "Homelessness Prevention",
                ProjectType == 13 ~ "Rapid Rehousing"
              )
            ) %>%
            filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
          # to be here so the selection text matches the mutated data
          
          LoSSummary <- LoSDetail %>%
            group_by(FriendlyProjectName,
                     Region,
                     County,
                     ProjectType) %>%
            summarise(
              Days = case_when(
                input$radioAvgMeanLoS == "Average Days" ~
                  as.integer(mean(DaysinProject, na.rm = TRUE)),
                input$radioAvgMeanLoS == "Median Days" ~
                  as.integer(median(DaysinProject, na.rm = TRUE))
              )
            )
          
          esdata <-
            LoSSummary %>% filter(ProjectType == "Emergency Shelter")
          esLoSGoal <- as.integer(LoSGoals %>%
                                    filter(ProjectType == 1) %>%
                                    select(Goal))
          
          plot_ly(
            data = esdata,
            x = ~ FriendlyProjectName,
            y = ~ Days
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = esLoSGoal,
                y1 = esLoSGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Emergency Shelters',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Length of Stay (Days)", 
                           rangemode = "tozero",
                           showgrid = TRUE),
              xaxis = list(title = "Providers", showgrid = TRUE,
                           rangemode = "tozero")
            )
          
        })
      }
    else{
      
    }
    
    output$QPRLoSPlotTH <-
      if (nrow(QPR_EEs %>% filter(Region %in% c(
        input$LoSRegionSelect
      ) &
      ProjectType == 2)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$LoSSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$LoSSlider, 7, 7) == 2 ~ "06-30",
              substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$LoSSlider, 1, 4)
          )), "%m-%d-%Y")

          LoSGoals <- goals %>%
            select(-Measure) %>%
            filter(SummaryMeasure == "Length of Stay" &
                     !is.na(Goal)) %>%
            unique()

          LoSDetail <- QPR_EEs %>%
            filter((((!is.na(MoveInDateAdjust) &
                        ProjectType %in% c(13)) |
                       (ProjectType %in% c(1, 2, 8)) &
                       !is.na(ExitDate)
            )) &
              exited_between(., ReportStart, ReportEnd)) %>%
            mutate(
              ProjectType = case_when(
                ProjectType == 1 ~ "Emergency Shelter",
                ProjectType == 2 ~ "Transitional Housing",
                ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
                ProjectType == 4 ~ "Street Outreach",
                ProjectType == 8 ~ "Safe Haven",
                ProjectType == 12 ~ "Homelessness Prevention",
                ProjectType == 13 ~ "Rapid Rehousing"
              )
            ) %>%
            filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
          # to be here so the selection text matches the mutated data

          LoSSummary <- LoSDetail %>%
            group_by(FriendlyProjectName,
                     Region,
                     County,
                     ProjectType) %>%
            summarise(
              Days = case_when(
                input$radioAvgMeanLoS == "Average Days" ~
                  as.integer(mean(DaysinProject, na.rm = TRUE)),
                input$radioAvgMeanLoS == "Median Days" ~
                  as.integer(median(DaysinProject, na.rm = TRUE))
              )
            )

          thdata <-
            LoSSummary %>% filter(ProjectType == "Transitional Housing")
          thLoSGoal <- as.integer(LoSGoals %>%
                                    filter(ProjectType == 2) %>%
                                    select(Goal))

          plot_ly(
            data = thdata,
            x = ~ FriendlyProjectName,
            y = ~ Days
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = thLoSGoal,
                y1 = thLoSGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Transitional Housing',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Length of Stay (Days)",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "Providers",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )

        })
      }
    else{

    }

    output$QPRLoSPlotSH <-
      if (nrow(QPR_EEs %>% filter(Region %in% c(
        input$LoSRegionSelect
      ) &
      ProjectType == 8)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$LoSSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$LoSSlider, 7, 7) == 2 ~ "06-30",
              substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$LoSSlider, 1, 4)
          )), "%m-%d-%Y")

          LoSGoals <- goals %>%
            select(-Measure) %>%
            filter(SummaryMeasure == "Length of Stay" &
                     !is.na(Goal)) %>%
            unique()

          LoSDetail <- QPR_EEs %>%
            filter((((!is.na(MoveInDateAdjust) &
                        ProjectType %in% c(13)) |
                       (ProjectType %in% c(1, 2, 8)) &
                       !is.na(ExitDate)
            )) &
              exited_between(., ReportStart, ReportEnd)) %>%
            mutate(
              ProjectType = case_when(
                ProjectType == 1 ~ "Emergency Shelter",
                ProjectType == 2 ~ "Transitional Housing",
                ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
                ProjectType == 4 ~ "Street Outreach",
                ProjectType == 8 ~ "Safe Haven",
                ProjectType == 12 ~ "Homelessness Prevention",
                ProjectType == 13 ~ "Rapid Rehousing"
              )
            ) %>%
            filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
          # to be here so the selection text matches the mutated data

          LoSSummary <- LoSDetail %>%
            group_by(FriendlyProjectName,
                     Region,
                     County,
                     ProjectType) %>%
            summarise(
              Days = case_when(
                input$radioAvgMeanLoS == "Average Days" ~
                  as.integer(mean(DaysinProject, na.rm = TRUE)),
                input$radioAvgMeanLoS == "Median Days" ~
                  as.integer(median(DaysinProject, na.rm = TRUE))
              )
            )

          shdata <- LoSSummary %>% filter(ProjectType == "Safe Haven")
          shLoSGoal <- as.integer(LoSGoals %>%
                                    filter(ProjectType == 8) %>%
                                    select(Goal))

          plot_ly(
            data = shdata,
            x = ~ FriendlyProjectName,
            y = ~ Days
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = shLoSGoal,
                y1 = shLoSGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Safe Haven',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Length of Stay (Days)",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "Providers",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )


        })
      }
    else{
    }

    output$QPRLoSPlotRRH <-
      if (nrow(QPR_EEs %>% filter(Region %in% c(input$LoSRegionSelect) &
                                  ProjectType == 13)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$LoSSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$LoSSlider, 7, 7) == 2 ~ "06-30",
              substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$LoSSlider, 1, 4)
          )), "%m-%d-%Y")

          LoSGoals <- goals %>%
            select(-Measure) %>%
            filter(SummaryMeasure == "Length of Stay" &
                     !is.na(Goal)) %>%
            unique()

          LoSDetail <- QPR_EEs %>%
            filter((((!is.na(MoveInDateAdjust) &
                        ProjectType %in% c(13)) |
                       (ProjectType %in% c(1, 2, 8)) &
                       !is.na(ExitDate)
            )) &
              exited_between(., ReportStart, ReportEnd)) %>%
            mutate(
              ProjectType = case_when(
                ProjectType == 1 ~ "Emergency Shelter",
                ProjectType == 2 ~ "Transitional Housing",
                ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
                ProjectType == 4 ~ "Street Outreach",
                ProjectType == 8 ~ "Safe Haven",
                ProjectType == 12 ~ "Homelessness Prevention",
                ProjectType == 13 ~ "Rapid Rehousing"
              )
            ) %>%
            filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
          # to be here so the selection text matches the mutated data

          LoSSummary <- LoSDetail %>%
            group_by(FriendlyProjectName,
                     Region,
                     County,
                     ProjectType) %>%
            summarise(
              Days = case_when(
                input$radioAvgMeanLoS == "Average Days" ~
                  as.integer(mean(DaysinProject, na.rm = TRUE)),
                input$radioAvgMeanLoS == "Median Days" ~
                  as.integer(median(DaysinProject, na.rm = TRUE))
              )
            )

          rrhdata <-
            LoSSummary %>% filter(ProjectType == "Rapid Rehousing")
          rrhLoSGoal <- as.integer(LoSGoals %>%
                                     filter(ProjectType == 13) %>%
                                     select(Goal))

          plot_ly(
            data = rrhdata,
            x = ~ FriendlyProjectName,
            y = ~ Days
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = rrhLoSGoal,
                y1 = rrhLoSGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Rapid Rehousing',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Length of Stay (Days)",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "Providers",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )


        })
      }
    else{

    }
  
  })
    
    observeEvent(c(input$SuccessPlaceRegionSelect), {
      output$QPRSuccessfulPlacementES <-
      if (nrow(QPR_EEs %>% filter(Region %in% c(input$SuccessPlaceRegionSelect) &
                                  ProjectType == 1)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 1

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                DestinationGroup == "Permanent" & # exited to ph
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(SuccessfullyPlacedHHs = n())

          # calculating the total households to compare successful placements to
          TotalHHsSuccessfulPlacement <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          esPlacementData <-  SuccessfulPlacement 

          esPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = esPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = esPlacementGoal,
                y1 = esPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Emergency Shelters',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }

    output$QPRSuccessfulPlacementTH <-
      if (nrow(QPR_EEs %>%
               filter(Region %in% c(input$SuccessPlaceRegionSelect) &
                      ProjectType == 2)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 2

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                ((
                  ProjectType %in% c(3, 9, 13) & !is.na(MoveInDateAdjust)
                ) |
                  ProjectType %in% c(1, 2, 4, 8, 12)) &
                # excluding non-mover-inners
                ((
                  DestinationGroup == "Permanent" &
                    (exited_between(., ReportStart, ReportEnd) |
                       #exited to ph or still in PSH/HP
                       is.na(ExitDate))
                ) &
                  ProjectType %in% c(3, 9, 12) # PSH & HP
                ) |
                (
                  DestinationGroup == "Permanent" & # exited to ph
                    exited_between(., ReportStart, ReportEnd) &
                    ProjectType %in% c(1, 2, 4, 8, 13)
                ) # ES, TH, SH, RRH, OUT
                ) %>%
                group_by(FriendlyProjectName, ProjectType, County, Region) %>%
                summarise(SuccessfullyPlacedHHs = n())
              
              # calculating the total households to compare successful placements to
              TotalHHsSuccessfulPlacement <- QPR_EEs %>%
                filter(
                  ProjectType == ptc &
                    Region %in% c(input$SuccessPlaceRegionSelect) &
                    (
                      served_between(., ReportStart, ReportEnd) &
                        ProjectType %in% c(3, 9, 12) # PSH & HP
                    ) |
                    (
                      exited_between(., ReportStart, ReportEnd) &
                        ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
                    )
                ) %>%
                group_by(FriendlyProjectName, ProjectType, County, Region) %>%
                summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          rm(TotalHHsSuccessfulPlacement, SuccessfullyPlaced)

          thPlacementData <-
            SuccessfulPlacement %>%
            filter(ProjectType == ptc &
                     Region %in% c(input$SuccessPlaceRegionSelect))

          thPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = thPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = thPlacementGoal,
                y1 = thPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Transitional Housing',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }

    output$QPRSuccessfulPlacementPSH <-
      if (nrow(QPR_EEs %>%
               filter(Region %in% c(input$SuccessPlaceRegionSelect) &
                      ProjectType == 3)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 3

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                ((
                  ProjectType %in% c(3, 9, 13) & !is.na(MoveInDateAdjust)
                ) |
                  ProjectType %in% c(1, 2, 4, 8, 12)) &
                # excluding non-mover-inners
                ((
                  DestinationGroup == "Permanent" &
                    (exited_between(., ReportStart, ReportEnd) |
                       #exited to ph or still in PSH/HP
                       is.na(ExitDate))
                ) &
                  ProjectType %in% c(3, 9, 12) # PSH & HP
                ) |
                (
                  DestinationGroup == "Permanent" & # exited to ph
                    exited_between(., ReportStart, ReportEnd) &
                    ProjectType %in% c(1, 2, 4, 8, 13)
                ) # ES, TH, SH, RRH, OUT
                ) %>%
                group_by(FriendlyProjectName, ProjectType, County, Region) %>%
                summarise(SuccessfullyPlacedHHs = n())
              
              # calculating the total households to compare successful placements to
              TotalHHsSuccessfulPlacement <- QPR_EEs %>%
                filter(
                  ProjectType == ptc &
                    Region %in% c(input$SuccessPlaceRegionSelect) &
                    (
                      served_between(., ReportStart, ReportEnd) &
                        ProjectType %in% c(3, 9, 12) # PSH & HP
                    ) |
                    (
                      exited_between(., ReportStart, ReportEnd) &
                        ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
                    )
                ) %>%
                group_by(FriendlyProjectName, ProjectType, County, Region) %>%
                summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          rm(TotalHHsSuccessfulPlacement, SuccessfullyPlaced)

          pshPlacementData <-
            SuccessfulPlacement %>%
            filter(ProjectType == ptc &
                     Region %in% c(input$SuccessPlaceRegionSelect))

          pshPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = pshPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = pshPlacementGoal,
                y1 = pshPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Permanent Supportive Housing',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }

    output$QPRSuccessfulPlacementOUT <-
      if (nrow(QPR_EEs %>% 
               filter(Region %in% c(input$SuccessPlaceRegionSelect) &
      ProjectType == 4)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 4 # Street Outreach

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                DestinationGroup == "Permanent" & # exited to ph
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(SuccessfullyPlacedHHs = n())

          # calculating the total households to compare successful placements to
          TotalHHsSuccessfulPlacement <- QPR_EEs %>%
            filter(ProjectType == ptc &
                     Region %in% c(input$SuccessPlaceRegionSelect) &
                     exited_between(., ReportStart, ReportEnd)
              ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          rm(TotalHHsSuccessfulPlacement, SuccessfullyPlaced)

          outPlacementData <-
            SuccessfulPlacement %>%
            filter(ProjectType == ptc &
                     Region %in% c(input$SuccessPlaceRegionSelect))

          outPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    Measure == "Exits to Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = outPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = outPlacementGoal,
                y1 = outPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Street Outreach',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }

    output$QPRSuccessfulPlacementSH <-
      if (nrow(QPR_EEs %>%
               filter(Region %in% c(input$SuccessPlaceRegionSelect) &
               ProjectType == 8)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 8 # Safe Haven

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                DestinationGroup == "Permanent" & # exited to ph
                   exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(SuccessfullyPlacedHHs = n())

          # calculating the total households to compare successful placements to
          TotalHHsSuccessfulPlacement <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          rm(TotalHHsSuccessfulPlacement, SuccessfullyPlaced)

          shPlacementData <-  SuccessfulPlacement
          
          shPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = shPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = shPlacementGoal,
                y1 = shPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Safe Haven',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }
    
    output$QPRSuccessfulPlacementHP <-
      if (nrow(QPR_EEs %>% 
               filter(Region %in% c(input$SuccessPlaceRegionSelect) &
      ProjectType == 12)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 12 # Homelessness Prevention

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                ((ProjectType %in% c(3, 9, 13) & !is.na(MoveInDateAdjust)) |
                   ProjectType %in% c(1, 2, 4, 8, 12)) & # excluding non-mover-inners
                ((DestinationGroup == "Permanent" &
                    (exited_between(., ReportStart, ReportEnd) | #exited to ph or still in PSH/HP
                       is.na(ExitDate))) &
                   ProjectType %in% c(3, 9, 12) # PSH & HP
                ) |
                (DestinationGroup == "Permanent" & # exited to ph
                   exited_between(., ReportStart, ReportEnd) &
                   ProjectType %in% c(1, 2, 4, 8, 13)) # ES, TH, SH, RRH, OUT
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(SuccessfullyPlacedHHs = n())

          # calculating the total households to compare successful placements to
          TotalHHsSuccessfulPlacement <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                
                (
                  served_between(., ReportStart, ReportEnd) &
                    ProjectType %in% c(3, 9, 12) # PSH & HP
                ) |
                (
                  exited_between(., ReportStart, ReportEnd) &
                    ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
                )
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          rm(TotalHHsSuccessfulPlacement, SuccessfullyPlaced)

          hpPlacementData <- SuccessfulPlacement
          
          hpPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = hpPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = hpPlacementGoal,
                y1 = hpPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Homelessness Prevention',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }
    output$QPRSuccessfulPlacementRRH <-
      if (nrow(QPR_EEs %>% filter(Region %in% c(input$SuccessPlaceRegionSelect) &
      ProjectType == 13)) > 0) {
        renderPlotly({
          ReportStart <- format.Date(ymd(paste0(
            substr(input$SuccessPlaceSlider, 1, 4),
            "-01-01"
          )), "%m-%d-%Y")
          ReportEnd <- format.Date(mdy(paste0(
            case_when(
              substr(input$SuccessPlaceSlider, 7, 7) == 1 ~ "03-31-",
              substr(input$SuccessPlaceSlider, 7, 7) == 2 ~ "06-30",
              substr(input$SuccessPlaceSlider, 7, 7) == 3 ~ "09-30-",
              substr(input$SuccessPlaceSlider, 7, 7) == 4 ~ "12-31-"
            ),
            substr(input$SuccessPlaceSlider, 1, 4)
          )), "%m-%d-%Y")

          ptc <- 13 # Rapid Rehousing

          SuccessfullyPlaced <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                !is.na(MoveInDateAdjust) & # excluding non-mover-inners
                DestinationGroup == "Permanent" & # exited to ph
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(SuccessfullyPlacedHHs = n())

          # calculating the total households to compare successful placements to
          TotalHHsSuccessfulPlacement <- QPR_EEs %>%
            filter(
              ProjectType == ptc &
                Region %in% c(input$SuccessPlaceRegionSelect) &
                exited_between(., ReportStart, ReportEnd)
            ) %>%
            group_by(FriendlyProjectName, ProjectType, County, Region) %>%
            summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served;
          # otherwise, it's total hhs *exited* during the reporting period

          SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
            left_join(SuccessfullyPlaced,
                      by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
            mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

          SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

          rrhPlacementData <- SuccessfulPlacement
          
          rrhPlacementGoal <-
            as.numeric(
              goals %>%
                filter(
                  ProjectType == ptc &
                    SummaryMeasure == "Obtaining and Maintaining Permanent Housing" &
                    !is.na(Goal)
                ) %>%
                select(Goal)
            )

          plot_ly(
            data = rrhPlacementData,
            x = ~ FriendlyProjectName,
            y = ~ Percent
          ) %>%
            add_trace(type = "bar") %>%
            layout(
              shapes = list(
                type = 'line',
                xref = "paper",
                yref = "y",
                x0 = 0,
                x1 = 1,
                y0 = rrhPlacementGoal,
                y1 = rrhPlacementGoal,
                line = list(width = 1),
                name = "CoC Goal"
              ),
              title = 'Rapid Rehousing',
              margin = list(
                l = 50,
                r = 50,
                b = 100,
                t = 100,
                pad = 4
              ),
              yaxis = list(title = "Exits to Permanent Housing",
                           showgrid = TRUE,
                           rangemode = "tozero"),
              xaxis = list(title = "",
                           showgrid = TRUE,
                           rangemode = "tozero")
            )%>%
            layout(yaxis = list(tickformat = "%"))

        })
      }
    else{

    }
    
  })
  
}