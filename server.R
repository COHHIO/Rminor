


function(input, output, session) {
  output$res <- renderPrint({
    print(ymd(paste0(
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
      months(1) - 1)
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
               "Utilization",-ProjectID,-ProjectName,-ProjectType) %>%
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
               "Utilization",-ProjectID,-ProjectName,-ProjectType) %>%
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
          "Utilization",-ProjectID,
          -ProjectName,
          -ProjectType,
          -Month
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
  
  output$QPRLoSPlotEE <-
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
          ),
          Region = paste("Homeless Planning Region", Region)
        ) %>%
        filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
      # to be here so the selection text matches the mutated data
      
      LoSSummary <- LoSDetail %>%
        group_by(brokenProjectNames,
                 FriendlyProjectName,
                 Region,
                 County,
                 ProjectType) %>%
        summarise(Days = case_when(
                    input$radioAvgMeanLoS == "Average Days" ~ 
                      as.integer(mean(DaysinProject, na.rm = TRUE)),
                    input$radioAvgMeanLoS == "Median Days" ~
                      as.integer(median(DaysinProject, na.rm = TRUE))
                  ))
      
      esdata <- LoSSummary %>% filter(ProjectType == "Emergency Shelter")
      esLoSGoal <- as.integer(LoSGoals %>%
                                filter(ProjectType == 1) %>%
                                select(Goal))
      
      plot_ly(data = esdata,
              x = ~FriendlyProjectName,
              y = ~Days) %>%
        add_trace(type = "bar") %>%
        layout(shapes=list(type='line', 
                           xref = "paper",
                           yref = "y",
                           x0= 0,
                           x1= 1,
                           y0 = esLoSGoal,
                           y1 = esLoSGoal, 
                           line=list(width=1)
        ),
        title = 'Emergency Shelters',
        yaxis = list(title = "Average Days", showgrid = TRUE),
        xaxis = list(title = "Project Name", showgrid = TRUE))
      
    })
  
  output$QPRLoSPlotTH <-
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
          ),
          Region = paste("Homeless Planning Region", Region)
        ) %>%
        filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
      # to be here so the selection text matches the mutated data
      
      LoSSummary <- LoSDetail %>%
        group_by(brokenProjectNames,
                 FriendlyProjectName,
                 Region,
                 County,
                 ProjectType) %>%
        summarise(Days = case_when(
                    input$radioAvgMeanLoS == "Average Days" ~ 
                      as.integer(mean(DaysinProject, na.rm = TRUE)),
                    input$radioAvgMeanLoS == "Median Days" ~
                      as.integer(median(DaysinProject, na.rm = TRUE))))
      
      th <-
        ggplot(
          LoSSummary %>% filter(ProjectType == "Transitional Housing"),
          aes(x = FriendlyProjectName)
        ) +
        ylab("Average Length of Stay") +
        xlab("") +
        ggtitle("Transitional Housing", subtitle = "date range") +
        geom_col(aes(y = Days), fill = "#56B4E9") +
        geom_hline(yintercept = as.integer(LoSGoals %>%
                                             filter(ProjectType == 2) %>%
                                             select(Goal))) +
        annotate(
          "text",
          x = 0.65,
          y = as.integer(LoSGoals %>%
                           filter(ProjectType == 2) %>%
                           select(Goal)) + 1,
          label = "CoC Goal"
        ) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45))
      
      ggplotly(th)
      
    })
  
  output$QPRLoSPlotSH <-
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
          ),
          Region = paste("Homeless Planning Region", Region)
        ) %>%
        filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
      # to be here so the selection text matches the mutated data
      
      LoSSummary <- LoSDetail %>%
        group_by(brokenProjectNames,
                 FriendlyProjectName,
                 Region,
                 County,
                 ProjectType) %>%
        summarise(Days = case_when(
                    input$radioAvgMeanLoS == "Average Days" ~ 
                      as.integer(mean(DaysinProject, na.rm = TRUE)),
                    input$radioAvgMeanLoS == "Median Days" ~
                      as.integer(median(DaysinProject, na.rm = TRUE))
                  ))
      
      sh <-
        ggplot(LoSSummary %>% filter(ProjectType == "Safe Haven"),
               aes(x = FriendlyProjectName)) +
        ylab("Average Length of Stay") +
        xlab("") +
        ggtitle("Safe Haven", subtitle = "date range") +
        geom_col(aes(y = Days), fill = "#56e98c") +
        geom_hline(yintercept = as.integer(LoSGoals %>%
                                             filter(ProjectType == 8) %>%
                                             select(Goal))) +
        annotate(
          "text",
          x = 0.65,
          y = as.integer(LoSGoals %>%
                           filter(ProjectType == 8) %>%
                           select(Goal)) + 1,
          label = "CoC Goal"
        ) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45))
      
      
      ggplotly(sh)
      
    })
  
  output$QPRLoSPlotRRH <-
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
          ),
          Region = paste("Homeless Planning Region", Region)
        ) %>%
        filter(Region %in% c(input$LoSRegionSelect)) # this filter needs
      # to be here so the selection text matches the mutated data
      
      LoSSummary <- LoSDetail %>%
        group_by(brokenProjectNames,
                 FriendlyProjectName,
                 Region,
                 County,
                 ProjectType) %>%
        summarise(Days = case_when(
                    input$radioAvgMeanLoS == "Average Days" ~ 
                      as.integer(mean(DaysinProject, na.rm = TRUE)),
                    input$radioAvgMeanLoS == "Median Days" ~
                      as.integer(median(DaysinProject, na.rm = TRUE))
                  ))
      
      rrh <-
        ggplot(
          LoSSummary %>% filter(ProjectType == "Rapid Rehousing"),
          aes(x = FriendlyProjectName)
        ) +
        ylab("Average Length of Stay") +
        xlab("") +
        ggtitle("Rapid Rehousing", subtitle = "date range") +
        geom_col(aes(y = Days), fill = "#ba56e9") +
        geom_hline(yintercept = as.integer(LoSGoals %>%
                                             filter(ProjectType == 13) %>%
                                             select(Goal))) +
        annotate(
          "text",
          x = 0.65,
          y = as.integer(LoSGoals %>%
                           filter(ProjectType == 13) %>%
                           select(Goal)) + 1,
          label = "CoC Goal"
        ) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45))
      
      ggplotly(rrh)
      
      
    })
  
}