

function(input, output, session) {
  # output$res <- renderPrint({
  #  # input$LoSRegionSelect
  # cat("Length of Stay", input$LoSRegionSelect)
  # })
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
                             as.integer(mean(DaysinProject, na.rm = TRUE)),
                           input$radioAvgMeanLoS == "Median Days" ~
                             as.integer(median(DaysinProject, na.rm = TRUE))
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
            xaxis = list(title = ~ FriendlyProjectName),
            yaxis = list(title = "Exited to Permanent Housing",
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
                xaxis = list(title = ~ FriendlyProjectName),
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
}