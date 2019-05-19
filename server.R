

function(input, output, session) {
  output$res <- renderPrint({
    print(ymd(paste0(substr(input$utilizationSlider, 
                            str_length(input$utilizationSlider) - 4,
                            str_length(input$utilizationSlider)),
                     substr(input$utilizationSlider, 1, 
                            str_length(input$utilizationSlider) - 5),
                     "01")) +
            months(1) - 1
          )
  })
  observeEvent(c(input$providerList), {
    output$currentHHs <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Households",
            color = "aqua",
            icon = icon("users"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(Households)
          )
        })
      }
    else{
      
    }
    
    output$currentUnits <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Unit Capacity",
            color = "aqua",
            icon = icon("building"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitCount)
          )
        })
      }
    else{
      
    }
    

    output$currentUnitUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Unit Utilization",
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
    
    output$currentClients <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Clients",
            color = "purple",
            icon = icon("user"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(Clients)
          )
        })
      }
    else{
      
    }
    
    output$currentBeds <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Bed Capacity",
            color = "purple",
            icon = icon("bed"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedCount)
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
            "Current Bed Utilization",
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
  })

  output$SPDATScoresByCounty <- 
    renderPlot({
      # ReportStart <- format.Date(mdy(paste0("01-01-", input$y)), "%m-%d-%Y")
      ReportStart <- format.Date(ymd(paste0(
        substr(input$spdatSlider, 1, 4), 
        "-01-01")), "%m-%d-%Y")
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
        geom_point(size = 14, shape = 95) +
        scale_y_continuous(limits = c(0,17)) +
        geom_point(
          aes(y = HousedAverageScore),
          size = 8,
          shape = 17,
          colour = "#56B4E9"
        ) +
        xlab("County Where Served") +
        ylab("Average SPDAT Score") +
        labs(title = input$regionList, subtitle = paste("Date Range:", ReportStart, "to", ReportEnd)) +
        theme_light() + 
        theme(plot.title = element_text(lineheight = 5, size = rel(1.8)),
              axis.text.x = element_text(size = rel(1.8)),
              axis.text.y = element_text(size = rel(1.8)),
              plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
              ) +
        labs(
          caption = "VI-SPDAT scores and household enrollment data comes from
          the Ohio Balance of State CoC HMIS. Detail may be found at R minor 
          elevated."
        )
    })
  
  output$bedPlot <-
    renderPlot({
      ReportEnd <- ymd(paste0(substr(input$utilizationSlider, 
                                     str_length(input$utilizationSlider) - 4,
                                     str_length(input$utilizationSlider)),
                              substr(input$utilizationSlider, 1, 
                                     str_length(input$utilizationSlider) - 5),
                              "01")) +
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
        filter(ProjectName == input$providerListUtilization,
               mdy(Month) %within% ReportingPeriod) %>%
        mutate(Month = floor_date(mdy(Month), unit = "month"),
               Bed = Utilization,
               Utilization = NULL)
      
      unitPlot <- UnitUtilization %>% select(-FilePeriod) %>%
        gather("Month",
               "Utilization",
               -ProjectID,
               -ProjectName,
               -ProjectType) %>%
        filter(ProjectName == input$providerListUtilization,
               mdy(Month) %within% ReportingPeriod) %>%
        mutate(Month = floor_date(mdy(Month), unit = "month"),
               Unit = Utilization,
               Utilization = NULL)
      
      utilizationPlot <- unitPlot %>%
        full_join(bedPlot,
                  by = c("ProjectID", "ProjectName", "ProjectType", "Month")) %>%
        gather("UtilizationType",
               "Utilization",
               -ProjectID, -ProjectName, -ProjectType, -Month) %>%
        arrange(Month)
      
      ggplot(utilizationPlot,
             aes(x = Month,
                 y = Utilization,
                 color = UtilizationType)) +
        theme_light() +
        geom_line() +
        scale_y_continuous(limits = c(0, 2),
                           labels = scales::percent_format(accuracy = 1)) +
        scale_x_date(date_labels = "%B %Y", date_breaks = "3 months",
                     minor_breaks = "1 month") +
        scale_colour_manual(values = c("#56B4E9", "#6be956"))+
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
  
  output$QPRLoSPlot <- 
    renderPlot({
      ReportStart <- format.Date(ymd(paste0(
        substr(input$LoSSlider, 1, 4), 
        "-01-01")), "%m-%d-%Y")
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$LoSSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$LoSSlider, 7, 7) == 2 ~ "06-30",
          substr(input$LoSSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$LoSSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$LoSSlider, 1, 4)
      )), "%m-%d-%Y")
      
      LoSDetail <- QPR_EEs %>% 
        filter(((!is.na(MoveInDateAdjust) & ProjectType %in% c(3, 9, 13)) |
                  ProjectType %in% c(1, 2, 4, 8, 12)) &
                 !is.na(ExitDate) &
                 exited_between(., ReportStart, ReportEnd)
                 ) 
      
      LoSSummary <- LoSDetail %>%
        mutate(Short_Provider = paste(substr(ProjectName, 1, 10), "...", 
                                      substr(ProjectName, 
                                             str_length(ProjectName)-10, 
                                             str_length(ProjectName))),
               ProjectType = case_when(
                 ProjectType == 1 ~ "Emergency Shelter",
                 ProjectType == 2 ~ "Transitional Housing",
                 ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
                 ProjectType == 4 ~ "Street Outreach",
                 ProjectType == 8 ~ "Safe Haven",
                 ProjectType == 12 ~ "Prevention",
                 ProjectType == 13 ~ "Rapid Rehousing"
               )) %>%
        group_by(ProjectName, Short_Provider, ProjectType) %>%
        summarise(avg = mean(DaysinProject, na.rm = TRUE),
                  median = median(DaysinProject, na.rm = TRUE))
      
      ggplot(LoSSummary, 
             aes(x = Short_Provider)) +
        geom_col(aes(y = as.numeric(avg), fill = ProjectType)) +
        theme(axis.text.x = element_text(angle = 45)) +
        xlab("Project Name") + 
        ylab("Average")
    })
}