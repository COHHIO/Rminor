#' body_spm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_spm_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(
      ui_picker_program(
        inputId = ns("coc"),
        choices = sort(unique(spm()$CoCCode)),
        selected = "OH-507",
        multiple = FALSE,
        width = "70%"
      )
    ),
    ui_row(
        DT::dataTableOutput(ns("spm_1")),
        title = "Measure 1: Length of Stay"
    ),
    ui_row(
      DT::dataTableOutput(ns("spm_2")),
      title = "Measure 2: Returns to Homelessness"
    ),
    ui_row(
      DT::dataTableOutput(ns("spm_3")),
      title = "Measure 3: HMIS Counts"
    ),
    ui_row(
      DT::dataTableOutput(ns("spm_4")),
      title = "Measure 4: Increased Income"
    ),
    ui_row(
      DT::dataTableOutput(ns("spm_5")),
      title = "Measure 5: First Time Homelessness"
    ),
    ui_row(
      DT::dataTableOutput(ns("spm_7")),
      title = "Measure 7: Successful Street Outreach and Placement"
    )
  )


}
    
#' body_spm Server Functions
#'
#' @noRd 
mod_body_spm_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI({
      server_header(
        "System Performance Measures"
      )
    })
      
      
      output$spm_1 <- DT::renderDT(server = FALSE, {
        req(input$coc)
        
        spm() |>
          dplyr::filter(CoCCode == input$coc) |>
          dplyr::select(FiscalYear, ESSHTHAvgTime_1A) |>
          dplyr::rename("Fiscal Year" = FiscalYear,
                        "ES-SH-TH Avgerage Length of Stay" = ESSHTHAvgTime_1A) |>
          datatable_default()
      })
      
      output$spm_2 <- DT::renderDT(server = FALSE, {
        req(input$coc)
        
        spm() |>
          dplyr::filter(CoCCode == input$coc) |>
          dplyr::mutate_if(is.character, as.numeric) |> 
          dplyr::mutate(TotalExitPH_2 = (SOExitPH_2 + ESExitPH_2 + THExitPH_2 +
                                           SHExitPH_2 + PHExitPH_2),
                        TotalReturn0to180_2 = (SOReturn0to180_2 + ESReturn0to180_2 +
                                                 THReturn0to180_2 + SHReturn0to180_2 +
                                                 PHReturn0to180_2),
                        TotalReturn181to365_2 = (SOReturn181to365_2 + ESReturn181to365_2 +
                                                 THReturn181to365_2 + SHReturn181to365_2 +
                                                 PHReturn181to365_2),
                        TotalReturn366to730_2 = (SOReturn366to730_2 + ESReturn366to730_2 +
                                                 THReturn366to730_2 + SHReturn366to730_2 +
                                                 PHReturn366to730_2),
                        TotalReturn_2 = (TotalReturn0to180_2 + TotalReturn181to365_2 +
                                         TotalReturn366to730_2),
                        PctReturn0to180_2 = (TotalReturn0to180_2 / TotalExitPH_2),
                        PctReturn181to365_2 = (TotalReturn181to365_2 / TotalExitPH_2),
                        PctReturn366to730_2 = (TotalReturn366to730_2 / TotalExitPH_2),
                        PctReturn_2 = (TotalReturn_2 / TotalExitPH_2)
                        ) |> 
          dplyr::select(FiscalYear, TotalExitPH_2, TotalReturn0to180_2,
                        TotalReturn181to365_2, TotalReturn366to730_2, TotalReturn_2,
                        PctReturn0to180_2, PctReturn181to365_2, PctReturn366to730_2,
                        PctReturn_2) |>
          dplyr::mutate(PctReturn0to180_2 = scales::percent(PctReturn0to180_2), 
                        PctReturn181to365_2 = scales::percent(PctReturn181to365_2), 
                        PctReturn366to730_2 = scales::percent(PctReturn366to730_2),
                        PctReturn_2 = scales::percent(PctReturn_2)) |> 
          dplyr::rename("Fiscal Year" = FiscalYear,
                        "Total Number of Persons who Exited to a PH Destination (2 Years Prior)" = TotalExitPH_2,
                        "Number Returning to Homelessness in Less than 6 Months" = TotalReturn0to180_2,
                        "Percentage of Returns in Less than 6 Months" = PctReturn0to180_2,
                        "Number Returning to Homelessness from 6 to 12 Months" = TotalReturn181to365_2,
                        "Percentage of Returns from 6 to 12 Months" = PctReturn181to365_2,
                        "Number of Returning to Homelessness from 13 to 24 Months" = TotalReturn366to730_2,
                        "Percentage of Returns from 13 to 24 Months" = PctReturn366to730_2,
                        "Number of Returns in 2 Years" = TotalReturn_2,
                        "Percentage of Returns in 2 Years" = PctReturn_2) |> 
          datatable_default()
      })
      
      output$spm_3 <- DT::renderDT(server = FALSE, {
        req(input$coc)
        
        spm() |>
          dplyr::filter(CoCCode == input$coc) |>
          dplyr::select(FiscalYear, TotalAnnual_3, ESAnnual_3, SHAnnual_3, THAnnual_3) |> 
          dplyr::rename("Fiscal Year" = FiscalYear,
                        "Total Counts of People in HMIS" = TotalAnnual_3,
                        "ES Total" = ESAnnual_3,
                        "SH Total" = SHAnnual_3,
                        "TH Total" = THAnnual_3) |>
          datatable_default()
      })
      
      output$spm_4 <- DT::renderDT(server = FALSE, {
        req(input$coc)
        
        spm() |>
          dplyr::filter(CoCCode == input$coc) |>
          dplyr::select(FiscalYear, 
                        AdultStayers_4,
                        IncreaseEarned4_1,
                        IncreaseOther4_2,
                        IncreaseTotal4_3,
                        AdultLeavers_4,
                        IncreaseEarned4_4,
                        IncreaseOther4_5,
                        IncreaseTotal4_6) |>
          dplyr::mutate_if(is.character, as.numeric) |>
          dplyr::mutate(PctAdultStayersIncreaseEarned = (IncreaseEarned4_1 / AdultStayers_4),
                        PctAdultStayersIncreaseOther = (IncreaseOther4_2 / AdultStayers_4),
                        PctAdultStayersIncreaseTotal = (IncreaseTotal4_3 / AdultStayers_4),
                        PctAdultLeaversIncreaseEarned = (IncreaseEarned4_4 / AdultLeavers_4),
                        PctAdultLeaversIncreaseOther = (IncreaseOther4_5 / AdultLeavers_4),
                        PctAdultLeaversIncreaseTotal = (IncreaseTotal4_6 / AdultLeavers_4)
                        ) |> 
          dplyr::mutate(PctAdultStayersIncreaseEarned = scales::percent(PctAdultStayersIncreaseEarned),
                        PctAdultStayersIncreaseOther = scales::percent(PctAdultStayersIncreaseOther),
                        PctAdultStayersIncreaseTotal = scales::percent(PctAdultStayersIncreaseTotal),
                        PctAdultLeaversIncreaseEarned = scales::percent(PctAdultLeaversIncreaseEarned),
                        PctAdultLeaversIncreaseOther = scales::percent(PctAdultLeaversIncreaseOther),
                        PctAdultLeaversIncreaseTotal = scales::percent(PctAdultLeaversIncreaseTotal)
                        ) |>
          dplyr::rename("Fiscal Year" = FiscalYear,
                        "Number of Adults (system stayers)" = AdultStayers_4,
                        "Percentage of Adult Stayers Increased Earned Income" = PctAdultStayersIncreaseEarned,
                        "Percentage of Adult Stayers Increased Non-Employment Income" = PctAdultStayersIncreaseOther,
                        "Percentage of Adult Stayers Increased Total Income" = PctAdultStayersIncreaseTotal,
                        "Number of Adults who exited" = AdultLeavers_4,
                        "Percentage of Adult Leavers Increased Earned Income" = PctAdultLeaversIncreaseEarned,
                        "Percentage of Adult Leavers Increased Non-Employment Income" = PctAdultLeaversIncreaseOther,
                        "Percentage of Adult Leavers Increased Total Income" = PctAdultLeaversIncreaseTotal) |>
          dplyr::select("Fiscal Year",
                        "Number of Adults (system stayers)",
                        "Percentage of Adult Stayers Increased Earned Income",
                        "Percentage of Adult Stayers Increased Non-Employment Income",
                        "Percentage of Adult Stayers Increased Total Income",
                        "Number of Adults who exited",
                        "Percentage of Adult Leavers Increased Earned Income",
                        "Percentage of Adult Leavers Increased Non-Employment Income" ,
                        "Percentage of Adult Leavers Increased Total Income"
                        ) |> 
          datatable_default()
      })
      
      output$spm_5 <- DT::renderDT(server = FALSE, {
        req(input$coc)
        
        spm() |>
          dplyr::filter(CoCCode == input$coc) |>
          dplyr::select(FiscalYear, EnterESSHTH5_1, ESSHTHWithPriorSvc5_1,
                        EnterESSHTHPH5_2, ESSHTHPHWithPriorSvc5_2) |>
          dplyr::mutate_if(is.character, as.numeric) |> 
          dplyr::mutate(FirstTimeESSHTH = EnterESSHTH5_1 - ESSHTHWithPriorSvc5_1,
                        FirstTimeESSHTHPH = EnterESSHTHPH5_2 - ESSHTHPHWithPriorSvc5_2) |>
          dplyr::select(FiscalYear, FirstTimeESSHTH, FirstTimeESSHTHPH) |> 
          dplyr::rename("Fiscal Year" = FiscalYear,
                        "Change in the number of persons entering ES, SH, and TH projects with no prior enrollments in HMIS" = FirstTimeESSHTH,
                        "Change in the number of persons entering ES, SH, TH, and PH projects with no prior enrollments in HMIS" = FirstTimeESSHTHPH) |>
          datatable_default()
      })
      
      output$spm_7 <- DT::renderDT(server = FALSE, {
        req(input$coc)
        
        spm() |>
          dplyr::filter(CoCCode == input$coc) |>
          dplyr::select(FiscalYear, 
                        SOExit_7,
                        SOExitTempInst_7,
                        SOExitPH_7,
                        ESSHTHRRHExit_7,
                        ESSHTHRRHToPH_7,
                        PHClients_7,
                        PHClientsStayOrExitPH_7) |>
          dplyr::mutate_if(is.character, as.numeric) |> 
          dplyr::mutate(PctSOExit = (SOExitTempInst_7 + SOExitPH_7) / SOExit_7,
                        PctESSHTHRRHExit = ESSHTHRRHToPH_7 / ESSHTHRRHExit_7,
                        PctPHExit = PHClientsStayOrExitPH_7 / PHClients_7) |> 
          dplyr::select(FiscalYear,
                        PctSOExit,
                        PctESSHTHRRHExit,
                        PctPHExit) |> 
          dplyr::mutate(PctSOExit = scales::percent(PctSOExit),
                        PctESSHTHRRHExit = scales::percent(PctESSHTHRRHExit),
                        PctPHExit = scales::percent(PctPHExit)) |> 
          dplyr::rename("Fiscal Year" = FiscalYear,
                        "Successful Street Outreach" = PctSOExit,
                        "Successful Exits from ES, SH, TH, and RRH" = PctESSHTHRRHExit,
                        "Successful Exits from PH" = PctPHExit) |>
          datatable_default()
      })
  })
}
    
## To be copied in the UI
# mod_body_spm_ui("body_spm_1")
    
## To be copied in the server
# mod_body_spm_server("body_spm_1")
