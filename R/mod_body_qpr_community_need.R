#' body_qpr_community_need UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_community_need_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(ui_picker_program(label = "Planning Region",
                             inputId = ns("region"),
                             choices = choices_regions[choices_regions != "Mahoning County CoC"],
                             selected = "Homeless Planning Region 1",
                             width = "70%",
                             multiple = FALSE)),
    ui_date_range(start = lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::years(1)),
    ui_row(
      title = "Community Need",
      DT::dataTableOutput(ns("SPDATScoresByCounty")),
      width = 12
    )
  )
}

#' body_qpr_community_need Server Functions
#'
#' @noRd 
mod_body_qpr_community_need_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$header <- renderUI({
      server_header(title = "Community Need", date_range = input$date_range)
    })
    
    
    server_debounce(input$date_range, input$region)
    
    
    Compare <- eventReactive(c(input$date_range, input$region), {
      req(input$date_range, input$region)
      # counting all hhs who were scored AND SERVED between the report dates
      CountyAverageScores <- qpr_spdats_county() %>%
        dplyr::filter(HMIS::served_between(., input$date_range[1], input$date_range[2])) %>%
        dplyr::select(CountyServed, PersonalID, Score) %>%
        dplyr::distinct() %>%
        dplyr::group_by(CountyServed) %>%
        dplyr::summarise(AverageScore = round(mean(Score, na.rm = TRUE), 1),
                         HHsLHinCounty = dplyr::n())
      # counting all hhs who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- qpr_spdats_project() %>%
        dplyr::filter(HMIS::entered_between(., input$date_range[1], input$date_range[2])) %>%
        dplyr::group_by(CountyServed) %>%
        dplyr::summarise(HousedAverageScore = round(mean(ScoreAdjusted, na.rm = TRUE), 1),
                         HHsHousedInCounty = dplyr::n())
      
      # pulling in both averages for each county plus adding Region for grouping
      Compare <-
        dplyr::full_join(CountyAverageScores,
                         CountyHousedAverageScores,
                         by = "CountyServed") %>%
        dplyr::arrange(CountyServed) %>%
        dplyr::left_join(., Regions(), by = c("CountyServed" = "County")) %>%
        dplyr::filter(RegionName == input$region)
    }) |> 
      debounce(1500)
  
    output$SPDATScoresByCounty <-
      DT::renderDT({
        ctable <- Compare() 
        ctable |> datatable_default(escape = FALSE)})}
  )
}

## To be copied in the UI
# mod_body_qpr_community_need_ui("body_qpr_community_need_1")

## To be copied in the server
# mod_body_qpr_community_need_server("body_qpr_community_need_1")
