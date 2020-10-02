#' ceAPs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 



mod_ceAPs_ui <- function(id, search_by){
  ns <- NS(id)
  tagList(
    tabItem(
      tabName = "ceAPs",
      tabsetPanel(
        type = "tabs",
        tabPanel("By County", 
                 fluidRow(box(pickerInput(
                   inputId = "ap_by_county",
                   label = "Select County/-ies",
                   options = pickerOptions(dropupAuto = FALSE,
                                           actionsBox = TRUE),
                   choices = bos_counties,
                   multiple = TRUE
                 ))),
                 fluidRow(box(
                   title = "Coordinated Entry Access Points",
                   width = 12,
                   dataTableOutput("AP_list_county")
                 ))
        ),
        tabPanel("By Service Area", 
                 fluidRow(box(pickerInput(
                   inputId = "ap_by_region",
                   label = "Select Service Area",
                   options = pickerOptions(dropupAuto = FALSE,
                                           actionsBox = TRUE),
                   choices = choices_service_areas,
                   multiple = TRUE)
                 )),
                 fluidRow(box(
                   title = "Coordinated Entry Access Points",
                   width = 12,
                   dataTableOutput("AP_list_region"))
                 )),
        tabPanel("By Organization", 
                 fluidRow(box(
                   pickerInput(
                     inputId = "ap_by_org",
                     label = "Select Organization",
                     selected = NULL,
                     options = pickerOptions(dropupAuto = FALSE,
                                             actionsBox = TRUE),
                     multiple = TRUE,
                     choices = APs %>%
                       arrange(ProjectAKA) %>%
                       pull(ProjectAKA) %>%
                       unique()
                   )
                 )), 
                 fluidRow(box(
                   title = "Coordinated Entry Access Points",
                   width = 12,
                   dataTableOutput("AP_list_org")))
        )
      ),
      fluidRow(box(
        title = "Ohio Balance of State CoC Homeless Planning Regions",
        HTML("The solid-colored counties are all part of the Ohio
                       Balance of State CoC. The Ohio Development Services 
                       Agency (ODSA) further divided the counties in the Balance
                       of State into 17 Homeless Planning Regions to make
                       implementation of state-funded programs in the Balance of
                       State more localized."),
        img(
          src =
            "Homeless-Region-Map-for-COHHIO-2017.png",
          height = '100%',
          width = '100%'
        ),
        width = 12
      )),
    ),
  )
}
    
#' ceAPs Server Functions
#'
#' @noRd 
mod_ceAPs_server <- function(id, search_by){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
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
  })
}
    
## To be copied in the UI
# mod_ceAPs_ui("ceAPs_ui_1")
    
## To be copied in the server
# mod_ceAPs_server("ceAPs_ui_1")
