
#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  refreshed <- purrr::map(list.files(path = "data", full.names = TRUE), ~file.info(.x)$mtime) |> {\(x) {do.call(c, x)}}() |> max()
   bs4Dash::bs4DashSidebar(
     title = HTML("<small>Rminor</small>"),
     status = "white",
     id = "sidebar",
     skin = "light",
     elevation = 4,
     collapsed = TRUE,
     expandOnHover = TRUE,
     fixed = FALSE,
     bs4Dash::bs4SidebarMenu(
       id = "active_tab",
       compact = TRUE,
       bs4Dash::bs4SidebarMenuItem(
         text = "Welcome",
         tabName = "welcome", #providerDashboard-Tab
         icon = shiny::icon("home")
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Program Dashboard",
         tabName = "client_counts", #providerDashboard-Tab
         icon = shiny::icon("users")
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "BoS CoC Competition",
         tabName = "coc_competition", # cocCompetitionTab
         icon = shiny::icon("flag-checkered")
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Coordinated Entry Access Points",
         tabName = "ceaps", #ceAPs-Tab
         icon = shiny::icon("door-open")
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Bed & Unit Utilization",
         tabName = "utilization", #utilization-Tab
         icon = shiny::icon("bed")
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Ohio BoS Performance",
         tabName = "qpr", # quarterly performance tab
         icon = shiny::icon("file-medical-alt"),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "System Performance Measures",
           tabName = "spm"# SPM-Tab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Performance Summary",
           tabName = "performance_summary"
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Program Details",
           tabName = "program_details"
         ),
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Ohio Youth Performance",
         tabName = "qpr", # quarterly performance tab
         icon = shiny::icon("child"),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Youth Performance Summary",
           tabName = "performance_summary_youth"
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Length of Stay",
           tabName = "qpr_length_of_stay_youth" # LoS-Tab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Exits to Permanent Housing",
           tabName = "qpr_permanent_housing_youth" # PHTab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Exits to Temp or Permanent Housing",
           tabName = "qpr_temp_permanent_housing_youth" # PHTab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Non-Cash Benefits at Exit",
           tabName = "qpr_noncash_benefits_youth" # NCB-Tab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Health Insurance at Exit",
           tabName = "qpr_health_insurance_youth" # HI-Tab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Income Growth",
           tabName = "qpr_income_growth_youth" # Income-Tab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Rapid Placement for RRH",
           tabName = "qpr_rrh_placement_youth" # RRH-Tab
         )
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "About",
         tabName = "about",
         icon = shiny::icon("info-circle")
       ),
       bs4Alert(
         tags$strong("Data refreshed: ",tags$br(), refreshed),
         id = "data_refresh",
         status = purrr::when(
           refreshed,
           . > Sys.Date() ~ "success",
           . > Sys.Date() - 7 ~ "warning",
           ~ "danger"
         ),
         width = 12
       )
     )

   )
}

#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}


## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")