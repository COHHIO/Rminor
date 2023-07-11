
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
       # bs4Dash::bs4SidebarMenuItem(
       #   text = "COVID-19",
       #   tabName = "covid19", #covid19-Tab
       #   icon = shiny::icon("virus")
       # ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Bed & Unit Utilization",
         tabName = "utilization", #utilization-Tab
         icon = shiny::icon("bed")
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "Quarterly Performance Report",
         icon = shiny::icon("file-medical-alt"),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "System Performance Measures",
           tabName = "qpr_spm"# SPM-Tab
         ),
         bs4Dash::bs4SidebarMenuSubItem(
           text = "Community Need (by County)",
           tabName = "qpr_community_need" #spdat-Tab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "Length of Stay",
           tabName = "qpr_length_of_stay" # LoS-Tab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "Exits to Permanent Housing",
           tabName = "qpr_permanent_housing" # PHTab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "Non-Cash Benefits at Exit",
           tabName = "qpr_noncash_benefits" # NCB-Tab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "Health Insurance at Exit",
           tabName = "qpr_health_insurance" # HI-Tab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "Income Growth",
           tabName = "qpr_income_growth" # Income-Tab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "Rapid Placement for RRH",
           tabName = "qpr_rrh_placement" # RRH-Tab
         ),
         bs4Dash::bs4SidebarMenuItem(
           text = "RRH v HP Spending",
           tabName = "qpr_rrh_spending" # RRHspending-Tab
         )
       ),
       bs4Dash::bs4SidebarMenuItem(
         text = "About",
         tabName = "about",
         icon = shiny::icon("info-circle")
       ),
       actionButton(
         inputId = "logOut",
         label = "Log Out",
         onclick =
           "window.open(window.location.href + '__logout__/', '_self')"
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