#' body_welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton("browser", "browser"),
    tags$script("$('#browser').hide();"),
    ui_row(
      title = "About",
      bs4Dash::bs4Accordion(
        id = "about",
        bs4Dash::bs4AccordionItem(
          title = "About R Minor",
          tags$p(
            "R minor is a free and open source project created and maintained by the HMIS team at Coalition on Homelessness and Housing in Ohio (COHHIO) for the Ohio Balance of State CoC and the Mahoning County CoC. Please find the code ",
            tags$a(href = "https://github.com/COHHIO/Rminor", "here.", target = "_blank"),
            " R minor contains up-to-date aggregate reporting at the project and system level. Its target audience is community planners, agency management, funders, and HMIS users who are looking for more system- level reporting."
          ),
          collapsed = FALSE
        ),
        bs4Dash::bs4AccordionItem(
          title = "Ohio Balance of State CoC",
          tags$p(
            "The Ohio Balance of State Continuum of Care (BoSCoC) is the homeless 
            response system that is comprised of 80 of Ohio's 88 counties. The 
            Ohio Department of Development (ODOD) and the Coalition on  Homelessness
            and Housing in Ohio (COHHIO) provide system leadership and staff support
            to the CoC. ODOD serves as the Ohio BoSCoC Collaborative Applicant,
            while COHHIO serves as the HMIS Lead Agency and provides primary support
            to the CoC."
          ),
          htmltools::img(
            src = "www/MapBoS.png",
            height = '25%',
            width = '25%'
          )
        ),
        bs4Dash::bs4AccordionItem(
          title = "Mahoning County CoC",
          tags$p(
            "The Mahoning County Homeless Continuum of Care (MCHCoC) is one of nine Continua of Care in the State of Ohio and is the planning body for homeless services in Mahoning County. The Board of Mahoning County Commissioners serves as the Collaborative Applicant, HMIS Lead and employs the CoC Coordinator for the MCHCoC, while COHHIO serves as HMIS System Administrators."
          ),
          htmltools::img(
            src = "www/MapMahoningCountyCoC.png",
            height = '25%',
            width = '25%'
          )
        ),
        bs4Dash::bs4AccordionItem(
          title = "Ohio Balance of State CoC Homeless Planning Regions",
          tags$p(
            "To facilitate funding distribution and support system coordination
            efforts, the Ohio BoSCoC is divided into 17 Homeless Planning Regions.
            Throughout R minor, you will notice references to Homeless Planning Regions. 
            Please consult this map if you are unsure what Region your county is in."
          ),
          htmltools::img(
            src = "www/BoS-Region-Map-2023.png",
            height = '50%',
            width = '50%'
          )
        ),
        bs4Dash::bs4AccordionItem(
          title = "Citations",
          tags$p(
            "This project would not
              exist were it not for the existence of other quality free and open
              source products. Following are citations for the products R minor
              relies on."
          ),
          tags$p(
            "R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.",
            tags$a(href = "https://www.R-project.org/", "R programming language", target  = "_blank")
          ),
          tags$p(
            "Hadley Wickham (2017). tidyverse: Easily Install and Load
              the 'Tidyverse'. R package version 1.2.1.",
            tags$a(href = "https://CRAN.R-project.org/package=tidyverse", "Tidyverse package", target = "_blank")
          ),
          tags$p(
            "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and
              Jonathan McPherson (2019). shiny: Web Application Framework for R. R package version 1.3.2.",
            tags$a(href = "https://CRAN.R-project.org/package=shiny", "R Shiny package", target = "_blank"),
            "and shinydashboard: Create Dashboards with 'Shiny'. R package version 0.7.1.",
            tags$a(href = "https://CRAN.R-project.org/package=shinydashboard", "shinydashboard package", target = "_blank")
          )
        )
      )
    )
  )
}
    
#' body_welcome Server Functions
#'
#' @noRd 
mod_body_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_body_welcome_ui("body_welcome_1")
    
## To be copied in the server
# mod_body_welcome_server("body_welcome_1")
