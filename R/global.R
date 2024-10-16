# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 
# loading the image files from the data/ folder
#' @include golem_utils_server.R

# Create accessor functions
Sys.setenv(TZ = "America/New_York")
# List of data files in the data directory (handles feather, csv, rds)
data_files <- list.files("data", pattern = "\\.(feather|csv|rds)$", full.names = TRUE)

# Function to create an accessor for each file based on its extension
create_accessor <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  
  function() {
    if (file_ext == "feather") {
      data <- arrow::read_feather(file_path)
    } else if (file_ext == "csv") {
      data <- readr::read_csv(file_path)
    } else if (file_ext == "rds") {
      data <- readRDS(file_path)
    } else {
      stop("Unsupported file type: ", file_ext)
    }
    return(data)
  }
}

# Create accessors and assign them to the global environment
for (file_path in data_files) {
  func_name <- fs::path_ext_remove(basename(file_path))
  assign(func_name, create_accessor(file_path), envir = .GlobalEnv)
}


  
  # Run only if in production mode or testing
  
  
  # creating various lists needed in the app
  
  choices_month <-
    format(seq.Date(
      from = as.Date(lubridate::floor_date(lubridate::today(), unit = "month") - lubridate::years(2)),
      by = "month",
      length.out = 24
    ), "%b %Y")
  
  choices_service_areas <- sort(unique(APs()$ProjectCountyServed))

  choices_regions <- unique(Regions()$RegionName)
  

  programs <- validation() |>
    dplyr::distinct(ProjectID, ProjectName) |>
    dplyr::arrange(ProjectName) |> 
      {\(x) {rlang::set_names(x$ProjectID, x$ProjectName)}}()
  # the sample() function was pulling in a zz'd provider in the Provider Dashboard
  # so I'm filtering out the zz'd programs because why would they ever need to
  # check their Provider Dashboard? they wouldn't. Also we don't want to see APs.
  
  provider_dash_selected <- validation() |>
          dplyr::distinct(ProjectID, ProjectName) |>
          dplyr::arrange(ProjectName) |>
          dplyr::select(ProjectName) |> 
          unique() |> 
          dplyr::arrange(ProjectName)


# CHANGED Add names (which will be visible to users) and numeric values (for 
  # filtering data). Eliminates the need for mutating the data to human readable 
  # names
# NOTE entries have been alphabetized.
choices_project_type <- list(
  `Coordinated Entry` = 14,
  `Emergency Shelters` = c(0, 1),
  `Prevention` = 12,
  `Permanent Supportive Housing` = c(3, 9),
  `Rapid Rehousing` = 13,
  `Safe Haven` = 8,
  `Services Only` = 6,
  `Street Outreach` = 4,
  `Transitional Housing` = 2
)

tab_choices <-
  list(
    LoS = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Rapid Rehousing"
    ),
    PH = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    ),
    NCB = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    ),
    HI = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    ),
    Income = c(
      "Emergency Shelters",
      "Transitional Housing",
      "Safe Haven",
      "Prevention",
      "Rapid Rehousing",
      "Permanent Supportive Housing",
      "Street Outreach"
    )
  )

if (exists("Regions")) {
  regions <- Regions() |> 
    dplyr::distinct(Region, RegionName) |> 
    {\(x) {rlang::set_names(x$Region, x$RegionName)}}()
  counties <- sort(Regions()$County)
  qpr_tab_choices <- regions |>
    {\(x) {list(
      community_need_ph = list(
        choices = x
      ),
      community_need_lh = list(
        choices = x
      ),
      length_of_stay = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") & ProjectType %in% c(0, 1, 2, 8, 13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      length_of_stay_youth = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |> 
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") & ProjectType %in% c(0, 1, 2, 8, 13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      permanent_housing = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType %in% c(0:4, 8:9, 12:13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      permanent_housing_youth = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType %in% c(0:4, 8:9, 12:13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      temp_permanent_housing = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType == 4) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      temp_permanent_housing_youth = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                    ProjectType == 4) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      noncash_benefits = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      noncash_benefits_youth = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
                          dplyr::pull(ProjectName) |> unique()
          ),
      health_insurance = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      health_insurance_youth = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      income_growth = list(
        choices = qpr_income() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      income_growth_youth = list(
        choices = qpr_income() |> 
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      rrh_placement = list(
        choices = qpr_rrh_enterers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      rrh_placement_youth = list(
        choices = qpr_rrh_enterers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      reentries = list(
        choices = unique(sort(
          qpr_reentries() |>
            dplyr::filter(ProgramCoC == "OH-507") |>
            dplyr::filter(!stringr::str_detect(tolower(ExitingHP), "odh|youth|yhdp")) |> 
            dplyr::pull(ExitingHP) |> unique()
        ))
      ),
      rrh_spending = list(
        choices = unique(sort(
          qpr_spending() |>
            dplyr::filter(ProgramCoC == "OH-507") |>
            dplyr::filter(!stringr::str_detect(tolower(OrganizationName), "odh|youth|yhdp")) |> 
            dplyr::pull(OrganizationName) |> unique()
        ))
      )
    )}}()
  
}

data_ui <- list(choices_month = choices_month,
                choices_service_areas = choices_service_areas,
                choices_regions = choices_regions,
                counties = counties,
                provider_dash_choices = programs,
                choices_project_type = choices_project_type,
                tab_choices = tab_choices)

