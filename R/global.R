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

.time <- system.time({
  maleta::create_accessors("data")
})

  
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
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(0, 1, 2, 8, 13)])
      ),
      permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(0:4, 8:9, 12:13)])
      ),
      temp_permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(4)])
      ),
      noncash_benefits = list(
        choices = unique(qpr_benefits()$ProjectName)
      ),
      health_insurance = list(
        choices = unique(qpr_benefits()$ProjectName)
      ),
      income_growth = list(
        choices = unique(qpr_income()$ProjectName)
      ),
      rrh_placement = list(
        choices = unique(sort(
          qpr_rrh_enterers()$ProjectName
        ))
      ),
      reentries = list(
        choices = unique(sort(
          qpr_reentries()$ExitingHP
        ))
      ),
      rrh_spending = list(
        choices = unique(sort(
          qpr_spending()$OrganizationName
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

