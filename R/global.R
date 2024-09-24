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

#### functions from yogat3ch/maleta
#### removed dropbox dependency

#' Clean rds files with NULL values
#'
#' @param files \code{(chr)} of files to clean NULL saves
#'
#' @return \code{(chr)} of valid files after cleaning

clean_null <- function(files) {
  .rds <- stringr::str_subset(files, "rds$")
  .sizes <- file.size(.rds)
  file.remove(.rds[.sizes == 44])
  files[!files %in% .rds[.sizes == 44]]
}

#' Create accessor function for each dependency
#'
#' @param .x \code{(chr)} The full path of the file.
#' @param do_update \code{(lgl)} If TRUE, will update the dependency.
#' @return A function that can be used to access the dependency.
accessor_create <- function(.x, do_update) {
  rlang::new_function(
    args = rlang::pairlist2(
      path = rlang::expr(!!.x),
      dep_update = update_local,  # No more Dropbox, use local update function
      do_update = rlang::expr(!!do_update),
      ... = 
    ),
    body = base::quote({
      if (do_update) {
        dep_update(path)  # Update from local source
      }
      UU::file_fn(path)(path, ...)  # Use the file function on the path
    })
  )
}

#' Assign accessor functions to the package namespace
#'
#' @param funs A list of accessor functions to assign.
#' @param ns The package namespace where functions will be assigned.
#' @return Functions are assigned to the namespace or returned as a list.
do_assignment <- function(funs, ns = pkgload::pkg_name()) {
  if (UU::is_legit(try(ns, silent = TRUE))) {
    namespace <- rlang::ns_env(ns)
    rlang::env_unlock(namespace)
    purrr::iwalk(funs, ~{
      if (exists(.y, envir = namespace, inherits = FALSE)) {
        rlang::env_binding_unlock(namespace, .y)
      }
      assign(.y, .x, envir = namespace)
      assignInNamespace(.y, .x, ns, envir = namespace)
      rlang::env_binding_lock(namespace, .y)
    })
  } else {
    funs
  }
}

#' Update a dependency locally (no remote)
#'
#' @param filepaths \code{(chr)} A list of file paths to check and load.
#' @return Loaded dependencies.
update_local <- function(filepaths) {
  # You can add logic here to check file timestamps or reload the files
  purrr::walk(filepaths, function(filepath) {
    message(glue::glue("Checking and loading {filepath}..."))
    # Example: If file doesn't exist or is outdated, reload it
    if (!file.exists(filepath)) {
      stop(glue::glue("File {filepath} not found."))
    }
  })
}

#' Create accessor functions in the namespace (local files only)
#'
#' @param dep_dir \code{(chr)} The directory where dependencies are stored.
#' @param deps \code{(chr)} Full file paths for dependencies to be accessed.
#' @param dep_update \code{(function)} Dependency update function.
#' @param update_all \code{(lgl)} Whether to load/update all dependencies.
#' @return Accessor functions will be available in the package namespace.
#' @export
create_accessors <- function(dep_dir = "data", deps = NULL, dep_update = update_local, update_all = TRUE) {
  # Get the list of dependencies if not provided
  if (is.null(deps)) {
    deps <- clean_null(UU::list.files2(dep_dir)) |>
      stringr::str_subset("\\.png$", negate = TRUE)  # Avoid .png files
  }
  
  # Get absolute paths of dependencies
  deps <- fs::path_abs(deps)
  
  # Ensure the directory exists
  UU::mkpath(dep_dir)
  
  # Optionally update all dependencies
  if (update_all) {
    dep_update(deps)
  }
  
  # Create accessor functions for each dependency
  accessor_funs <- purrr::map(
    rlang::set_names(deps, fs::path_ext_remove(basename(deps))),
    accessor_create,
    do_update = !update_all
  )
  
  # Assign accessor functions to the namespace or return them as a list
  do_assignment(accessor_funs)
}




# Create accessor functions
Sys.setenv(TZ = "America/New_York")

.time <- system.time({
  create_accessors("data")
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

