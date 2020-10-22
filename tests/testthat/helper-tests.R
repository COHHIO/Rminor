withr::with_message_sink("~/R/Contributor_Repos/COHHIO/Rminor.RCheck/tests.log", {
  message(paste0("testing: ", testthat::is_testing()))
  if (golem::app_prod() || testthat::is_testing()) {
    # Run only if in production mode or testing
    env <- environment()
    
    # loading the image files from the data/ folder
    find_and_load("Rminor.RData", env)
    message("Data Loaded")
    message(paste0(ls(), collapse  = ", "))
    # creating various lists needed in the app
    
    choices_month <-
      format(seq.Date(
        from = as.Date(lubridate::floor_date(lubridate::today(), unit = "month") - lubridate::years(2)),
        by = "month",
        length.out = 24
      ), "%b %Y")
    
    choices_service_areas <- sort(unique(APs$ProjectAreaServed)) 
    
    choices_regions <- unique(regions$RegionName[regions$County != "Mahoning"])
    
    providers <- validation %>%
      dplyr::select(ProjectName, ProjectType) %>%
      unique() %>%
      dplyr::filter(stringr::str_detect(ProjectName, "zz", negate = TRUE) == TRUE &
                      ProjectType %in% c(1, 2, 3, 8, 12, 13))
    
    # the sample() function was pulling in a zz'd provider in the Provider Dashboard
    # so I'm filtering out the zz'd providers because why would they ever need to
    # check their Provider Dashboard? they wouldn't. Also we don't want to see APs.
    
    provider_dash_choices <-
      sort(providers$ProjectName) %>%
      unique()
    
    provider_dash_selected <- providers %>%
      dplyr::left_join(validation, by = c("ProjectName", "ProjectType")) %>%
      dplyr::filter(is.na(ExitDate)) %>%
      dplyr::select(ProjectName) %>%
      unique() %>%
      dplyr::arrange(ProjectName)
    
    # CHANGED Add names (which will be visible to users) and numeric values (for filtering data). Eliminates the need for mutating the data to human readable names
    # NOTE entries have been alphabetized.
    choices_project_type <- list(
      `Coordinated Entry` = 14,
      `Emergency Shelters` = 1, 
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
        )
      )
  }
})
