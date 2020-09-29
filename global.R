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

library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(scales)
library(plotly)
library(zoo)
library(DT)

env <- environment()

# loading the image files from the data/ folder

load("data/Rminor.RData")

# creating various lists needed in the app

choices_month <-
  format(seq.Date(
    from = as.Date(floor_date(today(), unit = "month") - years(2)),
    by = "month",
    length.out = 24
  ), "%b %Y")

choices_regions <- unique(regions$RegionName[regions$County != "Mahoning"])

choices_service_areas <- sort(unique(APs$ProjectAreaServed)) 

providers <- validation %>%
  select(ProjectName, ProjectType) %>%
  unique() %>%
  filter(str_detect(ProjectName, "zz", negate = TRUE) == TRUE &
           ProjectType %in% c(1, 2, 3, 8, 12, 13))

# the sample() function was pulling in a zz'd provider in the Provider Dashboard
# so I'm filtering out the zz'd providers because why would they ever need to
# check their Provider Dashboard? they wouldn't. Also we don't want to see APs.

provider_dash_choices <- 
  sort(providers$ProjectName) %>% 
  unique() 

provider_dash_selected <- providers %>%
  left_join(validation, by = c("ProjectName", "ProjectType")) %>%
  filter(is.na(ExitDate)) %>% 
  select(ProjectName) %>%
  unique() %>%
  arrange(ProjectName)

choices_project_type <- c(
  "Emergency Shelter", 
  "Transitional Housing", 
  "Permanent Supportive Housing",
  "Street Outreach", 
  "Safe Haven", 
  "Homelessness Prevention", 
  "Rapid Rehousing"
)
