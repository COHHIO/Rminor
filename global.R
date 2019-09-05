#COHHIO_HMIS
#Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published
#by the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(scales)
library(plotly)
library(zoo)

load("data/Utilization.RData")

filebeginningdate <- updatedate - years(2)

load("data/QPR_SPDATs.RData")

load("data/QPR_EEs.RData")

load("data/Veterans.RData")

load("data/Data_Quality.RData")

choices_month <-
  format(seq.Date(
    from = as.Date(floor_date(today(), unit = "month") - years(2)),
    by = "month",
    length.out = 24
  ), "%b %Y")

choices_regions <- unique(Regions$RegionName)

providers <- validation %>%
  filter(str_detect(ProjectName, "zz", negate = TRUE) == TRUE)
# the sample() function was pulling in a zz'd provider in the Provider Dashboard
# so I'm filtering out the zz'd providers because why would they ever need to
# check their Provider Dashboard? they wouldn't.
providers <- 
  sort(providers$ProjectName) %>% 
  unique() 

choices_project_type <- c(
  "Emergency Shelter", 
  "Transitional Housing", 
  "Permanent Supportive Housing",
  "Street Outreach", 
  "Safe Haven", 
  "Homelessness Prevention", 
  "Rapid Rehousing"
)
