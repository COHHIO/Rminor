library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(scales)
library(plotly)
library(zoo)



load("data/Utilization.RData")

filebeginningdate <- 
  updatedate - years(2)

load("data/QPR_SPDATs.RData")

load("data/QPR_EEs.RData")

load("data/Veterans.RData")

choices_month <-
  format(seq.Date(
    from = as.Date(floor_date(today(), unit = "month") - years(2)),
    by = "month",
    length.out = 24
  ), "%b %Y")

choices_regions <- unique(Regions$RegionName)

choices_project_type <- c(
  "Emergency Shelter", 
  "Transitional Housing", 
  "Permanent Supportive Housing",
  "Street Outreach", 
  "Safe Haven", 
  "Homelessness Prevention", 
  "Rapid Rehousing"
)
