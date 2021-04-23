# Ethiopia Road Safety: Master

# File Paths -------------------------------------------------------------------
if(Sys.info()[["user"]] == "robmarty") dropbox_dir <- "~/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety"
if(Sys.info()[["user"]] == "WB521633") dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety"

if(Sys.info()[["user"]] == "robmarty") github_dir <- "~/Documents/Github/Ethiopia-Road-Safety"

## Data directories
data_dir         <- file.path(dropbox_dir, "Data")
etre_crashes_dir <- file.path(data_dir, "ETRE - Crashes")
etre_traffic_dir <- file.path(data_dir, "ETRE - Traffic")
rsdp_dir         <- file.path(data_dir, "RSDP Roads")
aae_dir          <- file.path(data_dir, "Addis Adama Expressway")
precip_dir       <- file.path(data_dir, "Precipitation")
holidays_dir     <- file.path(data_dir, "Holidays")
dailyhourly_dir  <- file.path(data_dir, "Daily and Hourly Data")

## Github Directories
functions_dir <- file.path(github_dir, "Functions")
datawork_dir <- file.path(github_dir, "DataWork")

# Packages ---------------------------------------------------------------------
library(sf)
library(leaflet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(rgdal)
library(data.table)
library(rgeos)
library(haven)
library(bit64)
library(dplyr)
library(data.table)
library(ncdf4)
library(stargazer)
library(grid)
library(gridExtra)
library(xtable)
library(ggmap)
library(lfe)
library(lubridate)

source(file.path(functions_dir, "turning_angle.R"))

# Run Scripts ------------------------------------------------------------------
if(F){

  # Clean Individual Datasets --------------------------------------------------
  ## Addis Adama Expressway (Polyline and Points)
  source(file.path(datawork_dir, "Addis Ababa Expressway", "01_clean_expressway", "01_create_expressway_file.R"))
  source(file.path(datawork_dir, "Addis Ababa Expressway", "01_clean_expressway", "02_create_points_along_expressway.R"))
  
  ## ETRE Data (Crashes and Traffic)
  source(file.path(datawork_dir, "ETRE - Crashes", "clean_crashes.R"))
  source(file.path(datawork_dir, "ETRE - Traffic", "append_clean_traffic_data.R"))
  
  ## Precipitation near expressway
  source(file.path(datawork_dir, "Precipitation", "daily_precipitation.R"))
  
  # Daily/Hourly Data and Analysis ---------------------------------------------
  ## Clean Data
  source(file.path(datawork_dir, "Daily and Hourly Data", "01_create_daily_hourly_daily_datasets.R"))
  
  # Segment Level Data and Analysis --------------------------------------------
  ## Clean Data
  source(file.path(datawork_dir, "Addis Ababa Expressway", "02_add_data_to_points", "01_crashes.R"))
  source(file.path(datawork_dir, "Addis Ababa Expressway", "02_add_data_to_points", "01_dist_toll_plazas.R"))
  source(file.path(datawork_dir, "Addis Ababa Expressway", "02_add_data_to_points", "01_turn_angle.R"))
  source(file.path(datawork_dir, "Addis Ababa Expressway", "02_add_data_to_points", "02_merge_clean.R"))
}

