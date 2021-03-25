# Ethiopia Road Safety: Master

# File Paths -------------------------------------------------------------------
if(Sys.info()[["user"]] == "robmarty") dropbox_dir <- "~/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety"
if(Sys.info()[["user"]] == "WB521633") dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety"

## Data directories
data_dir         <- file.path(dropbox_dir, "Data")
etre_crashes_dir <- file.path(data_dir, "ETRE - Crashes")
etre_traffic_dir <- file.path(data_dir, "ETRE - Traffic")
rsdp_dir         <- file.path(data_dir, "RSDP Roads")
aae_dir          <- file.path(data_dir, "Addis Adama Expressway")
precip_dir       <- file.path(data_dir, "Precipitation")

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

# Run Scripts ------------------------------------------------------------------
if(F){

  # Prepare Data ---------------------------------------------------------------
  # Creates precipitation dataset
  daily_precipitation.R
  
  # Create single dataset with all accidents / traffic
  append_clean_accident_data.R
  append_clean_traffic_data.R
  
  create_accident_segment_dataset.R
  create_accident_traffic_hourly_daily_datasets.R
  create_traffic_segment_dataset.R
  
  # Analysis -------------------------------------------------------------------
  figures_maps.R
  

  
}

