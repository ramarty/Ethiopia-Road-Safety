# Ethiopia Road Safety: Master

# File Paths -------------------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") project_file_path <- "~/Dropbox/research/2017/ethroads/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety"
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE - Road Safety"

raw_data_file_path <- file.path(project_file_path,"Data","RawData")
intermediate_data_file_path <- file.path(project_file_path,"Data","IntermediateData")
final_data_file_path <- file.path(project_file_path,"Data","FinalData")
tables_file_path <- file.path(project_file_path,"Results","Tables")
figures_file_path <- file.path(project_file_path,"Results","Figures")
code_file_path <- file.path(project_file_path, "Code", "etre_analysis")

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
library(doBy)
library(bit64)
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

