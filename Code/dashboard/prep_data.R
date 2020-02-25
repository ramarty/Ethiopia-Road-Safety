library(tidyverse)
library(dplyr)
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
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)
library(raster)
library(stargazer)
library(ggalluvial)
library(rsconnect)

rm(list = ls())

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

# Load and Prep Hourly Data ----------------------------------------------------
load(file.path(intermediate_data_file_path, "traffic_accidents_hourly_long.Rda"))

dataset_hours_long$friday_saturday <- dataset_hours_long$day_of_week %in% c("Friday", "Saturday")

dataset_hours_long$night_earlymorning <- (dataset_hours_long$hour %in% c(19:23,0:1)) %>% as.numeric
dataset_hours_long$friday_saturday <- (dataset_hours_long$day_of_week %in% c("Friday","Saturday")) %>% as.numeric
dataset_hours_long$direction <- as.character(dataset_hours_long$direction) %>% as.factor
dataset_hours_long$year <- substring(dataset_hours_long$day,1,4) %>% as.numeric

# Load and Prep Daily Data ----------------------------------------------------
load(file.path(intermediate_data_file_path, "traffic_accidents_daily_long.Rda"))

dataset_days_long$friday_saturday <- dataset_days_long$day_of_week %in% c("Friday", "Saturday")

# Load and Prep Expressway Points ----------------------------------------------
load(file.path(intermediate_data_file_path, "addis_adama_points_data.Rda"))

addis_adama_points_data_subset <- addis_adama_points_data
addis_adama_points_data_subset$km_round <- round(addis_adama_points_data_subset$distance_adama_direction/1000)
addis_adama_points_data_subset_round <- summaryBy(traffic_2015+traffic_to_adama_2015+traffic_to_addis_2015+
                                                    traffic_2016+traffic_to_adama_2016+traffic_to_addis_2016+
                                                    traffic_2017+traffic_to_adama_2017+traffic_to_addis_2017 ~ km_round,
                                                  data=addis_adama_points_data_subset,FUN=mean,keep.names=T)

# Load and Prep Accident Data --------------------------------------------------
load(file.path(intermediate_data_file_path, "accident_data.Rda"))

accidents_all$long <- accidents_all$longitude
accidents_all$lat <- accidents_all$latitude

coordinates(accidents_all) <- ~longitude+latitude
crs(accidents_all) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#accidents_all$accident_cause_simple %>% table
#accidents_all$accident_type_simple %>% table

accidents_all$accident_cause_simple_num <- NA
accidents_all$accident_cause_simple_num[grepl("Animal Crossing", accidents_all$accident_cause_simple)] <- 1
accidents_all$accident_cause_simple_num[grepl("Brake Problem", accidents_all$accident_cause_simple)] <- 2
accidents_all$accident_cause_simple_num[grepl("Fatigue", accidents_all$accident_cause_simple)] <- 3
accidents_all$accident_cause_simple_num[grepl("Flat Tire/Tire Problem", accidents_all$accident_cause_simple)] <- 4
accidents_all$accident_cause_simple_num[grepl("Speeding", accidents_all$accident_cause_simple)] <- 5
accidents_all$accident_cause_simple_num[grepl("Steering Wheel Malfunction", accidents_all$accident_cause_simple)] <- 6
accidents_all$accident_cause_simple_num[grepl("Technical Malfunction", accidents_all$accident_cause_simple)] <- 7
accidents_all$accident_cause_simple_num[grepl("Unethical Driving", accidents_all$accident_cause_simple)] <- 8
accidents_all$accident_cause_simple_num[grepl("Vehicle Crush", accidents_all$accident_cause_simple)] <- 9
accidents_all$accident_cause_simple_num[grepl("Other", accidents_all$accident_cause_simple)] <- 10
accidents_all$accident_cause_simple_num[grepl("Not Specified", accidents_all$accident_cause_simple)] <- 11

accidents_all$accident_type_simple_num <- NA
accidents_all$accident_type_simple_num[grepl("Crush", accidents_all$accident_type_simple)] <- 1
accidents_all$accident_type_simple_num[grepl("Crush with Gaurdrail", accidents_all$accident_type_simple)] <- 2
accidents_all$accident_type_simple_num[grepl("Crushed with Property", accidents_all$accident_type_simple)] <- 3
accidents_all$accident_type_simple_num[grepl("Front Back Crushed", accidents_all$accident_type_simple)] <- 4
accidents_all$accident_type_simple_num[grepl("Overthrow", accidents_all$accident_type_simple)] <- 5
accidents_all$accident_type_simple_num[grepl("Vehicle to Vehicle Crush", accidents_all$accident_type_simple)] <- 6
accidents_all$accident_type_simple_num[grepl("Crush, Other", accidents_all$accident_type_simple)] <- 7
accidents_all$accident_type_simple_num[grepl("Other", accidents_all$accident_type_simple)] <- 8
accidents_all$accident_type_simple_num[grepl("Not Specified", accidents_all$accident_type_simple)] <- 9

accidents_all$vehicle_type_str <- NA
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 1] <- "Small Auto"
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 2] <- "Minibus"
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 3] <- "Medium Bus/ISUZU"
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 4] <- "Big Size Bus"
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 5] <- "Heavy Truck" # Heavy Truck [4 Axle]
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 6] <- "Heavy Truck" # Heavy Truck [5 Axle]
accidents_all$vehicle_type_str[accidents_all$`Vehicle Type (V)` %in% 7] <- "Heavy Truck" # Heavy Truck [6 Axle]

accidents_all$year <- substring(accidents_all$`Accident Date`,1,4) %>% as.numeric

accidents_all$km <- round(accidents_all$accident_loc_dist / 1000)
accidents_all$N <- 1

accidents_all$hour <- accidents_all$hour - 1 #1-24, but traffic goes from 0-23
accidents_all$hour_str <- as.character(accidents_all$hour)
accidents_all$hour_str[accidents_all$hour %in% 0:9] <- paste0("0",accidents_all$hour_str[accidents_all$hour %in% 0:9])
accidents_all$dayhour <- paste(accidents_all$`Accident Date`, accidents_all$hour_str)

accidents_all$dayhour[grepl("NA|1900", accidents_all$dayhour)] <- NA

addis_adama_points_data_subset <- subset(addis_adama_points_data, select=c(distance_adama_direction, 
                                                                           traffic_2015, traffic_to_adama_2015, traffic_to_addis_2015,
                                                                           traffic_2016, traffic_to_adama_2016, traffic_to_addis_2016,
                                                                           traffic_2017, traffic_to_adama_2017, traffic_to_addis_2017))
accidents_all <- merge(accidents_all, addis_adama_points_data_subset, by.x="accident_loc_dist",by.y="distance_adama_direction",all.x=T,all.y=F)

accidents_all$Day[grepl("Sun", accidents_all$Day)] <- "Sunday"
accidents_all$Day[grepl("Mon", accidents_all$Day)] <- "Monday"
accidents_all$Day[grepl("Tus|Tusday", accidents_all$Day)] <- "Tuesday"
accidents_all$Day[grepl("Wedn|Wedns|Wednsday|Wends|Wens|Wesdn|Wensday", accidents_all$Day)] <- "Wednesday"
accidents_all$Day[grepl("Thur|Thure|Thuresd", accidents_all$Day)] <- "Thursday"
accidents_all$Day[grepl("Firday", accidents_all$Day)] <- "Friday"
accidents_all$Day[grepl("Sat|Satu", accidents_all$Day)] <- "Saturday"
accidents_all$Day[!grepl("Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday", accidents_all$Day)] <- NA

accidents_all$Weather[grepl("Claudy|Cloud|Half Suny", accidents_all$Weather)] <- "Cloudy"
accidents_all$Weather[grepl("Drizzle|Heavy rair|Light shower", accidents_all$Weather)] <- "Rainy"
accidents_all$Weather[grepl("Wind|Windy", accidents_all$Weather)] <- "Rainy"
accidents_all$Weather[grepl("Suny", accidents_all$Weather)] <- "Sunny"

accident_cause_simple_list <- sort(unique(accidents_all$accident_cause_simple))
accident_type_simple_list <- sort(unique(accidents_all$accident_type_simple))

# Crush to Crash ---------------------------------------------------------------
accidents_all$accident_cause_simple <- gsub("Crush", "Crash", accidents_all$accident_cause_simple)
accidents_all$accident_type_simple <- gsub("Crush", "Crash", accidents_all$accident_type_simple)
accidents_all$accident_cause_vehicle_human <- gsub("Crush", "Crash", accidents_all$accident_cause_vehicle_human)

accident_cause_simple_list <- gsub("Crush", "Crash", accident_cause_simple_list)
accident_type_simple_list <- gsub("Crush", "Crash", accident_type_simple_list)

accidents_all$vehicle_type_str[is.na(accidents_all$vehicle_type_str)] <- "Unknown"
vehicle_type_str_list <- unique(accidents_all$vehicle_type_str)

# Save Data --------------------------------------------------------------------
save(list = ls(all.names = TRUE), file=file.path(project_file_path, "Dashboard", "data_for_shiny.Rda"))


