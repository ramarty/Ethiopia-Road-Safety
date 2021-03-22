# Ethiopia Road Safety: Prepare Accident Data

# Run master to setup ----------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

# Load and Append Accident Data ------------------------------------------------
accident_vars <- c("Accident Date", 
                   "Accident Location", 
                   "Address", 
                   "Case No", 
                   "Cause of Accident", 
                   "Day",                  
                   "Direction", "Driver Age",
                   "ETRE Asset Damage",
                   "Experience",
                   "Extent of Damage",
                   "Fatality",             
                   "Gender",
                   "HASHED Plate Number",
                   "License  year",
                   "Light Injury",
                   "Owner",
                   "Ownership",
                   "relation with vehicle",
                   "Road",
                   "Serious Injury",
                   "Time of Accident",
                   "Type of accident",
                   "Vehicle Type (V)",
                   "Weather",
                   "Year of Production") 

accidents_2015 <- read_excel(file.path(raw_data_file_path,"etre","Research Data","AAE Crush Research Data","2015 AAE Crush Research Data.xlsx")) %>%
  dplyr::select(accident_vars) %>%
  dplyr::mutate(`Case No` = as.character(`Case No`)) %>%
  dplyr::mutate(`Driver Age` = as.character(`Driver Age`))

accidents_2016 <- read_excel(file.path(raw_data_file_path,"etre","Research Data","AAE Crush Research Data","2016 AAE Crush Research Data.xlsx")) %>%
  dplyr::rename(`Accident Date` = `Accidet Date`) %>% 
  dplyr::select(accident_vars)

accidents_2017 <- read_excel(file.path(raw_data_file_path,"etre","Research Data","AAE Crush Research Data","2017 AAE Crush  Research Data .xlsx")) %>%
  dplyr::select(accident_vars)

accidents_all <- bind_rows(accidents_2015, accidents_2016, accidents_2017)

accidents_all$`Light Injury` <- as.numeric(accidents_all$`Light Injury`)
accidents_all$`Serious Injury` <- as.numeric(accidents_all$`Serious Injury`)
accidents_all$Fatality <- as.numeric(accidents_all$Fatality)

table(!is.na(accidents_all$Fatality) | !is.na(accidents_all$`Serious Injury`) | !is.na(accidents_all$`Light Injury`))

# Load Road Data and Convert to Points -----------------------------------------
roads_2016 <- readOGR(dsn=file.path(raw_data_file_path,"RoadNetworkPanelDataV3_1996_2016_Revised"), "All_Network_2016")

# Create Shapefile of Points Every 10 Meters Along Road
equal_distant_projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
addis_adama_express <- roads_2016[roads_2016$LINKNAME %in% "Addis - Adama (Toll Road)",]
addis_adama_express <- spTransform(addis_adama_express, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
addis_adama_express <- spTransform(addis_adama_express, CRS(equal_distant_projection)) 

num_points <- gLength(addis_adama_express) / 10
addis_adama_points <- spsample(addis_adama_express, n = num_points, type = "regular")
addis_adama_points <- spTransform(addis_adama_points, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
addis_adama_points <- as.data.frame(addis_adama_points)
names(addis_adama_points) <- c("longitude","latitude")

addis_adama_points$distance_adama_direction <- (1:nrow(addis_adama_points))*10

# Determine Distance Along Roads of Accidents ----------------------------------
accidentLocationToDistance <- function(loc){
  loc <- strsplit(loc, "\\+")[[1]]
  loc <- as.numeric(loc)
  if(is.na(loc[2])){
    loc[2] <- 0
  }
  loc.dist <- loc[1]*1000 + loc[2]
  return(loc.dist)
}

# Calculate distances
accidents_all$accident_loc_dist <- lapply(accidents_all$`Accident Location`, accidentLocationToDistance) %>% unlist

# Remove observations not in direction of Addis or Adama & remove observations
# with NA distance
accidents_all <- accidents_all %>% 
  subset(grepl("(Addis|Adama)", Direction)) %>%
  drop_na(accident_loc_dist)

# Convert All Distances to Adama Direction; DONT DO THIS. Locations are not dependent on direction
#accidents_all$accident_loc_dist[grepl("Addis", accidents_all$Direction)] <- 76840 - accidents_all$accident_loc_dist[grepl("Addis", accidents_all$Direction)]

# Merge lat/lon to accident dataset
accidents_all <- merge(accidents_all, addis_adama_points, by.x="accident_loc_dist", by.y="distance_adama_direction")

# Generate/Fix Variables
accidents_all$Fatality <- accidents_all$Fatality %>% as.numeric
accidents_all$`Light Injury` <- accidents_all$`Light Injury` %>% as.numeric
accidents_all$`Serious Injury` <- accidents_all$`Serious Injury` %>% as.numeric
accidents_all$Direction[grepl("Addis",accidents_all$Direction)] <- "Addis"
accidents_all$Direction[grepl("Adama",accidents_all$Direction)] <- "Adama"

# Accident Time & Day ----------------------------------------------------------
# NOTE: Number of assumptions made about DAY and NIGHT
# NOTE: 12am=Midnight; 12pm=noon

accidents_all <- accidents_all %>%
  mutate(`Time of Accident` = `Time of Accident` %>% 
           str_replace_all("፡", ":")) %>%
  
  ### Accident Time (0-12)
  mutate(acc_time_num = `Time of Accident`) %>%
  mutate(acc_time_num = acc_time_num %>% 
           gsub(pattern = "[a-z A-Z]", replacement="")) %>%
  mutate(temp=acc_time_num) %>%
  separate(acc_time_num, into=c("hour","minute")) %>%
  mutate(minute = replace(minute, (hour != "" & minute == ""), "00")) %>%
  mutate(hour = hour %>% as.numeric) %>%
  mutate(minute = minute %>% as.numeric) %>%
  
  ### Accident Time of Day (am/pm)
  mutate(acc_time_ampm = NA) %>%
  mutate(acc_time_ampm = replace(acc_time_ampm, grepl("Morning|am|AM|Am", `Time of Accident`), "am")) %>%
  mutate(acc_time_ampm = replace(acc_time_ampm, grepl("pm|PM|Pm", `Time of Accident`), "pm")) %>%
  
  # Evening Assumptions (12=am)
  mutate(acc_time_ampm = replace(acc_time_ampm, (grepl("Evening", `Time of Accident`) & hour %in% c(1:11)),"pm")) %>%
  mutate(acc_time_ampm = replace(acc_time_ampm, (grepl("Evening", `Time of Accident`) & hour %in% c(12)),"am")) %>%
  
  # Night Time Assumptions (12 - 3 "Night" tag as "am", very early morning hours)
  mutate(acc_time_ampm = replace(acc_time_ampm, (grepl("Night|night|Nitght", `Time of Accident`) & hour %in% c(4,5,6,7,8,9,10,11)),"pm")) %>%
  mutate(acc_time_ampm = replace(acc_time_ampm, (grepl("Night|night|Nitght", `Time of Accident`) & hour %in% c(12,1,2,3)),"am")) %>%
  
  # Day Time Assumptions (daytime=6am to 6pm); am=7,8,9,10,11; pm=12,1,2,3,4,5,6
  mutate(acc_time_ampm = replace(acc_time_ampm, (grepl("Day", `Time of Accident`) & hour %in% c(7,8,9,10,11)),"am")) %>%
  mutate(acc_time_ampm = replace(acc_time_ampm, (grepl("Day", `Time of Accident`) & hour %in% c(12,1,2,3,4,5,6)),"pm")) %>%
  
  ### Convert Time to 24 Hours
  mutate(hour = replace(hour, is.na(acc_time_ampm), NA)) %>%
  mutate(minute = replace(minute, is.na(acc_time_ampm), NA)) %>%
  mutate(hour = replace(hour, (acc_time_ampm %in% "pm"), hour[acc_time_ampm %in% "pm"]+12)) %>%
  mutate(time = hour + minute / 60) 

# Accident Categories ----------------------------------------------------------
accidents_all$accident_cause_simple <- accidents_all$`Cause of Accident` 
Encoding(accidents_all$accident_cause_simple) <- "latin1"
accidents_all$accident_cause_simple <- tolower(accidents_all$accident_cause_simple)
accidents_all$accident_cause_simple <- trimws(accidents_all$accident_cause_simple)

accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("other",
                                                                               "front & back",
                                                                               "water problem",
                                                                               "pedestrian",
                                                                               "rainy",
                                                                               "wire problem",
                                                                               "over load")] <- "Other"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("animal crossing", 
                                                                               "animals crossing")] <- "Animal Crossing"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("brake problem",
                                                                               "brake  problem",
                                                                               "brake malfunction",
                                                                               "brake problem (dx)",
                                                                               "braker problem",
                                                                               "breaker problem",
                                                                               "brecker problem",
                                                                               "loose brake")] <- "Brake Problem"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("crush", 
                                                                               "crush by vehicle",
                                                                               "crush vehicle to vehicle",
                                                                               "crushed with other vehicle",
                                                                               "vehicles crush",
                                                                               "back crush")] <- "Vehicle Crush"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("fatigue",
                                                                               "sleep",
                                                                               "sleeping",
                                                                               "sleeping problem",
                                                                               "sleeping problem (isuzu)")] <- "Fatigue"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("fiat tire",
                                                                               "flat tire",
                                                                               "tire flat",
                                                                               "tire problem",
                                                                               "tire problem (abdulla)",
                                                                               "tyre problem",
                                                                               "tyre prblem",
                                                                               "tyre prblem (lifane)")] <- "Flat Tire/Tire Problem"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("speed",
                                                                               "over speed",
                                                                               "over speeding",
                                                                               "overspeeding",
                                                                               "overspeed",
                                                                               "over speed pickup")] <- "Speeding"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("steering wheel malfunction",
                                                                               "steering wheel problem",
                                                                               "streeing wheel problem",
                                                                               "steering wheel maltunction")] <- "Steering Wheel Malfunction"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("techinical malfunction",
                                                                               "technical malfunction",
                                                                               "technical prblem",
                                                                               "tehical problem",
                                                                               "technical problem",
                                                                               "technical problem",
                                                                               "transmition problem")] <- "Technical Malfunction"
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("unethical driving")] <- "Unethical Driving"

sum_temp <- accidents_all$accident_cause_simple %>% table %>% as.data.frame %>% dplyr::rename(var=".")
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% as.character(sum_temp$var[sum_temp$Freq %in% 1])] <- "Other"

accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% c("-")] <- NA
accidents_all$accident_cause_simple[grepl("?<??|?s?????", accidents_all$accident_cause_simple)] <- NA
accidents_all$accident_cause_simple[is.na(accidents_all$accident_cause_simple)] <- "Not Specified"
Encoding(accidents_all$accident_cause_simple) <- "UTF-8"

# Type of Accident Categories --------------------------------------------------
accidents_all$accident_type_simple <- accidents_all$`Type of accident`
Encoding(accidents_all$accident_type_simple) <- "latin1"
accidents_all$accident_type_simple <- tolower(accidents_all$accident_type_simple)
accidents_all$accident_type_simple<- trimws(accidents_all$accident_type_simple)

accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("-","?^~?s.?^??^???????????")] <- NA
accidents_all$accident_type_simple[grepl("?^~",accidents_all$accident_type_simple)] <- NA
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("car overthrow",
                                                                             "overthrow",
                                                                             "car overtherow",
                                                                             "car overt herow")] <- "Overthrow"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("crush with guardrail",
                                                                             "car crushed with guardrail",
                                                                             "crush with guard rail",
                                                                             "crush with guardrail",
                                                                             "cross median guardrail",
                                                                             "crush with guardrail፡",
                                                                             "crush with guardrail",
                                                                             "crush guardrail")] <- "Crush with Gaurdrail"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("crush vehicle tovehicle",
                                                                             "cehicles collision",
                                                                             "crush each other",
                                                                             "crush vehicle to vehicle",
                                                                             "crush vehiclewith vehicle",
                                                                             "crush with another car",
                                                                             "vehicles crush",
                                                                             "vehicles collision",
                                                                             "vehicles  crush",
                                                                             "Head on Crush",
                                                                             "front rear crush",
                                                                             "truck car crush him")] <- "Vehicle to Vehicle Crush"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("crushed with poperty")] <- "Crushed with Property"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("crush")] <- "Crush"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("front back crushed")] <- "Front Back Crushed"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("crush with concrete",
                                                                             "crush with crain",
                                                                             "crush with galvanized",
                                                                             "crush with parked vehicle",
                                                                             "crushing",
                                                                             "dolfine crush pickup",
                                                                             "isuzu crush others",
                                                                             "the vitz crush the isuzu",
                                                                             "sino crush isuzu",
                                                                             "sidecrush",
                                                                             "side crush",
                                                                             "head on crush",
                                                                             "head on crush?s?",
                                                                             " head on crush?s?",
                                                                             "crush with ditch",
                                                                             "crush with side dicth",
                                                                             "out of road",
                                                                             "near to near crushed")] <- "Crush, Other"
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% c("front & back","direction","direction change","flat tire","other")] <- "Other"

sum_temp <- accidents_all$accident_type_simple %>% table %>% as.data.frame %>% dplyr::rename(var=".")
accidents_all$accident_type_simple[accidents_all$accident_type_simple %in% as.character(sum_temp$var[sum_temp$Freq %in% 1])] <- "Other"

accidents_all$accident_type_simple[is.na(accidents_all$accident_type_simple)] <- "Not Specified"

# Vehicle Classification -------------------------------------------------------
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 1] <- "Small automobiles: Cars, Jeep, L/Rover, Taxi, Pick Up"
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 2] <- "Minibus"
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 3] <- "Medium Bus, ISUZU"
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 4] <- "Big Size Bus, Medium Truck"
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 5] <- "Heavy Truck Trailers [4 axel]"
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 6] <- "Heavy Truck Trailers [5 axel]"
accidents_all$vehicle_description[accidents_all$`Vehicle Type (V)` %in% 7] <- "Heavy Truck Trailers [>5 axel]"

accidents_all$axle_number[accidents_all$`Vehicle Type (V)` %in% 1:3] <- "2"
accidents_all$axle_number[accidents_all$`Vehicle Type (V)` %in% 4] <- "3"
accidents_all$axle_number[accidents_all$`Vehicle Type (V)` %in% 5] <- "4"
accidents_all$axle_number[accidents_all$`Vehicle Type (V)` %in% 6] <- "5"
accidents_all$axle_number[accidents_all$`Vehicle Type (V)` %in% 7] <- ">6"

# Accident Cause: Vehicle or Human ---------------------------------------------
accidents_all$accident_cause_vehicle_human <- "Other"
accidents_all$accident_cause_vehicle_human[accidents_all$accident_cause_simple %in% c("Brake Problem",
                                                                                      "Vehicle Crush",
                                                                                      "Flat Tire/Tire Problem",
                                                                                      "Steering Wheel Malfunction",
                                                                                      "Technical Malfunction")] <- "Vehicle Error"
accidents_all$accident_cause_vehicle_human[accidents_all$accident_cause_simple %in% c("Fatigue",
                                                                                      "Speeding",
                                                                                      "Unethical Driving")] <- "Human Error"

# Export Data ------------------------------------------------------------------
save(accidents_all, file = file.path(intermediate_data_file_path, "accident_data.Rda"))
save(addis_adama_points, file = file.path(intermediate_data_file_path, "addis_adama_points.Rda"))

