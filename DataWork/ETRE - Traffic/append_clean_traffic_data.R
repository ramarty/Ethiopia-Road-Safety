# Ethiopia Road Safety: Prepare Traffic Data

# 1. Load/Append Data
# 2. Adjust variable names

# 1. Load/append data ----------------------------------------------------------
# Only load in relevant variables; do when initially load dataset to minimize 
# memory usage

# Load 2015 to grab names; use to give same names to 2014 data, which does not
# have variable names (has V1, V2, etc...)
traffic_2015 <- fread(file.path(etre_traffic_dir, "RawData", "2015 AAE Traffic Research Data.csv"))
traffic_2015_names <- names(traffic_2015)
rm(traffic_2015)

traffic_df <- file.path(etre_traffic_dir, "RawData") %>%
  list.files(full.names = T,
             pattern = ".csv") %>%
  map_df(function(path){
    
    df <- fread(path) 
    
    # 2014 doesn't have variable names. It has same number of columns as 2015,
    # so use those variable names
    if(grepl("2014 AAE Traffic Research Data.csv", path)){
      names(df) <- traffic_2015_names
    }
    
    df <- df %>%
      dplyr::select(Plaza_ID,
                    TransOccurTime,
                    CarLicense,
                    VehType,
                    UpDown,
                    TotalWeight,
                    Distance,
                    ENT_PlazaID,
                    ENT_OccurTime)
    
    return(df)
  })

# 2. Adjust variable names -----------------------------------------------------
traffic_df <- traffic_df %>%
  dplyr::rename(plaza_id = Plaza_ID,
                trans_occur_time = TransOccurTime,
                car_license = CarLicense,
                veh_type = VehType,
                up_down = UpDown,
                total_weight = TotalWeight,
                distance = Distance,
                ent_plaza_id = ENT_PlazaID,
                ent_occur_time = ENT_OccurTime)

# 2. Calculate Time on Road and Speed ------------------------------------------
# Remove Bad Dates and Distances
traffic_df$ent_occur_time[substring(traffic_df$ent_occur_time, 1, 4) %in% c("1899","1900")] <- NA
traffic_df$distance[traffic_df$distance == 0] <- NA

# Time on Road
traffic_df$time_on_road <- difftime(traffic_df$trans_occur_time, traffic_df$ent_occur_time, units = "mins") %>% as.numeric

# Remove Bad "Time on Road" Values [something wrong with how recorded dates]
traffic_df$time_on_road[traffic_df$time_on_road <= 0] <- NA # what timeframe is unreasonable?
traffic_df$time_on_road[traffic_df$time_on_road > 60*24] <- NA # what timeframe is unreasonable?

# Speed
traffic_df$speed_km_hr  <- (traffic_df$Distance / 1000) / (traffic_df$time_on_road / 60)

# Remove Bad Speeds
traffic_df$speed_km_hr[traffic_df$speed_km_hr >= 300] <- NA
traffic_df$speed_km_hr[traffic_df$speed_km_hr <= 5] <- NA

# 3. Add Entrance and Exit km (mapped from Plaza ID) ---------------------------
traffic_df$entrance_km <- NA
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 0]   <- 2
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 101] <- 2
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 201] <- 16
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 301] <- 33
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 401] <- 52
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 501] <- 60
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 503] <- 60
traffic_df$entrance_km[traffic_df$ent_plaza_id %in% 601] <- 64

traffic_df$exit_km <- NA
traffic_df$exit_km[traffic_df$plaza_id %in% 102] <- 2
traffic_df$exit_km[traffic_df$plaza_id %in% 202] <- 16
traffic_df$exit_km[traffic_df$plaza_id %in% 302] <- 33
traffic_df$exit_km[traffic_df$plaza_id %in% 402] <- 52
traffic_df$exit_km[traffic_df$plaza_id %in% 502] <- 60
traffic_df$exit_km[traffic_df$plaza_id %in% 504] <- 60
traffic_df$exit_km[traffic_df$plaza_id %in% 602] <- 64

# 4. Export Data ---------------------------------------------------------------
saveRDS(traffic_df, file = file.path(etre_traffic_dir, "FinalData", "traffic.Rds"))

# traffic_vars <- c("MID", # transaction ID
#                   "Plaza_ID", # Exit Toll Plaza ID
#                   "TransOccurTime", # Transaction Time at Exit
#                   "UpDown", #Up=0;Down=1 (direction of travel)
#                   "CarLicense", # Hashed liscence plate
#                   "VehType", # Vehicle Type 
#                   "AxisNum", # Axle Number (Detected at Exit)
#                   "Height", # Height (Detected at Exit)
#                   "TotalWeight", # Total Weight (Detected at Exit)
#                   "ENT_PlazaID", # Entrance Toll Plaza ID
#                   "ENT_OccurTime", # Transaction Time at entrance
#                   "Distance" # Distance traveled on AAE
# )
# 

