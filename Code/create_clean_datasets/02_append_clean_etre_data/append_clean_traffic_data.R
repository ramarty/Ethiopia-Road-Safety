# Ethiopia Road Safety: Prepare Traffic Data

# Run master to setup ----------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "r521633") source("/home/wb521633/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

# Load and Append Traffic Data -------------------------------------------------
traffic_vars <- c("MID", # transaction ID
                  "Plaza_ID", # Exit Toll Plaza ID
                  "TransOccurTime", # Transaction Time at Exit
                  "UpDown", #Up=0;Down=1 (direction of travel)
                  "CarLicense", # Hashed liscence plate
                  "VehType", # Vehicle Type 
                  "AxisNum", # Axle Number (Detected at Exit)
                  "Height", # Height (Detected at Exit)
                  "TotalWeight", # Total Weight (Detected at Exit)
                  "ENT_PlazaID", # Entrance Toll Plaza ID
                  "ENT_OccurTime", # Transaction Time at entrance
                  "Distance" # Distance traveled on AAE
                  )

traffic_2015 <- fread(file.path(raw_data_file_path, "etre","Research Data","AAE Traffic Research Data","2015 AAE Traffic Research Data.csv")) %>%
  select(traffic_vars)
traffic_2016 <- fread(file.path(raw_data_file_path, "etre","Research Data","AAE Traffic Research Data","2016 AAE Traffic Research Data.csv")) %>%
  select(traffic_vars)
traffic_2017 <- fread(file.path(raw_data_file_path, "etre","Research Data","AAE Traffic Research Data","2017 AAE Traffic Research Data.csv")) %>%
  select(traffic_vars)
traffic_2018 <- fread(file.path(raw_data_file_path, "etre","Research Data","AAE Traffic Research Data","2018 AAE Traffic Research Data.csv")) %>%
  select(traffic_vars)

traffic_2018$TransOccurTime %>% min()
traffic_2018$TransOccurTime %>% max()
nrow(traffic_2018)

traffic_all <- bind_rows(list(traffic_2015, traffic_2016, traffic_2017))

# Calculate Time on Road and Speed ---------------------------------------------
# Remove Bad Dates and Distances
traffic_all$ENT_OccurTime[substring(traffic_all$ENT_OccurTime, 1, 4) %in% c("1899","1900")] <- NA
traffic_all$Distance[traffic_all$Distance == 0] <- NA

# Time on Road
traffic_all$time_on_road <- difftime(traffic_all$TransOccurTime, traffic_all$ENT_OccurTime, units = "mins") %>% as.numeric
traffic_all$time_on_road <- traffic_all$time_on_road %>% as.numeric

# Remove Bad "Time on Road" Values [something wrong with how recorded dates]
traffic_all$time_on_road[traffic_all$time_on_road <= 0] <- NA # what timeframe is unreasonable?
traffic_all$time_on_road[traffic_all$time_on_road > 60*24] <- NA # what timeframe is unreasonable?

# Speed
traffic_all$speed_km_hr  <- (traffic_all$Distance / 1000) / (traffic_all$time_on_road / 60)

# Remove Bad Speeds
traffic_all$speed_km_hr[traffic_all$speed_km_hr >= 300] <- NA
traffic_all$speed_km_hr[traffic_all$speed_km_hr <= 5] <- NA

# Entrance and Exit km ---------------------------------------------------------
entrance_df <- cbind(c(0,101,201,301,401,501,503,601),
                     c(2,2,  16, 33, 52, 60, 60, 64)) %>% as.data.frame
names(entrance_df) <- c("plaza","entrance_km")

exit_df <- cbind(c(102, 202, 302, 402, 502, 504, 602),
                 c(2,16,33,52,60,60,64)) %>% as.data.frame
names(exit_df) <- c("plaza","exit_km")

traffic_all <- merge(traffic_all, entrance_df, by.x="ENT_PlazaID", by.y="plaza")
traffic_all <- merge(traffic_all, exit_df, by.x="Plaza_ID", by.y="plaza")

# Create Limited Dataframe -----------------------------------------------------
traffic_all_limited <- subset(traffic_all, select=c("Plaza_ID","ENT_PlazaID","TransOccurTime","ENT_OccurTime","speed_km_hr",
                                                    "entrance_km","exit_km"))

# Export Data ------------------------------------------------------------------
save(traffic_all, file = file.path(intermediate_data_file_path, "traffic_data.Rda"))
save(traffic_all_limited, file = file.path(intermediate_data_file_path, "traffic_data_limited.Rda"))


