# Ethiopia Road Safety: Prepare Accident Data: Time and Segments

# Load Data --------------------------------------------------------------------
crashes_df <- readRDS(file.path(etre_crashes_dir, "FinalData", "crashes.Rds"))

load(file.path(intermediate_data_file_path, "traffic_data_limited.Rda"))
load(file.path(intermediate_data_file_path, "addis_adama_points.Rda"))

load(file.path(intermediate_data_file_path, "traffic_accidents_daily_long.Rda"))

# Prep Road File to Merge Into -------------------------------------------------
addis_adama_points$distance_adama_direction_1km <- round(addis_adama_points$distance_adama_direction/1000)
addis_adama_points_1km <- summaryBy(latitude+longitude ~ distance_adama_direction_1km, data=addis_adama_points, keep.names=T, FUN=mean)

dayshours <- lapply(unique(dataset_days_long$day), function(day) paste(day, c(paste0("0",0:9), 10:23))) %>% unlist

repeat_road_dataframe_across_days <- function(dayhour, df){
  df$dayhour <- dayhour
  return(df)
}

addis_adama_points_1km_dayhours <- lapply(dayshours, repeat_road_dataframe_across_days, addis_adama_points_1km) %>% bind_rows

addis_adama_points_1km_dayhours$km_dayhour <- paste(addis_adama_points_1km_dayhours$distance_adama_direction_1km, addis_adama_points_1km_dayhours$dayhour)

# Function ---------------------------------------------------------------------
#traffic_all_limited <- traffic_all_limited[1:50000,]
traffic_all_limited <- traffic_all_limited[traffic_all_limited$ENT_OccurTime != "1900-01-01 00:00:00.000",] # HOW OFTEN // WHICH YEARS??

traffic_all_limited_toaddis <- traffic_all_limited[traffic_all_limited$ENT_PlazaID > traffic_all_limited$Plaza_ID,]
traffic_all_limited_toadama <- traffic_all_limited[traffic_all_limited$ENT_PlazaID <= traffic_all_limited$Plaza_ID,]
rm(traffic_all_limited)

# Plaza IDs and Locations
# NOTE: 601 should really be 64; however, plaza is a few km before expressway ends.
entrance_df <- cbind(c(0,101,201,301,401,501,503,601),
                     c(2,2,  16, 33, 52, 60, 60, 64)) %>% as.data.frame
names(entrance_df) <- c("plaza","km")

exit_df <- cbind(c(102, 202, 302, 402, 502, 504, 602),
                 c(2,16,33,52,60,60,64)) %>% as.data.frame
names(exit_df) <- c("plaza","km")

# Function
determine_traffic_segments_time <- function(i, traffic_all_limited){
  
  tryCatch({
    
    traffic_all_limited_i <- traffic_all_limited[i,]
    
    day <- substring(traffic_all_limited_i$ENT_OccurTime,1,10)
    
    #####  Determine km markers traveled 
    ent_plaza <- traffic_all_limited_i$ENT_PlazaID
    exit_plaza <- traffic_all_limited_i$Plaza_ID
    
    ent_km <- entrance_df$km[entrance_df$plaza == ent_plaza]
    exit_km <- exit_df$km[exit_df$plaza == exit_plaza]
    
    kms_traveled <- ent_km:exit_km
    
    ##### Determine Time
    if(as.numeric(substring(traffic_all_limited_i$TransOccurTime,9,10)) <= 12){
      # Sometimes month/day flipped; try alternate version of exit time.
      traffic_all_limited_i$TransOccurTime_v2 <- paste0(substring(traffic_all_limited_i$TransOccurTime,1,4),"-",
                                                        substring(traffic_all_limited_i$TransOccurTime,9,10),"-",
                                                        substring(traffic_all_limited_i$TransOccurTime,6,7)," ",
                                                        substring(traffic_all_limited_i$TransOccurTime,12,50))
      
      
      time_enter <- as.POSIXct(traffic_all_limited_i$ENT_OccurTime)
      time_exit_v1 <- as.POSIXct(traffic_all_limited_i$TransOccurTime)
      time_exit_v2 <- as.POSIXct(traffic_all_limited_i$TransOccurTime_v2)
      
      time_on_road_v1 <- abs(difftime(time_exit_v1 , time_enter, units = "mins"))
      time_on_road_v2 <- abs(difftime(time_exit_v2 , time_enter, units = "mins"))
      
      if(time_on_road_v1 > time_on_road_v2){
        time_exit <- as.POSIXct(traffic_all_limited_i$TransOccurTime_v2)
      } else{
        time_exit <- as.POSIXct(traffic_all_limited_i$TransOccurTime)
      }
      
    } else{
      time_enter <- as.POSIXct(traffic_all_limited_i$ENT_OccurTime)
      time_exit <- as.POSIXct(traffic_all_limited_i$TransOccurTime)
    }
    
    # Determine Hours
    hour_begin <- substring(time_enter,12,13) %>% as.numeric
    hour_end <- substring(time_exit,12,13) %>% as.numeric
    
    minute_begin <- substring(time_enter,15,16) %>% as.numeric
    minute_end <- substring(time_exit,15,16) %>% as.numeric
    
    # If pass through 1 hour
    if(length(hour_begin:hour_end) == 1){
      kms_dayhours_df <- cbind(kms_traveled,
                               rep(hour_begin, length(kms_traveled))) %>% 
        as.data.frame
    }
    
    # If pass through 2 hours
    if(length(hour_begin:hour_end) == 2){
      
      minutes_in_hour_begin <- 60-minute_begin
      minutes_in_hour_end <- minute_end
      
      proportion_hour_begin <- minutes_in_hour_begin/(minutes_in_hour_begin+minutes_in_hour_end)
      proportion_hour_end <- minutes_in_hour_end/(minutes_in_hour_begin+minutes_in_hour_end)
      
      number_kms_hour_begin <- round(length(kms_traveled)*proportion_hour_begin)
      number_kms_hour_end <- round(length(kms_traveled)*proportion_hour_end)
      
      hours_in_kms <- c(rep(hour_begin, number_kms_hour_begin), rep(hour_end, number_kms_hour_end))
      
      while(length(kms_traveled) != length(hours_in_kms)){
        if(length(kms_traveled) < length(hours_in_kms)) hours_in_kms <- hours_in_kms[-1]
        if(length(kms_traveled) > length(hours_in_kms)) hours_in_kms <- c(hours_in_kms[1],hours_in_kms)
      }
      
      kms_dayhours_df <- cbind(kms_traveled, hours_in_kms) %>% as.data.frame
    }
    
    # If pass through 3 or more hours
    if(length(hour_begin:hour_end) >= 3){
      minutes_in_hour_begin <- 60-minute_begin
      minutes_in_hour_end <- minute_end
      
      in_between_hours <- (hour_begin+1):(hour_end-1)
      
      num_hours_inbetween <- length(in_between_hours)
      
      proportion_hour_begin <- minutes_in_hour_begin/(minutes_in_hour_begin+minutes_in_hour_end+num_hours_inbetween*60)
      proportion_hour_end <- minutes_in_hour_end/(minutes_in_hour_begin+minutes_in_hour_end+num_hours_inbetween*60)
      
      number_kms_hour_begin <- ceiling(length(kms_traveled)*proportion_hour_begin)
      number_kms_hour_end <- ceiling(length(kms_traveled)*proportion_hour_end)
      num_kms_inbetween_hours <- length(kms_traveled)-number_kms_hour_begin-number_kms_hour_end
      
      num_kms_per_inbetween_hour <- round(num_kms_inbetween_hours/num_hours_inbetween)
      
      in_between_hours_vector <- rep(in_between_hours, num_kms_per_inbetween_hour) %>% sort
      
      while(length(in_between_hours_vector) != num_kms_inbetween_hours){
        if(length(in_between_hours_vector) > num_kms_inbetween_hours) in_between_hours_vector <- in_between_hours_vector[-1]
        if(length(in_between_hours_vector) < num_kms_inbetween_hours) in_between_hours_vector <- c(in_between_hours_vector[1],in_between_hours_vector)
      }
      
      hours_in_kms <- c(rep(hour_begin, number_kms_hour_begin), 
                        in_between_hours_vector, 
                        rep(hour_end, number_kms_hour_end))
      
      kms_dayhours_df <- cbind(kms_traveled, hours_in_kms) %>% as.data.frame
    }
    
    # Prep df
    names(kms_dayhours_df) <- c("km","dayhour")
    kms_dayhours_df$dayhour <- paste(day, kms_dayhours_df$dayhour)
    kms_dayhours_df$km_dayhour <- paste(kms_dayhours_df$km, kms_dayhours_df$dayhour)
    
    out <- kms_dayhours_df$km_dayhour
    
    
    #out_id <- addis_adama_points_1km_dayhours$id[addis_adama_points_1km_dayhours$km_dayhour %in% kms_dayhours_df$km_dayhour]
    
    return(out)
    
  }, error=function(e) return(NA))
}

# Implement Function: Determine Traffic -------------------------------------------------------------
for(i in 1:5) gc() # garbage collection

##### Traffic to Addis
traffic_to_addis <- pbmclapply(1:nrow(traffic_all_limited_toaddis), determine_traffic_segments_time, traffic_all_limited_toaddis, mc.cores=16, max.vector.size = 22020096000) %>% unlist
traffic_to_addis_freq <- table(traffic_to_addis) %>% as.data.frame
names(traffic_to_addis_freq) <- c("km_dayhour","traffic_toaddis")
addis_adama_points_1km_dayhours_trafficaddis <- merge(addis_adama_points_1km_dayhours, traffic_to_addis_freq, by="km_dayhour",all.x=T)
save(addis_adama_points_1km_dayhours_trafficaddis, file = file.path(intermediate_data_file_path, "addis_adama_points_1km_dayhours_trafficaddis.Rda"))

# Garbage Collection
rm(addis_adama_points_1km_dayhours_trafficaddis, traffic_to_addis, traffic_to_addis_freq)
for(i in 1:5) gc()

##### Traffic to Adama
traffic_to_adama <- pbmclapply(1:nrow(traffic_all_limited_toadama), determine_traffic_segments_time, traffic_all_limited_toadama, mc.cores=16, max.vector.size = 22020096000) %>% unlist
traffic_to_adama_freq <- table(traffic_to_adama) %>% as.data.frame
names(traffic_to_adama_freq) <- c("km_dayhour","traffic_toadama")
addis_adama_points_1km_dayhours_trafficadama <- merge(addis_adama_points_1km_dayhours, traffic_to_adama_freq, by="km_dayhour",all.x=T)
save(addis_adama_points_1km_dayhours_trafficadama, file = file.path(intermediate_data_file_path, "addis_adama_points_1km_dayhours_trafficadama.Rda"))

# Garbage Collection
rm(addis_adama_points_1km_dayhours_trafficadama, traffic_to_adama, traffic_to_adama_freq)
for(i in 1:5) gc()

end_time <- Sys.time()
end_time - start_time



