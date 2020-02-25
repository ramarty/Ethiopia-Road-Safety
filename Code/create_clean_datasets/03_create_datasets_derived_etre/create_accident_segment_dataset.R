# Ethiopia Road Safety: Prepare Accident Data: Time and Segments

# Run master to setup ----------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

# Load Data --------------------------------------------------------------------
load(file.path(intermediate_data_file_path, "accident_data.Rda"))
load(file.path(intermediate_data_file_path, "addis_adama_points.Rda"))
load(file.path(intermediate_data_file_path, "traffic_data_limited.Rda"))

# Number of Accidents Along Road -----------------------------------------------
determine_accidents_along_road <- function(addis_adama_points, accidents_all, acc_type){
  accidents_all$N_accidents_total <- 1
  accidents_all$N_accidents_AddisDirection <- (accidents_all$Direction == "Addis") %>% as.numeric
  accidents_all$N_accidents_AdamaDirection <- (accidents_all$Direction == "Adama") %>% as.numeric
  
  accidents_all_sum_locations <- summaryBy(N_accidents_total +
                                    N_accidents_AddisDirection +
                                    N_accidents_AdamaDirection ~ 
                                    accident_loc_dist, 
                                    data=accidents_all, keep.names=T, FUN=sum)
  names(accidents_all_sum_locations)[2:4] <- paste0(names(accidents_all_sum_locations)[2:4],acc_type)
  
  addis_adama_points <- merge(addis_adama_points, accidents_all_sum_locations, 
                                by.x="distance_adama_direction", by.y="accident_loc_dist",all.x=T)
  return(addis_adama_points)
}

addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all, 
                                                     "_all")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_cause_simple %in% "Unethical Driving",], 
                                                     "_unethicaldriving")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_cause_simple %in% "Speeding",], 
                                                     "_speeding")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_type_simple %in% "Crush with Gaurdrail",], 
                                                     "_crush_gaurdrail")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_type_simple %in% "Crushed with Property",], 
                                                     "_crush_property")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_type_simple %in% "Overthrow",], 
                                                     "_overthrow")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_type_simple %in% "Crush",], 
                                                     "_crush")
addis_adama_points <- determine_accidents_along_road(addis_adama_points, 
                                                     accidents_all[accidents_all$accident_type_simple %in% "Vehicle to Vehicle Crush",], 
                                                     "_vehicle_crush")

for(var in names(addis_adama_points)[grepl("N_accidents_", names(addis_adama_points))]){
  addis_adama_points[[var]][is.na(addis_adama_points[[var]])] <- 0
}

# Segment Traffic: Total Number of Cars Passed Through Segment -----------------
determine_traffic_along_road <- function(traffic_all_limited, addis_adama_points, year){
  
  year <- year %>% as.character
  
  traffic_all_limited <- traffic_all_limited[substring(traffic_all_limited$TransOccurTime,1,4) %in% year,]

  traffic_all_limited <- traffic_all_limited[traffic_all_limited$entrance_km != traffic_all_limited$exit_km,]
  traffic_all_limited$direction_to_adama <- traffic_all_limited$ENT_PlazaID < traffic_all_limited$Plaza_ID
  
  traffic_all_limited$entrance_km[traffic_all_limited$entrance_km == 2] <- 0
  traffic_all_limited$exit_km[traffic_all_limited$exit_km == 2] <- 0
  
  traffic_all_limited$entrance_km[traffic_all_limited$entrance_km == 64] <- 77
  traffic_all_limited$exit_km[traffic_all_limited$exit_km == 64] <- 77
  
  traffic_all_limited$N <- 1
  traffic_all_limited_sum_to_adama <- summaryBy(N ~ entrance_km + exit_km, data=traffic_all_limited[traffic_all_limited$direction_to_adama == TRUE,], FUN=sum, keep.names=T)
  traffic_all_limited_sum_to_addis <- summaryBy(N ~ entrance_km + exit_km, data=traffic_all_limited[traffic_all_limited$direction_to_adama == FALSE,], FUN=sum, keep.names=T)
  
  addis_adama_points$traffic_to_adama <- 0
  addis_adama_points$traffic_to_addis <- 0
  
  for(i in 1:nrow(traffic_all_limited_sum_to_adama)){
    traffic_all_limited_i <- traffic_all_limited_sum_to_adama[i,]
    kms_traveled <- (traffic_all_limited_i$entrance_km*100):(traffic_all_limited_i$exit_km*100)*10
    addis_adama_points$traffic_to_adama[addis_adama_points$distance_adama_direction %in% kms_traveled] <- addis_adama_points$traffic_to_adama[addis_adama_points$distance_adama_direction %in% kms_traveled] + traffic_all_limited_i$N
  }
  
  for(i in 1:nrow(traffic_all_limited_sum_to_addis)){
    traffic_all_limited_i <- traffic_all_limited_sum_to_addis[i,]
    kms_traveled <- (traffic_all_limited_i$entrance_km*100):(traffic_all_limited_i$exit_km*100)*10
    addis_adama_points$traffic_to_addis[addis_adama_points$distance_adama_direction %in% kms_traveled] <- addis_adama_points$traffic_to_addis[addis_adama_points$distance_adama_direction %in% kms_traveled] + traffic_all_limited_i$N
  }
  
  addis_adama_points$traffic <- addis_adama_points$traffic_to_addis + addis_adama_points$traffic_to_adama
  
  addis_adama_points <- subset(addis_adama_points, select=c(traffic_to_adama, traffic_to_addis, traffic))
  names(addis_adama_points) <- paste0(names(addis_adama_points),"_",year) 
  
  return(addis_adama_points)
}

addis_adama_points_traffic_2015 <- determine_traffic_along_road(traffic_all_limited, addis_adama_points, 2015)
addis_adama_points_traffic_2016 <- determine_traffic_along_road(traffic_all_limited, addis_adama_points, 2016)
addis_adama_points_traffic_2017 <- determine_traffic_along_road(traffic_all_limited, addis_adama_points, 2017)

addis_adama_points <- cbind(addis_adama_points, addis_adama_points_traffic_2015)
addis_adama_points <- cbind(addis_adama_points, addis_adama_points_traffic_2016)
addis_adama_points <- cbind(addis_adama_points, addis_adama_points_traffic_2017)

# Distance to Entrance/Exit Plazas ---------------------------------------------
addis_adama_points$dist_plaza_k2 <- abs(addis_adama_points$distance_adama_direction - 2*1000)
addis_adama_points$dist_plaza_k16 <- abs(addis_adama_points$distance_adama_direction - 16*1000)
addis_adama_points$dist_plaza_k33 <- abs(addis_adama_points$distance_adama_direction - 33*1000)
addis_adama_points$dist_plaza_k52 <- abs(addis_adama_points$distance_adama_direction - 52*1000)
addis_adama_points$dist_plaza_k60 <- abs(addis_adama_points$distance_adama_direction - 60*1000)
addis_adama_points$dist_plaza_k64 <- abs(addis_adama_points$distance_adama_direction - 64*1000)

addis_adama_points$dist_plaza_nearest_m <- with(addis_adama_points, pmin(dist_plaza_k2, dist_plaza_k16, dist_plaza_k33,
                              dist_plaza_k52, dist_plaza_k60, dist_plaza_k64))
addis_adama_points$dist_plaza_nearest_km <- addis_adama_points$dist_plaza_nearest_m/1000

# Determine Turning Angle ------------------------------------------------------
# https://www.r-bloggers.com/calculate-turning-angles-and-step-lengths-from-location-data/
bearing.ta <- function(loc1,loc2,loc3,as.deg=FALSE){
  ## calculates the bearing and length of the two lines
  ##    formed by three points
  ## the turning angle from the first bearing to the
  ##    second bearing is also calculated
  ## locations are assumed to be in (X,Y) format.
  ## Options:
  ## as.deg = TRUE returns degrees instead of radians
  if (length(loc1) != 2 | length(loc2) != 2 | length(loc3) !=2){
    print("Locations must consist of either three vectors, length == 2,
          or three two-column dataframes")
    return(NaN)
  }
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  locdiff1<-loc2-loc1
  locdiff2<-loc3-loc2
  bearing1<-anglefun(locdiff1[1],locdiff1[2],bearing=F)
  bearing2<-anglefun(locdiff2[1],locdiff2[2],bearing=F)
  
  if(is.data.frame(locdiff1)){
    dist1<-sqrt(rowSums(locdiff1^2))
    dist2<-sqrt(rowSums(locdiff2^2))
  }else{
    dist1<-sqrt(sum(locdiff1^2))
    dist2<-sqrt(sum(locdiff2^2))
  }
  
  ta=(bearing2-bearing1)
  
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi
  return(list(bearing1=unlist(bearing1*c),bearing2=unlist(bearing2*c),
              ta=unlist(ta*c),dist1=unlist(dist1),dist2=unlist(dist2)))
}

anglefun <- function(xx,yy,bearing=TRUE,as.deg=FALSE){
  ## calculates the compass bearing of the line between two points
  ## xx and yy are the differences in x and y coordinates between two points
  ## Options:
  ## bearing = FALSE returns +/- pi instead of 0:2*pi
  ## as.deg = TRUE returns degrees instead of radians
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  b<-sign(xx)
  b[b==0]<-1  #corrects for the fact that sign(0) == 0
  tempangle = b*(yy<0)*pi+atan(xx/yy)
  if(bearing){
    #return a compass bearing 0 to 2pi
    #if bearing==FALSE then a heading (+/- pi) is returned
    tempangle[tempangle<0]<-tempangle[tempangle<0]+2*pi
  }
  return(tempangle*c)
}


determine_turn_angle <- function(i, turn_angle_window){

  indices <- c((i-turn_angle_window),i,(i+turn_angle_window))
  if((TRUE %in% (indices < 0)) | (TRUE %in% (indices > nrow(addis_adama_points)))){
    ta <- NA
  }else{
    addis_adama_points_i <- addis_adama_points[c((i-turn_angle_window),i,(i+turn_angle_window)),]
    
    A <- c(addis_adama_points_i$latitude[1], addis_adama_points_i$longitude[1])
    O <- c(addis_adama_points_i$latitude[2], addis_adama_points_i$longitude[2])
    B <- c(addis_adama_points_i$latitude[3], addis_adama_points_i$longitude[3])
    
    ta <- abs(bearing.ta(A,O,B,as.deg=T)$ta)
  }
  
  return(ta)
}

addis_adama_points$turnangle_1m <- lapply(1:nrow(addis_adama_points), determine_turn_angle, 1) %>% unlist
addis_adama_points$turnangle_10m <- lapply(1:nrow(addis_adama_points), determine_turn_angle, 10) %>% unlist
addis_adama_points$turnangle_50m <- lapply(1:nrow(addis_adama_points), determine_turn_angle, 50) %>% unlist
addis_adama_points$turnangle_100m <- lapply(1:nrow(addis_adama_points), determine_turn_angle, 100) %>% unlist
addis_adama_points$turnangle_500m <- lapply(1:nrow(addis_adama_points), determine_turn_angle, 500) %>% unlist

# Moving Average of Variables --------------------------------------------------
moving_window_accidents <- function(i, window){
  addis_adama_points_i <- addis_adama_points[i,]
  addis_adama_points_window <- addis_adama_points[addis_adama_points$distance_adama_direction %in% (addis_adama_points_i$distance_adama_direction-window):(addis_adama_points_i$distance_adama_direction+window),]
  
  addis_adama_points_window_accident_sums <- addis_adama_points_window[,names(addis_adama_points_window)[grepl("N_accidents_", names(addis_adama_points_window))]]
  addis_adama_points_window_accident_sums <- addis_adama_points_window_accident_sums[,names(addis_adama_points_window_accident_sums)[!grepl("\\bMA", names(addis_adama_points_window_accident_sums))]]
  
  accident_sums <- colSums(addis_adama_points_window_accident_sums)
  
  avg_turnangle_1m <- mean(addis_adama_points_window$turnangle_1m, na.rm=T)
  avg_turnangle_10m <- mean(addis_adama_points_window$turnangle_10m, na.rm=T)
  avg_turnangle_50m <- mean(addis_adama_points_window$turnangle_50m, na.rm=T)
  avg_turnangle_100m <- mean(addis_adama_points_window$turnangle_100m, na.rm=T)
  avg_turnangle_500m <- mean(addis_adama_points_window$turnangle_500m, na.rm=T)
  
  df_out <- cbind(t(as.numeric(accident_sums)),
                  avg_turnangle_1m,
                  avg_turnangle_10m,
                  avg_turnangle_50m,
                  avg_turnangle_100m,
                  avg_turnangle_500m) %>%
    as.data.frame
  
  names(df_out) <- c(names(accident_sums),
                      "turnagle_1m",
                      "turnagle_10m",
                      "turnagle_50m",
                      "turnagle_100m",
                      "turnagle_500m")
  
  names(df_out) <- paste0("MA",window*2,"_",names(df_out))
  
  return(df_out)
}

addis_adama_points <- cbind(addis_adama_points,
                            lapply(1:nrow(addis_adama_points), moving_window_accidents, 500) %>% bind_rows)
addis_adama_points <- cbind(addis_adama_points,
                            lapply(1:nrow(addis_adama_points), moving_window_accidents, 1000) %>% bind_rows)

# Export -----------------------------------------------------------------------
addis_adama_points_data <- addis_adama_points
save(addis_adama_points_data, file = file.path(intermediate_data_file_path, "addis_adama_points_data.Rda"))



