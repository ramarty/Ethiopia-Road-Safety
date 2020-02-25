# Ethiopia Road Safety: Merge Accident and Traffic Data into Daily and Hourly Datasets

# Run master to setup ----------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

# Load Data --------------------------------------------------------------------
load(file.path(intermediate_data_file_path, "accident_data.Rda"))
load(file.path(intermediate_data_file_path, "traffic_data.Rda"))
load(file.path(intermediate_data_file_path, "precipitation.Rda"))

# Functions --------------------------------------------------------------------
daily_summary_subset <- function(df, var, value, newvar, coll_type){
  df_subset <- df[df[[var]] %in% value,]
  df_subset$N <- 1
  if(coll_type == "sum") df_subset <- summaryBy(N ~ day, data=df_subset, FUN=sum, keep.names=T)
  if(coll_type == "mean") df_subset <- summaryBy(N ~ day, data=df_subset, FUN=mean, keep.names=T)
  df_subset <- df_subset[!is.na(df_subset$N),]
  names(df_subset) <- c("day", newvar)
  return(df_subset)
}

hourly_summary_subset <- function(df, var, value, newvar, coll_type){
  df_subset <- df[df[[var]] %in% value,]
  df_subset$N <- 1
  if(coll_type == "sum") df_subset <- summaryBy(N ~ dayhour, data=df_subset, FUN=sum, keep.names=T)
  if(coll_type == "mean") df_subset <- summaryBy(N ~ dayhour, data=df_subset, FUN=mean, keep.names=T)
  df_subset <- df_subset[!is.na(df_subset$N),]
  names(df_subset) <- c("dayhour", newvar)
  return(df_subset)
}

# Prep Datasets ----------------------------------------------------------------

# Traffic
traffic_all$day <- traffic_all$ENT_OccurTime %>% substring(1,10)
traffic_all$dayhour <- traffic_all$ENT_OccurTime %>% substring(1,13)
traffic_all <- traffic_all[traffic_all$day >= "2015-03-01",]
traffic_all$towardsAddis <- traffic_all$ENT_PlazaID > traffic_all$Plaza_ID

# Accidents
accidents_all$day <- accidents_all$`Accident Date` %>% substring(1,10)
accidents_all$hour <- (floor(accidents_all$time)-1) %>% as.character # traffic goes from 0-23
accidents_all$hour[as.numeric(accidents_all$hour) %in% 0:9] <- paste0("0", accidents_all$hour[as.numeric(accidents_all$hour) %in% 0:9])
accidents_all$dayhour <- paste(accidents_all$day, accidents_all$hour)
accidents_all$dayhour[is.na(accidents_all$time)] <- NA
accidents_all <- accidents_all[accidents_all$day >= "2015-03-01",]

# Create Hourly Dataset --------------------------------------------------------

# Traffic by Hour
traffic_all$traffic_N <- 1
traffic_county_hourly <- summaryBy(traffic_N ~ dayhour, data=traffic_all, FUN=sum, keep.names=T)
traffic_county_hourly <- traffic_county_hourly[!is.na(traffic_county_hourly$traffic_N),]

traffic_towards_addis_hourly <- hourly_summary_subset(traffic_all, "towardsAddis", 1, "traffic_towardsAddis", "sum")
traffic_towards_adama_hourly <- hourly_summary_subset(traffic_all, "towardsAddis", 0, "traffic_towardsAdama", "sum")

# Speed by Hour 
quantiles_use <- c(0.1, .25, .5, .75, .9)

speed_hourly <- summaryBy(speed_km_hr ~ dayhour, data=traffic_all, FUN = function(x) quantile(x, quantiles_use, na.rm=T))
names(speed_hourly) <- c("dayhour", paste0("speed_perc_",quantiles_use))

speed_towards_addis_hourly <- summaryBy(speed_km_hr ~ dayhour, data=traffic_all[traffic_all$towardsAddis == TRUE,], FUN = function(x) quantile(x, quantiles_use, na.rm=T))
names(speed_towards_addis_hourly) <- c("dayhour", paste0("speed_towards_addis_perc_",quantiles_use))

speed_towards_adama_hourly <- summaryBy(speed_km_hr ~ dayhour, data=traffic_all[traffic_all$towardsAddis == FALSE,], FUN = function(x) quantile(x, quantiles_use, na.rm=T))
names(speed_towards_adama_hourly) <- c("dayhour", paste0("speed_towards_adama_perc_",quantiles_use))

# Accidents by Hour 
accidents_all$accidents_N <- 1
accident_count_hourly <- summaryBy(accidents_N ~ dayhour, data=accidents_all, FUN=sum, keep.names=T)
accident_count_hourly <- accident_count_hourly[!is.na(accident_count_hourly$accidents_N),]

accident_towardsAddis_hourly <- hourly_summary_subset(accidents_all, "Direction","Addis", "accident_towardsAddis", "sum")
accident_towardsAdama_hourly <- hourly_summary_subset(accidents_all, "Direction","Adama", "accident_towardsAdama", "sum")

# Merge 
dataset_hours_wide <- merge(traffic_county_hourly, accident_count_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, traffic_towards_addis_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, traffic_towards_adama_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, speed_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, speed_towards_addis_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, speed_towards_adama_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, accident_towardsAddis_hourly, by="dayhour",all=T)
dataset_hours_wide <- merge(dataset_hours_wide, accident_towardsAdama_hourly, by="dayhour",all=T)

dataset_hours_wide <- dataset_hours_wide[!is.na(dataset_hours_wide$dayhour),]

# Create Daily Dataset --------------------------------------------------------
# NOTE: Can't just summarize hourly dataset; some accidents record the day but
#       not the time.

# Traffic by Hour
traffic_all$traffic_N <- 1
traffic_county_daily <- summaryBy(traffic_N ~ day, data=traffic_all, FUN=sum, keep.names=T)
traffic_county_daily <- traffic_county_daily[!is.na(traffic_county_daily$traffic_N),]

traffic_towards_addis_daily <- daily_summary_subset(traffic_all, "towardsAddis", 1, "traffic_towardsAddis", "sum")
traffic_towards_adama_daily <- daily_summary_subset(traffic_all, "towardsAddis", 0, "traffic_towardsAdama", "sum")

# Speed by Hour
speed_daily <- summaryBy(speed_km_hr ~ day, data=traffic_all, FUN = function(x) quantile(x, quantiles_use, na.rm=T))
names(speed_daily) <- c("day", paste0("speed_perc_",quantiles_use))

speed_towards_addis_daily <- summaryBy(speed_km_hr ~ day, data=traffic_all[traffic_all$towardsAddis == TRUE,], FUN = function(x) quantile(x, quantiles_use, na.rm=T))
names(speed_towards_addis_daily) <- c("day", paste0("speed_towards_addis_perc_",quantiles_use))

speed_towards_adama_daily <- summaryBy(speed_km_hr ~ day, data=traffic_all[traffic_all$towardsAddis == FALSE,], FUN = function(x) quantile(x, quantiles_use, na.rm=T))
names(speed_towards_adama_daily) <- c("day", paste0("speed_towards_adama_perc_",quantiles_use))

# Accidents by Hour 
accidents_all$accidents_N <- 1
accident_count_daily <- summaryBy(accidents_N ~ day, data=accidents_all, FUN=sum, keep.names=T)
accident_count_daily <- accident_count_daily[!is.na(accident_count_daily$accidents_N),]

accident_towardsAddis_daily <- daily_summary_subset(accidents_all, "Direction","Addis", "accident_towardsAddis", "sum")
accident_towardsAdama_daily <- daily_summary_subset(accidents_all, "Direction","Adama", "accident_towardsAdama", "sum")

# Merge 
dataset_days_wide <- merge(traffic_county_daily, accident_count_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, traffic_towards_addis_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, traffic_towards_adama_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, speed_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, speed_towards_addis_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, speed_towards_adama_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, accident_towardsAddis_daily, by="day",all=T)
dataset_days_wide <- merge(dataset_days_wide, accident_towardsAdama_daily, by="day",all=T)

dataset_days_wide <- dataset_days_wide[!is.na(dataset_days_wide$day),]

# Make Stacked Version of Datasets ---------------------------------------------
# Hourly
dataset_hours_long <- rbind(
  cbind(dataset_hours_wide$dayhour,
        dataset_hours_wide$traffic_N,
        dataset_hours_wide$speed_perc_0.1,
        dataset_hours_wide$speed_perc_0.25,
        dataset_hours_wide$speed_perc_0.5,
        dataset_hours_wide$speed_perc_0.75,
        dataset_hours_wide$speed_perc_0.9,
        dataset_hours_wide$accidents_N,
        rep("Total", nrow(dataset_hours_wide))),
  cbind(dataset_hours_wide$dayhour,
        dataset_hours_wide$traffic_towardsAddis,
        dataset_hours_wide$speed_towards_addis_perc_0.1,
        dataset_hours_wide$speed_towards_addis_perc_0.25,
        dataset_hours_wide$speed_towards_addis_perc_0.5,
        dataset_hours_wide$speed_towards_addis_perc_0.75,
        dataset_hours_wide$speed_towards_addis_perc_0.9,
        dataset_hours_wide$accident_towardsAddis,
        rep("To Addis", nrow(dataset_hours_wide))),
  cbind(dataset_hours_wide$dayhour,
        dataset_hours_wide$traffic_towardsAdama,
        dataset_hours_wide$speed_towards_adama_perc_0.1,
        dataset_hours_wide$speed_towards_adama_perc_0.25,
        dataset_hours_wide$speed_towards_adama_perc_0.5,
        dataset_hours_wide$speed_towards_adama_perc_0.75,
        dataset_hours_wide$speed_towards_adama_perc_0.9,
        dataset_hours_wide$accident_towardsAdama,
        rep("To Adama", nrow(dataset_hours_wide)))) 

dataset_hours_long <- as.data.frame(dataset_hours_long)
names(dataset_hours_long) <- c("dayhour","traffic",paste0("speed_perc_",quantiles_use), "accidents","direction")

dataset_hours_long$speed_perc_0.1 <- dataset_hours_long$speed_perc_0.1 %>% as.character %>% as.numeric
dataset_hours_long$speed_perc_0.25 <- dataset_hours_long$speed_perc_0.25 %>% as.character %>% as.numeric
dataset_hours_long$speed_perc_0.5 <- dataset_hours_long$speed_perc_0.5 %>% as.character %>% as.numeric
dataset_hours_long$speed_perc_0.75 <- dataset_hours_long$speed_perc_0.75 %>% as.character %>% as.numeric
dataset_hours_long$speed_perc_0.9 <- dataset_hours_long$speed_perc_0.9 %>% as.character %>% as.numeric

# Daily
dataset_days_long <- rbind(
  cbind(dataset_days_wide$day,
        dataset_days_wide$traffic_N,
        dataset_days_wide$speed_perc_0.1,
        dataset_days_wide$speed_perc_0.25,
        dataset_days_wide$speed_perc_0.5,
        dataset_days_wide$speed_perc_0.75,
        dataset_days_wide$speed_perc_0.9,
        dataset_days_wide$accidents_N,
        rep("Total", nrow(dataset_days_wide))),
  cbind(dataset_days_wide$day,
        dataset_days_wide$traffic_towardsAddis,
        dataset_days_wide$speed_towards_addis_perc_0.1,
        dataset_days_wide$speed_towards_addis_perc_0.25,
        dataset_days_wide$speed_towards_addis_perc_0.5,
        dataset_days_wide$speed_towards_addis_perc_0.75,
        dataset_days_wide$speed_towards_addis_perc_0.9,
        dataset_days_wide$accident_towardsAddis,
        rep("To Addis", nrow(dataset_days_wide))),
  cbind(dataset_days_wide$day,
        dataset_days_wide$traffic_towardsAdama,
        dataset_days_wide$speed_towards_adama_perc_0.1,
        dataset_days_wide$speed_towards_adama_perc_0.25,
        dataset_days_wide$speed_towards_adama_perc_0.5,
        dataset_days_wide$speed_towards_adama_perc_0.75,
        dataset_days_wide$speed_towards_adama_perc_0.9,
        dataset_days_wide$accident_towardsAdama,
        rep("To Adama", nrow(dataset_days_wide)))) 

dataset_days_long <- as.data.frame(dataset_days_long)
names(dataset_days_long) <- c("day","traffic",paste0("speed_perc_",quantiles_use), "accidents","direction")

dataset_days_long$speed_perc_0.1 <- dataset_days_long$speed_perc_0.1 %>% as.character %>% as.numeric
dataset_days_long$speed_perc_0.25 <- dataset_days_long$speed_perc_0.25 %>% as.character %>% as.numeric
dataset_days_long$speed_perc_0.5 <- dataset_days_long$speed_perc_0.5 %>% as.character %>% as.numeric
dataset_days_long$speed_perc_0.75 <- dataset_days_long$speed_perc_0.75 %>% as.character %>% as.numeric
dataset_days_long$speed_perc_0.9 <- dataset_days_long$speed_perc_0.9 %>% as.character %>% as.numeric

# Generate Variables -----------------------------------------------------------
###### Daily 
# Days
dataset_days_long$day <- dataset_days_long$day %>% as.character %>% as.Date
dataset_days_long$day_of_week <- weekdays(as.Date(dataset_days_long$day))

# Accident/Traffic
dataset_days_long$traffic <- dataset_days_long$traffic %>% as.character %>% as.numeric
dataset_days_long$accidents <- dataset_days_long$accidents %>% as.character %>% as.numeric
dataset_days_long$accidents[is.na(dataset_days_long$accidents)] <- 0
dataset_days_long$accident_yn <- (dataset_days_long$accidents > 0) %>% as.numeric

##### Hourly
# Days/Hours
dataset_hours_long$dayhour <- dataset_hours_long$dayhour %>% as.character
dataset_hours_long$hour <- substring(dataset_hours_long$dayhour, 12,13) %>% as.numeric
dataset_hours_long$day <- substring(dataset_hours_long$dayhour, 1,10) %>% as.character %>% as.Date
dataset_hours_long$day_of_week <- weekdays(as.Date(dataset_hours_long$day))

dataset_hours_long$hour_chunks <- NA
dataset_hours_long$hour_chunks[dataset_hours_long$hour %in% 0:3] <- 1
dataset_hours_long$hour_chunks[dataset_hours_long$hour %in% 4:7] <- 2
dataset_hours_long$hour_chunks[dataset_hours_long$hour %in% 8:11] <- 3
dataset_hours_long$hour_chunks[dataset_hours_long$hour %in% 12:15] <- 4
dataset_hours_long$hour_chunks[dataset_hours_long$hour %in% 16:19] <- 5
dataset_hours_long$hour_chunks[dataset_hours_long$hour %in% 20:23] <- 6

dataset_hours_long$hour_chunks <- factor(dataset_hours_long$hour_chunks,
                                         levels = c(1,2,3,4,5,6),
                                         labels = c("Midnight - 3am", "4am - 7am", "8am - 11am", "Noon - 3pm", "4pm - 7pm", "8pm - 11pm"))

# Accident/Traffic
dataset_hours_long$traffic <- dataset_hours_long$traffic %>% as.character %>% as.numeric
dataset_hours_long$accidents <- dataset_hours_long$accidents %>% as.character %>% as.numeric
dataset_hours_long$accidents[is.na(dataset_hours_long$accidents)] <- 0
dataset_hours_long$accident_yn <- (dataset_hours_long$accidents > 0) %>% as.numeric

# Merge in Precipitation -------------------------------------------------------
dataset_days_long <- merge(dataset_days_long, precipitation, all.x=T,all.y=F,by="day")
dataset_hours_long <- merge(dataset_hours_long, precipitation, all.x=T,all.y=F,by="day")

# Holidays ---------------------------------------------------------------------
# Public Holidays From:
# https://www.timeanddate.com/holidays/ethiopia/2015
# https://www.timeanddate.com/holidays/ethiopia/2016
# https://www.timeanddate.com/holidays/ethiopia/2017

holidays <- c("2015-01-07",
              "2015-01-19",
              "2015-03-02",
              "2015-04-10",
              "2015-04-12",
              "2015-05-01",
              "2015-05-05",
              "2015-05-28",
              "2015-07-17",
              "2015-09-12",
              "2015-09-23",
              "2015-09-28",
              "2015-12-23",
              
              "2016-01-07",
              "2016-01-19",
              "2016-03-02",
              "2016-04-29",
              "2016-05-01",
              "2016-05-05",
              "2016-05-28",
              "2016-07-07",
              "2016-09-11",
              "2016-09-13",
              "2016-09-27",
              "2016-12-12",
              
              "2017-01-07",
              "2017-01-19",
              "2017-03-02",
              "2017-04-14",
              "2017-04-16",
              "2017-05-01",
              "2017-05-05",
              "2017-05-28",
              "2017-06-26",
              "2017-09-02",
              "2017-09-11",
              "2017-09-27",
              "2017-12-01"
              )

holidays <- as.Date(holidays)
holidays_1day_buff <- c(holidays, (holidays-1), (holidays+1))

# Holiday Variable in Datasets
dataset_days_long$holiday <- (dataset_days_long$day %in% holidays) %>% as.numeric
dataset_days_long$holidays_1day_buff <- (dataset_days_long$day %in% holidays_1day_buff) %>% as.numeric

dataset_hours_long$holiday <- (dataset_hours_long$day %in% holidays) %>% as.numeric
dataset_hours_long$holidays_1day_buff <- (dataset_hours_long$day %in% holidays_1day_buff) %>% as.numeric

# Moving Window Averages -------------------------------------------------------
moving_avg_accident_traffic <- function(i, dataset_days_long, time_var, window, type){
  dataset_days_long_i <- dataset_days_long[i,]

  dataset_days_long_window <- dataset_days_long[dataset_days_long$direction %in% dataset_days_long_i$direction,]
  dataset_days_long_window <- dataset_days_long_window[dataset_days_long_window[[time_var]] %in% (dataset_days_long_i[[time_var]]-window):(dataset_days_long_i[[time_var]]+window),]
  
  df_out <- cbind(mean(dataset_days_long_window$accidents),
                  mean(dataset_days_long_window$speed_perc_0.1),
                  mean(dataset_days_long_window$speed_perc_0.25),
                  mean(dataset_days_long_window$speed_perc_0.5),
                  mean(dataset_days_long_window$speed_perc_0.75),
                  mean(dataset_days_long_window$speed_perc_0.9),
                  mean(dataset_days_long_window$traffic)) %>% 
            as.data.frame
  
  if(type == "days") MA_window <- window*2+1
  if(type == "hours") MA_window <- window/(60*60)*2+1 
  
  names(df_out) <- c(paste0("MA_",MA_window,"_day_accidents"),
                     paste0("MA_",MA_window,"_speed_perc_0.1"),
                     paste0("MA_",MA_window,"_speed_perc_0.25"),
                     paste0("MA_",MA_window,"_speed_perc_0.5"),
                     paste0("MA_",MA_window,"_speed_perc_0.75"),
                     paste0("MA_",MA_window,"_speed_perc_0.9"),
                     paste0("MA_",MA_window,"_day_traffic"))
  return(df_out)
}

dataset_days_long <- dataset_days_long[!is.na(dataset_days_long$day),]
MA_daily_df <- lapply(1:nrow(dataset_days_long), moving_avg_accident_traffic, dataset_days_long, "day", 10, "days") %>% bind_rows
dataset_days_long <- cbind(dataset_days_long, MA_daily_df)

dataset_hours_long <- dataset_hours_long[!is.na(dataset_hours_long$dayhour),]
dataset_hours_long$dayhour_posix <- paste0(dataset_hours_long$dayhour, ":00:00") %>% as.POSIXct
MA_hourly_df <- lapply(1:nrow(dataset_hours_long), moving_avg_accident_traffic, dataset_hours_long, "dayhour_posix", 1*60*60, "hours") %>% bind_rows
dataset_days_long <- cbind(dataset_days_long, MA_daily_df)

# Export -----------------------------------------------------------------------
save(dataset_hours_wide, file = file.path(intermediate_data_file_path, "traffic_accidents_hourly_wide.Rda"))
save(dataset_days_wide, file = file.path(intermediate_data_file_path, "traffic_accidents_daily_wide.Rda"))

save(dataset_hours_long, file = file.path(intermediate_data_file_path, "traffic_accidents_hourly_long.Rda"))
save(dataset_days_long, file = file.path(intermediate_data_file_path, "traffic_accidents_daily_long.Rda"))


