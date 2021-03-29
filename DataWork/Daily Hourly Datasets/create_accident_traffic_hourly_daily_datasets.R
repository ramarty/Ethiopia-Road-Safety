# Ethiopia Road Safety: Merge Accident and Traffic Data into Daily and Hourly Datasets

# Load Data --------------------------------------------------------------------
crashes_df <- readRDS(file.path(etre_crashes_dir, "FinalData", "crashes.Rds"))
traffic_df <- readRDS(file.path(etre_traffic_dir, "FinalData", "traffic_limitedvars.Rds"))
precip_df  <- readRDS(file.path(precip_dir, "FinalData", "precipitation.Rds"))
holidays_df  <- read.csv(file.path(holidays_dir, "RawData", "eth_holidays.csv"))

# Prep Datasets ----------------------------------------------------------------
traffic_df <- traffic_df %>%
  dplyr::select(trans_occur_time,
                speed_km_hr,
                direction) %>%
  filter(!is.na(trans_occur_time),
         trans_occur_time >= as.Date("2015-01-01"),
         !is.na(direction)) %>%
  mutate(date     = trans_occur_time %>% round_date(unit = "day"),
         date_hour = trans_occur_time %>% round_date(unit = "hour"),
         one = 1)

crashes_df <- crashes_df %>%
  filter(!is.na(direction)) %>%
  mutate(date_hour = accident_datetime %>% round_date(unit = "hour")) %>%
  dplyr::rename(date = accident_date)

precip_df <- precip_df %>%
  group_by(date) %>%
  dplyr::summarise(precip_mm = mean(precip_mm, na.rm = T))

holidays_df <- holidays_df %>%
  mutate(date = date %>% mdy,
         holiday = 1) %>%
  dplyr::select(date, holiday)

# Collapse Data ----------------------------------------------------------------
#### Traffic
traffic_hourly_df <- traffic_df[, list(N_vehicles = sum(one),
                                       speed_mean = mean(speed_km_hr, na.rm = T), 
                                       speed_p10 = as.numeric(quantile(speed_km_hr, probs = 0.1, na.rm = T)),
                                       speed_p25 = as.numeric(quantile(speed_km_hr, probs = 0.25, na.rm = T)),
                                       speed_p50 = as.numeric(quantile(speed_km_hr, probs = 0.5, na.rm = T)),
                                       speed_p75 = as.numeric(quantile(speed_km_hr, probs = 0.75, na.rm = T)),
                                       speed_p90 = as.numeric(quantile(speed_km_hr, probs = 0.9, na.rm = T))), 
                                by = list(date_hour, direction)]  %>%
  as.data.frame()

traffic_daily_df <- traffic_df[, list(N_vehicles = sum(one),
                                      speed_mean = mean(speed_km_hr, na.rm = T), 
                                      speed_p10 = as.numeric(quantile(speed_km_hr, probs = 0.1, na.rm = T)),
                                      speed_p25 = as.numeric(quantile(speed_km_hr, probs = 0.25, na.rm = T)),
                                      speed_p50 = as.numeric(quantile(speed_km_hr, probs = 0.5, na.rm = T)),
                                      speed_p75 = as.numeric(quantile(speed_km_hr, probs = 0.75, na.rm = T)),
                                      speed_p90 = as.numeric(quantile(speed_km_hr, probs = 0.9, na.rm = T))), 
                               by = list(date, direction)] %>%
  as.data.frame()

#### Crashes
crashes_hourly_df <- crashes_df %>%
  filter(!is.na(date_hour)) %>%
  group_by(date_hour, direction) %>%
  dplyr::summarise(N_crashes = n())

crashes_daily_df <- crashes_df %>%
  filter(!is.na(date)) %>%
  group_by(date, direction) %>%
  dplyr::summarise(N_crashes = n())

#### Merge
hourly_df <- merge(crashes_hourly_df, traffic_hourly_df, by = c("date_hour", "direction"), all = T) %>%
  complete(date_hour = seq(min(date_hour), max(date_hour), by = "hour"),
           direction) %>%
  mutate(date = date_hour %>% round_date(unit = "day") %>% as.Date)

daily_df <- merge(crashes_daily_df,   traffic_daily_df,  by = c("date", "direction"), all = T) %>%
  complete(date = seq.Date(min(date), max(date), by = "day"),
           direction)

# Merge in Precipitation -------------------------------------------------------
daily_df  <- merge(daily_df,  precip_df, all.x=T, all.y=F, by="date")
hourly_df <- merge(hourly_df, precip_df, all.x=T, all.y=F, by="date")

# Merge in Holidays ------------------------------------------------------------
## Holiday Date
daily_df  <- merge(daily_df,  holidays_df, all.x=T, all.y=F, by="date")
hourly_df <- merge(hourly_df, holidays_df, all.x=T, all.y=F, by="date")

## Holiday, +/- 1 day
holidays_df_pm1day <- bind_rows(holidays_df,
                         holidays_df %>%
                           mutate(date = date - 1),
                         holidays_df %>%
                           mutate(date = date + 1)) %>%
  distinct() %>%
  dplyr::rename(holiday_plusminus_1day = holiday)

daily_df  <- merge(daily_df,  holidays_df_pm1day, all.x=T, all.y=F, by="date")
hourly_df <- merge(hourly_df, holidays_df_pm1day, all.x=T, all.y=F, by="date")

## Holiday, +/- 2 day2
holidays_df_pm2day <- bind_rows(holidays_df,
                                holidays_df %>%
                                  mutate(date = date - 1),
                                holidays_df %>%
                                  mutate(date = date + 1),
                                holidays_df %>%
                                  mutate(date = date - 2),
                                holidays_df %>%
                                  mutate(date = date + 2)) %>%
  distinct() %>%
  dplyr::rename(holiday_plusminus_2day = holiday)

daily_df  <- merge(daily_df,  holidays_df_pm2day, all.x=T, all.y=F, by="date")
hourly_df <- merge(hourly_df, holidays_df_pm2day, all.x=T, all.y=F, by="date")

# Cleanup ----------------------------------------------------------------------
# TODO: N_vehicles and speed replace_na for hourly
daily_df <- daily_df %>%
  dplyr::mutate(holiday   = holiday %>% replace_na(0),
                N_crashes = N_crashes %>% replace_na(0),
                crash     = as.numeric(N_crashes >= 1))

hourly_df <- hourly_df %>%
  dplyr::mutate(holiday   = holiday %>% replace_na(0),
                N_crashes = N_crashes %>% replace_na(0),
                crash     = as.numeric(N_crashes >= 1))

# When don't have crash dataset, make NA
max_crash_date <- crashes_df$date %>% max(na.rm=T)

daily_df$crash[daily_df$date > max_crash_date] <- 0
daily_df$N_crashes[daily_df$date > max_crash_date] <- 0

hourly_df$crash[hourly_df$date > max_crash_date] <- 0
hourly_df$N_crashes[hourly_df$date > max_crash_date] <- 0

# When don't have traffic data, remove
min_traffic_date <- traffic_df$date %>% min(na.rm = T)

daily_df <- daily_df[daily_df$date >= min_traffic_date,]
hourly_df <- hourly_df[hourly_df$date >= min_traffic_date,]

# Export -----------------------------------------------------------------------
saveRDS(daily_df,  file.path(dailyhourly_dir, "FinalData", "daily.Rds"))
saveRDS(hourly_df, file.path(dailyhourly_dir, "FinalData", "hourly.Rds"))

daily_df %>%
  ggplot() +
  geom_line(aes(x = date,
                y = N_vehicles))




