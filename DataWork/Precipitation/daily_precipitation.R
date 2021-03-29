# Create Daily Precipitation File

# Create dataset that shows daily precipitation around Addis-Adama expressway at 
# daily level since 2014

# Functions to Extract Data ----------------------------------------------------
extract_daily_precip <- function(year, file_path_precip){
  print(year)
  
  ### Load file
  precip_yr <- nc_open(file.path(file_path_precip, paste0("precip.",year,".nc")))
  
  ### Get Coordinates
  precip_yr_lon <- ncvar_get(precip_yr,"lon")
  precip_yr_lat <- ncvar_get(precip_yr,"lat")
  nlon_yr <- dim(precip_yr_lon)
  nlat_yr <- dim(precip_yr_lat)
  
  ### Get time and convert to "yyyy-mm-dd hh:mm:ss" format
  time_yr <- ncvar_get(precip_yr,"time")
  n_time_yr <- dim(time_yr)

  ### Get PrecipitationData
  precip_yr_vec <- ncvar_get(precip_yr, "precip") %>% as.vector()
  
  ### Convert to Dataframe
  # Data
  precip_yr_mat <- matrix(precip_yr_vec, nrow=nlon_yr*nlat_yr, ncol=n_time_yr) %>%
    as.data.frame()
  
  # Coordinates
  lonlat_yr <- expand.grid(precip_yr_lon, precip_yr_lat) %>% 
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::rename(lon = Var1,
                  lat = Var2)
  
  # Merge together
  precip_yr_df <- bind_cols(lonlat_yr, precip_yr_mat) 
  
  # Restrict to Study Area
  precip_yr_df <- precip_yr_df[precip_yr_df$lat %in% 9.25 & precip_yr_df$lon %in% 38.75,]

  # Pivot Longer
  precip_yr_df <- precip_yr_df %>%
    pivot_longer(cols = -c(lon, lat)) %>%
    dplyr::rename(datetime = name)
  
  precip_yr_df$datetime <- as.POSIXct(as.vector(time_yr)*3600, origin='1900-01-01 12:00:00')
  
  return(precip_yr_df)
}

# Process Data -----------------------------------------------------------------
file_path_precip <- file.path(precip_dir, "RawData")

precip <- map_df(2014:2019, extract_daily_precip, file_path_precip)

precip <- precip %>%
  mutate(date = datetime %>% as.Date) %>%
  dplyr::rename(precip_mm = value) %>%
  dplyr::select(-datetime)

# Export -----------------------------------------------------------------------
saveRDS(precip, file.path(precip_dir, "FinalData", "precipitation.Rds"))
write.csv(precip, file.path(precip_dir, "FinalData", "precipitation.csv"), row.names = F)




