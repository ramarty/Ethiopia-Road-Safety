# Ethiopia Road Safety:  Daily Precipitation

# https://www.esrl.noaa.gov/psd/data/gridded/data.cpc.globalprecip.html
# units: mm - milimeter

# Run master to setup ----------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

file_path_precipitation <- file.path(raw_data_file_path,"precipitation")

# Functions to Extract Data ----------------------------------------------------
daily_precip <- function(year){
  
  ### Load file
  precip.yr <- nc_open(file.path(file_path_precipitation, paste0("precip.",year,".nc")))
  
  ### Get Coordinates
  precip.yr.lon <- ncvar_get(precip.yr,"lon")
  precip.yr.lat <- ncvar_get(precip.yr,"lat")
  nlon.yr <- dim(precip.yr.lon)
  nlat.yr <- dim(precip.yr.lat)
  
  ### Get time and convert to interpretable thing
  time.yr <- ncvar_get(precip.yr,"time")
  n.time.yr <- dim(time.yr)
  (tunits.yr <- ncatt_get(precip.yr,"time","units"))
  time.yr.yyyy.mm.dd <- as.POSIXct(time.yr*3600,origin='1900-01-01 12:00:00')
  time.yr.yyyy.mm.dd <- gsub("-", ".", time.yr.yyyy.mm.dd)
  time.yr.yyyy.mm.dd <- paste("t.", time.yr.yyyy.mm.dd, sep="")
  
  ### Get Data
  precip.yr_array <- ncvar_get(precip.yr,"precip")
  
  ### Convert to Dataframe
  # Data
  precip.yr_long <- as.vector(precip.yr_array)
  precip.yr_mat <- matrix(precip.yr_long, nrow=nlon.yr*nlat.yr, ncol=n.time.yr)
  
  # Coordinates
  lonlat.yr <- as.matrix(expand.grid(precip.yr.lon,precip.yr.lat))
  
  # Merge together
  precip.yr.df <- data.frame(cbind(lonlat.yr,precip.yr_mat))
  names(precip.yr.df) <- c("lon","lat",substring(time.yr.yyyy.mm.dd,1,12))
  
  # Closest to Addis Ababa
  #precip.yr.df[which.min(sqrt((precip.yr.df$lon - 38.74)^2 + (precip.yr.df$lat - 9.03)^2)),]
  
  # Stack and Select Lat/Lon
  grab_one_day <- function(day, precip.yr.df,lat,lon){
    precip.yr.df <- select(precip.yr.df, c("lon","lat",day))
    names(precip.yr.df) <- c("lon","lat","precipitation")
    precip.yr.df$day <- day
    
    precip.yr.df <- precip.yr.df[precip.yr.df$lon == lon & precip.yr.df$lat == lat,]
    
    return(precip.yr.df)
  }
  
  df_out <- lapply(names(precip.yr.df)[!(names(precip.yr.df) %in% c("lon","lat"))], grab_one_day, 
              precip.yr.df, 9.25, 38.75) %>% bind_rows
  
  return(df_out)
}

precip_2015 <- daily_precip(2015)
precip_2016 <- daily_precip(2016)
precip_2017 <- daily_precip(2017)

precipitation <- rbind(precip_2015, precip_2016, precip_2017)
year <- substring(precipitation$day,3,6)
month <- substring(precipitation$day,8,9)
day <- substring(precipitation$day,11,12)

precipitation$day <- paste0(year,"-",month,"-",day)
precipitation <- subset(precipitation, select=c(precipitation,day))

precipitation$day <- precipitation$day %>% as.character %>% as.Date

# Export -----------------------------------------------------------------------
save(precipitation, file = file.path(intermediate_data_file_path, "precipitation.Rda"))
